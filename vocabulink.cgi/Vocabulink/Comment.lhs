% Copyright 2008, 2009 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\section{Comments}

> module Vocabulink.Comment (  commentBox, commentForm, displayCommentBody,
>                              storeComment, commentPreview,
>                              renderComment, renderComments,
>                              rootReplyForm, replyToComment, getComments,
>                              voteOnComment ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import Network.Gravatar (gravatarWith, size)
> import qualified Text.XHtml.Strict.Formlets as F

> data Comment = Comment {  commentNo        :: Integer,
>                           commentLevel     :: Integer,
>                           commentUsername  :: String,
>                           commentEmail     :: String,
>                           commentTime      :: UTCTime,
>                           commentBody      :: String }

The following HTML is pretty messy. I now know why threaded comments are rare:
they're difficult to implement! It's not just the database threading that's
difficult but the display as well.

> commentBox :: Monad m => XHtmlForm m a -> XHtmlForm m a
> commentBox = plug (\xhtml -> thediv ! [theclass "comment toplevel editable rounded"] <<
>                                xhtml)

Creating the form for a comment requires knowing the comment's parent (as a
comment number). If this comment is not a reply to another comment (in the case
of root comments), it sould be passed as Nothing.

The comment form displays the member's gravatar as a visual hint to what the
comment will eventually look like when posted.

> commentForm :: String -> Maybe String -> AppForm (String, Maybe Integer)
> commentForm email parent = plug (\xhtml -> concatHtml [
>   image ! [  width "60", height "60", theclass "avatar",
>              src $ gravatarWith  (map toLower email)
>                                  Nothing (size 60) (Just "wavatar") ],
>   thediv ! [theclass "speech soft"] << xhtml,
>   thediv ! [theclass "signature"] << [
>     button << "Preview" +++ stringToHtml " " +++ submit "" "Post Comment" ] ])
>     ((\a b -> (a, maybeRead b))  <$> (F.textarea Nothing Nothing Nothing `check` ensures
>                                         (nonEmptyAndLessThan 10000 "Comment"))
>           <*> F.hidden parent)

Each comment uses (Pandoc-extended) Markdown syntax.

> displayCommentBody :: String -> Html
> displayCommentBody = markdownToHtml

Storing a comment establishes and returns its unique comment number.

> storeComment :: Integer -> String -> Maybe Integer -> App (Maybe Integer)
> storeComment memberNo body parent =
>   quickInsertNo'  "INSERT INTO comment (author, body, parent_no) \
>                   \VALUES (?, ?, ?)" [toSql memberNo, toSql body, toSql parent]
>                   "comment_comment_no_seq"

> getComments :: Integer -> App (Maybe [Comment])
> getComments root = do
>   comments <- queryTuples' "SELECT * FROM comment_tree(?)" [toSql root]
>   case comments of
>     Nothing  -> error "Error retrieving comments."
>     Just cs  -> return $ Just $ mapMaybe commentFromValues cs

> commentFromValues :: [SqlValue] -> Maybe Comment
> commentFromValues [n, l, u, e, t, b]  =
>   Just Comment {  commentNo        = fromSql n,
>                   commentLevel     = fromSql l,
>                   commentUsername  = fromSql u,
>                   commentEmail     = fromSql e,
>                   commentTime      = fromSql t,
>                   commentBody      = fromSql b }
> commentFromValues _                   = Nothing

Previewing comments is a truely asynchronous process (the first one
implemented). It makes for complicated JavaScript but a smoother interface.

We need to make sure that this doesn't go out of sync with renderComment.

Also, does this lead to XSS vulnerabilities?

> commentPreview :: App CGIResult
> commentPreview = do
>   memberName <- asks appMemberName
>   case memberName of
>     Nothing  -> outputUnauthorized
>     Just _   -> do
>       comment <- getRequiredInput "comment"
>       outputJSON [  ("html", showHtmlFragment $ displayCommentBody comment),
>                     ("status", "OK") ]

Displaying forum comments is complicated by the fact that we insert hidden
comment forms along with each comment if the page is going to a confirmed
member. This allows instant dynamic interactivity. However, it comes at the
cost of inflated HTML pages.

There is a more efficient way to do this that involves more JavaScript, but for
now we keep it simple.

> renderComment :: Comment -> App Html
> renderComment c = do
>   memberName  <- asks appMemberName
>   email       <- asks appMemberEmail
>   reply <- case (memberName, email) of
>              (Just _, Just email')  -> do
>                (_, xhtml) <- runForm' $ commentForm email' (Just $ show $ commentNo c)
>                let id' = "reply-" ++ show (commentNo c)
>                return $ concatHtml [
>                  button ! [  theclass $ "reveal " ++ id' ] << "Reply",
>                  clear,
>                  thediv ! [  thestyle "display: none",
>                              identifier id',
>                              theclass "reply" ] << [
>                    thediv ! [theclass "comment editable"] <<
>                       form ! [method "POST"] << [xhtml] ] ]
>              (Just _, Nothing)      -> return $ anchor ! [href "/member/confirmation"] <<
>                                                   "Confirm Your Email to Reply"
>              _                      -> return $ anchor ! [href "/member/login"] <<
>                                                   "Login to Reply"
>   return $ thediv ! [  theclass "comment toplevel rounded",
>                        thestyle $ "margin-left:" ++ show (commentLevel c * 2) ++ "em" ] << [
>     paragraph ! [theclass "timestamp"] << formatSimpleTime (commentTime c),
>     image ! [  width "60", height "60", theclass "avatar",
>                src $  gravatarWith (map toLower $ commentEmail c)
>                                    Nothing (size 60) (Just "wavatar") ],
>     thediv ! [theclass "speech"] << displayCommentBody (commentBody c),
>     thediv ! [theclass "signature"] << ("—" ++ commentUsername c),
>     thediv ! [theclass "reply-options"] << reply ]

> renderComments :: Integer -> App Html
> renderComments root = do
>   comments <- getComments root
>   case comments of
>     Nothing  -> return $ paragraph << "Unable to retrieve comments."
>     Just cs  -> do
>       commentBoxes  <- mapM renderComment cs
>       rootReply     <- rootReplyForm root
>       return $ thediv ! [theclass "comments"] << (commentBoxes +++ rootReply)

> rootReplyForm :: Integer -> App Html
> rootReplyForm root = do
>   memberName  <- asks appMemberName
>   email       <- asks appMemberEmail
>   case (memberName, email) of
>     (Just _, Just email')  -> do
>       (_, xhtml) <- runForm' $ commentForm email' (Just $ show root)
>       return $ thediv ! [  theclass "comment toplevel rounded",
>                            thestyle "margin-left: 2em" ] << [
>                  thediv ! [theclass "reply-options"] << [
>                    thediv ! [theclass "reply"] << [
>                      thediv ! [theclass "comment editable"] <<
>                        form ! [method "POST"] << [xhtml] ] ] ]
>     (Just _, Nothing)      -> return $ anchor ! [  href "/member/confirmation",
>                                                    thestyle "margin-left: 2em" ] <<
>                                          "Confirm Your Email to Comment"
>     _                      -> return $ anchor ! [  href "/member/login",
>                                                    thestyle "margin-left: 2em" ] <<
>                                          "Login to Comment"

Replying to a comment is also a complex matter (are you noticing a trend
here?). The complexity is mainly because we need to update information in a
couple relations. Determining the number of comments or the time of the latest
comment in a thread is possible with SQL, but it can be expensive. So when
adding a comment to a thread we update the number of comments in the thread and
the last comment time.

If the comment is posted successfully, we need to remove the topic page from
the cache so that it gets regenerated on the next request.

> replyToComment :: App CGIResult
> replyToComment = do
>   memberName <- asks appMemberName
>   email <- asks appMemberEmail
>   case (memberName, email) of
>     (Just _, Just email')  -> do
>       res <- runForm (commentForm email' Nothing) $ Right noHtml
>       case res of
>         Left xhtml -> outputJSON [  ("html", showHtmlFragment $ thediv !
>                                                [theclass "comment editable"] << xhtml),
>                                     ("status", "incomplete") ]
>         Right (body,parent')  -> do
>           memberNo <- fromJust <$> asks appMemberNo
>           commentNo' <- storeComment memberNo body parent'
>           case commentNo' of
>             Nothing  -> error "Failed to store comment."
>             Just n   -> do
>               res' <- queryTuple'  "SELECT c.comment_no, 0 as level, \
>                                           \m.username, m.email, \
>                                           \c.time, c.body \
>                                    \FROM comment c, member m \
>                                    \WHERE m.member_no = c.author \
>                                      \AND c.comment_no = ?"
>                                    [toSql n]
>               case res' of
>                 Nothing  -> outputJSON [  ("html", "Error posting comment."),
>                                           ("status", "error") ]
>                 Just c'  -> do
>                   let c = fromJust $ commentFromValues c'
>                   comment <- renderComment c
>                   outputJSON [  ("html", showHtmlFragment comment),
>                                 ("status", "accepted") ]
>     _                  -> outputUnauthorized

> voteOnComment :: Integer -> App CGIResult
> voteOnComment n = withRequiredMemberNumber $ \memberNo -> do
>   vote <- getRequiredInput "vote"
>   res <- quickStmt'  "INSERT INTO comment_vote (comment, member, vote) \
>                                        \VALUES (?, ?, ?)"
>                      [toSql n, toSql memberNo, toSql vote]
>   case res of
>     Nothing  -> error "Failed to record vote."
>     _        -> output' ""
