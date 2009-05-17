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
>                              storeComment, commentPreview, displayComment,
>                              replyToComment ) where

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
> commentBox = plug (\xhtml -> thediv ! [theclass "comment toplevel editable"] <<
>                                xhtml)

Creating the form for a comment requires knowing the comment's parent (as a
comment number). If this comment is not a reply to another comment (in the case
of root comments), it sould be passed as Nothing.

The comment form displays the member's gravatar as a visual hint to what the
comment will eventually look like when posted.

> commentForm :: String -> Maybe String -> Maybe String ->
>                AppForm (String, Maybe Integer)
> commentForm _ email parent = plug (\xhtml -> concatHtml [
>   image ! [  width "60", height "60", theclass "avatar",
>              src $ gravatarWith (maybe "" (map toLower) email)
>                                 Nothing (size 60) (Just "wavatar") ],
>   thediv ! [theclass "speech soft"] << xhtml,
>   thediv ! [theclass "signature"] << [
>     helpButton  "http://daringfireball.net/projects/markdown/basics"
>                 (Just "Formatting Help"),
>     isJust parent  ?  button << "Preview" +++ stringToHtml " " +++
>                       submit "" "Send Reply"
>                    $  submit "" "Create" ] ])
>     ((\a b -> (a, maybeRead =<< b))  <$> (F.textarea Nothing `check` ensures
>                 [  ((> 0)       . length,  "Comment must not be empty."),
>                    ((<= 10000)  . length,  "Comment must be 10,000 characters \
>                                            \or shorter.") ])
>           <*> (nothingIfNull $ F.hidden parent))

Each comment uses (Pandoc-extended) Markdown syntax.

> displayCommentBody :: String -> Html
> displayCommentBody = markdownToHtml

Storing a comment establishes and returns its unique comment number.

> storeComment :: Integer -> String -> Maybe Integer -> App (Maybe Integer)
> storeComment memberNo body parent = do
>   c <- asks appDB
>   liftIO $ insertNo c  "INSERT INTO comment (author, comment, parent_no) \
>                        \VALUES (?, ?, ?)" [toSql memberNo, toSql body, toSql parent]
>                        "comment_comment_no_seq"

> getComments :: Integer -> App (Maybe [Comment])
> getComments root = do
>   comments <- queryTuples' "SELECT * FROM comment_tree(?)" [toSql root]
>   case comments of
>     Nothing  -> error "Error retrieving comments."
>     Just cs  -> return Nothing

> commentFromValues :: [SqlValue] -> Maybe Comment
> commentFromValues [n, l, u, e, t, b]  =
>   Just $ Comment {  commentNo        = fromSql n,
>                     commentLevel     = fromSql l,
>                     commentUsername  = fromSql u,
>                     commentEmail     = fromSql e,
>                     commentTime      = fromSql t,
>                     commentBody      = fromSql b }
> commentFromValues _                   = Nothing

-- > map $(fromValues Comment 6) <$> queryTuples' "SELECT * FROM comment_tree(?)" [root]

-- > fromValues :: ( -> a) -> Int -> [SqlValue] -> Maybe a
-- > fromValues f n l =
-- >   if length l == n
-- >     then Just $ foldl (\f' y -> f' $ fromSql y) f l
-- >     else Nothing

-- > (((((Comment $ fromSql n) $ fromSql l) $ fromSql u) $ fromSql e) $ fromSql t) $ fromSql b

Previewing comments is a truely asynchronous process (the first one
implemented). It makes for complicated JavaScript but a smoother interface.

We need to make sure that this doesn't go out of sync with displayComment.

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

> displayComment :: Integer -> [SqlValue] -> App Html
> displayComment i [n, l, u, e, t, c]  = do
>   memberName <- asks appMemberName
>   email <- asks appMemberEmail
>   let n'  :: Integer  = fromSql n
>       l'  :: Integer  = fromSql l
>       u'  :: String   = fromSql u
>       e'  :: String   = e == SqlNull ? "" $ fromSql e
>       t'  :: UTCTime  = fromSql t
>       c'  :: String   = fromSql c
>       id'             = "reply-" ++ (show n')
>   reply <- case (memberName, email) of
>              (Just mn, Just _)  -> do
>                (_, xhtml) <- runForm' $ commentForm mn email (Just $ show n')
>                return $ concatHtml [
>                  button ! [  theclass $ "reveal " ++ id' ] << "Reply",
>                  paragraph ! [thestyle "clear: both"] << noHtml,
>                  thediv ! [  thestyle "display: none",
>                              identifier id',
>                              theclass "reply" ] << [
>                    thediv ! [theclass "comment editable"] <<
>                       form ! [method "POST"] << [  hidden "topic" (show i),
>                                                    xhtml ] ] ]
>              (Just _, Nothing)  -> return $ anchor ! [href "/member/confirmation"] <<
>                                               "Confirm Your Email to Reply"
>              _                  -> return $ anchor ! [href "/member/login"] <<
>                                               "Login to Reply"
>   return $ thediv ! [  theclass "comment toplevel",
>                        thestyle $ "margin-left:" ++ (show $ l'*2) ++ "em" ] << [
>     paragraph ! [theclass "timestamp"] << formatSimpleTime t',
>     image ! [  width "60", height "60", theclass "avatar",
>                src $  gravatarWith (map toLower e')
>                                    Nothing (size 60) (Just "wavatar") ],
>     thediv ! [theclass "speech"] << displayCommentBody c',
>     thediv ! [theclass "signature"] << ("—" ++ u'),
>     thediv ! [theclass "reply-options"] << reply ]
> displayComment _ _             = return $ paragraph << "Error retrieving comment."

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
>   topicNum <- fromJust . maybeRead <$> getRequiredInput "topic"
>   case (memberName, email) of
>     (Just mn, Just _)  -> do
>       parent <- getInput "parent"
>       res <- runForm (commentForm mn email parent) $ Right noHtml
>       case res of
>         Left xhtml           -> outputJSON [  ("html", showHtmlFragment $ thediv !
>                                                          [theclass "comment editable"] << xhtml),
>                                               ("status", "incomplete") ]
>         Right (body,parent')  -> do
>           memberNo <- fromJust <$> asks appMemberNo
>           res' <- withTransaction' $ do
>             commentNo' <- storeComment memberNo body parent'
>             run'  "UPDATE forum_topic \
>                   \SET last_comment = ?, \
>                       \num_replies = num_replies + 1 \
>                   \WHERE topic_no = ?"
>                   [toSql commentNo', toSql topicNum]
>             res'' <- queryTuple'  "SELECT c.comment_no, 0 as level, \
>                                          \m.username, m.email, \
>                                          \c.time, c.comment \
>                                   \FROM comment c, member m \
>                                   \WHERE m.member_no = c.author \
>                                     \AND comment_no = ?"
>                                   [toSql commentNo']
>             liftIO . commit =<< asks appDB
>             return $ fromJust res''
>           case res' of
>             Nothing  -> outputJSON [  ("html", "Error posting comment."),
>                                       ("status", "error") ]
>             Just c   -> do
>               c' <- asks appDB
>               liftIO $ commit c'
>               liftIO memcacheFlush
>               comment <- displayComment topicNum c
>               outputJSON [  ("html", showHtmlFragment comment),
>                             ("status", "accepted") ]
>     _                  -> outputUnauthorized
