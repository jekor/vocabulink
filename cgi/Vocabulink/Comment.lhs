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

> module Vocabulink.Comment (  renderComments, commentBox, getComments,
>                              storeComment, replyToComment, voteOnComment ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import Network.Gravatar (gravatarWith, size)

> data Comment = Comment {  commentNo        :: Integer,
>                           commentLevel     :: Integer,
>                           commentUsername  :: String,
>                           commentEmail     :: String,
>                           commentTime      :: UTCTime,
>                           commentBody      :: String }

> commentBox :: Comment -> Html
> commentBox c =
>   let indent = show (fromIntegral (commentLevel c) * (1.3 :: Double)) ++ "em" in
>   thediv ! [  identifier ("comment-" ++ show (commentNo c)),
>               theclass "comment-box",
>               thestyle ("margin-left: " ++ indent)] << [
>     image ! [  width "60", height "60", theclass "avatar",
>                src $ gravatarWith  (map toLower $ commentEmail c)
>                                    Nothing (size 60) (Just "wavatar") ],
>     paragraph ! [theclass "metadata"] << [
>       thespan ! [theclass "membername"] << commentUsername c,
>       thespan ! [theclass "timestamp"] << formatSimpleTime (commentTime c)],
>     thediv ! [theclass "body"] <<
>       thediv ! [theclass "comment htmlfrag"] << markdownToHtml (commentBody c) ]

Each comment uses (Pandoc-extended) Markdown syntax.

Storing a comment establishes and returns its unique comment number.

> storeComment :: Integer -> String -> Maybe Integer -> App (Maybe Integer)
> storeComment memberNo body parent =
>   case body of
>     ""  -> error "Empty comment body"
>     _   -> quickInsertNo'  "INSERT INTO comment (author, body, parent_no) \
>                            \VALUES (?, ?, ?)" [toSql memberNo, toSql body, toSql parent]
>                            "comment_comment_no_seq"

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

This handles rendering a proper tree of comments as well as comments branching
from a fake root comment. We examine the level of the first comment coming back
from getComments to determine which we're dealing with.

> renderComments :: Integer -> App Html
> renderComments root = do
>   comments <- getComments root
>   case comments of
>     Nothing  -> return $ paragraph << "Unable to retrieve comments."
>     Just cs  -> do
>       let cs' = if (length cs > 0 && commentLevel (head cs) == 0)
>                   then cs -- true root
>                   else map (\c -> c {commentLevel = commentLevel c - 1}) cs -- pseudo root
>       return $ thediv ! [  identifier ("comments-" ++ show root),
>                            theclass "comments"] << map commentBox cs'

Replying to a comment is also a complex matter (are you noticing a trend
here?). The complexity is mainly because we need to update information in a
couple relations. Determining the number of comments or the time of the latest
comment in a thread is possible with SQL, but it can be expensive. So when
adding a comment to a thread we update the number of comments in the thread and
the last comment time.

If the comment is posted successfully, we need to remove the topic page from
the cache so that it gets regenerated on the next request.

> replyToComment :: Integer -> App CGIResult
> replyToComment parent = withRequiredMemberNumber $ \memberNo -> do
>   body <- getBody
>   commentNo' <- storeComment memberNo body (Just parent)
>   case commentNo' of
>     Nothing  -> error "Failed to store comment."
>     Just n   -> do
>       res' <- queryTuple'  "SELECT c.comment_no, 0 as level, \
>                                   \m.username, m.email, \
>                                   \c.time, c.body \
>                            \FROM comment c, member m \
>                            \WHERE m.member_no = c.author \
>                              \AND c.comment_no = ?"
>                            [toSql n]
>       case res' of
>         Nothing  -> error "Error posting comment."
>         Just c'  -> do
>           let c = fromJust $ commentFromValues c'
>           output' $ showHtmlFragment $ markdownToHtml (commentBody c)

TODO: Make sure that the vote is in the proper format.

> voteOnComment :: Integer -> App CGIResult
> voteOnComment n = withRequiredMemberNumber $ \memberNo -> do
>   vote <- getRequiredInput "vote"
>   res <- quickStmt'  "INSERT INTO comment_vote (comment, member, vote) \
>                                        \VALUES (?, ?, ?)"
>                      [toSql n, toSql memberNo, toSql vote]
>   case res of
>     Nothing  -> error "Failed to record vote."
>     _        -> output' ""
