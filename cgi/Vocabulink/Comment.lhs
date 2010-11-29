% Copyright 2008, 2009, 2010 Chris Forno

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
> import Vocabulink.Html
> import Vocabulink.Utils

> import Network.Gravatar (gravatarWith, size)
> import System.IO (Handle)

> data Comment = Comment {  commentNo        :: Integer,
>                           commentLevel     :: Integer,
>                           commentUsername  :: String,
>                           commentEmail     :: String,
>                           commentTime      :: UTCTime,
>                           commentBody      :: String }

> commentBox :: Comment -> App Html
> commentBox c = do
>   reply <- loggedInVerifiedButton "Reply"
>   let indent = show (fromIntegral (commentLevel c) * (1.3 :: Double)) ++ "em"
>   return $ thediv ! [  identifier ("comment-" ++ show (commentNo c)),
>                        theclass "comment-box",
>                        thestyle ("margin-left: " ++ indent)] << [
>     image ! [  width "60", height "60", theclass "avatar",
>                src $ gravatarWith  (map toLower $ commentEmail c)
>                                    Nothing (size 60) (Just "wavatar") ],
>     paragraph ! [theclass "metadata"] << [
>       thespan ! [theclass "membername"] << commentUsername c,
>       thespan ! [theclass "timestamp"] << formatSimpleTime (commentTime c)],
>     thediv ! [theclass "body"] <<
>       thediv ! [theclass "comment htmlfrag"] << markdownToHtml (commentBody c),
>     reply ]

Each comment uses (Pandoc-extended) Markdown syntax.

Storing a comment establishes and returns its unique comment number.

TODO: Move back into the App monad once we can handle transactions in it.

> storeComment :: Handle -> Integer -> String -> Maybe Integer -> IO Integer
> storeComment h memberNo body parent =
>   case body of
>     "" -> error "Empty comment body"
>     _  -> case parent of
>             Nothing -> fromJust <$> $(queryTuple
>                          "INSERT INTO comment (author, body) \
>                                       \VALUES ({memberNo}, {body}) \
>                          \RETURNING comment_no") h
>             Just p  -> fromJust <$> $(queryTuple
>                          "INSERT INTO comment (author, body, parent_no) \
>                                       \VALUES ({memberNo}, {body}, {p}) \
>                          \RETURNING comment_no") h

> getComments :: Integer -> App [Comment]
> getComments root = map commentFromValues <$> $(queryTuples'
>   "SELECT * FROM comment_tree({root})")

> commentFromValues :: (Maybe Integer, Maybe Integer, Maybe String,
>                       Maybe String, Maybe UTCTime, Maybe String) -> Comment
> commentFromValues (n, l, u, e, t, b) =
>   Comment {  commentNo        = fromJust n,
>              commentLevel     = fromJust l,
>              commentUsername  = fromJust u,
>              commentEmail     = fromJust e,
>              commentTime      = fromJust t,
>              commentBody      = fromJust b }

This handles rendering a proper tree of comments as well as comments branching
from a fake root comment. We examine the level of the first comment coming back
from getComments to determine which we're dealing with.

> renderComments :: Integer -> App Html
> renderComments root = do
>   cs <- getComments root
>   let cs'  = if length cs > 0 && commentLevel (head cs) == 0
>                then cs -- true root
>                else map (\c -> c {commentLevel = commentLevel c - 1}) cs -- pseudo root
>   invitation <- invitationLink "Comment"
>   cs'' <- mapM commentBox cs'
>   return $ thediv ! [  identifier ("comments-" ++ show root),
>                        theclass "comments" ] << (cs'' +++ invitation)

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
>   h <- asks appDB
>   n <- liftIO $ storeComment h memberNo body (Just parent)
>   row <- commentFromValues' <$$> $(queryTuple'
>     "SELECT c.comment_no, 0 as level, m.username, m.email, \
>            \c.time, c.body \
>     \FROM comment c, member m \
>     \WHERE m.member_no = c.author \
>       \AND c.comment_no = {n}")
>   case row of
>     Nothing -> error "Error posting comment."
>     Just c  -> output' $ showHtmlFragment $ markdownToHtml (commentBody c)
>  where commentFromValues' (n, l, u, e, t, b) =
>          Comment {  commentNo        = n,
>                     commentLevel     = fromJust l,
>                     commentUsername  = u,
>                     commentEmail     = fromJust e,
>                     commentTime      = t,
>                     commentBody      = fromJust b }

TODO: Make sure that the vote is in the proper format.

> voteOnComment :: Integer -> App CGIResult
> voteOnComment n = withRequiredMemberNumber $ \memberNo -> do
>   vote <- getRequiredInput "vote"
>   $(execute' "INSERT INTO comment_vote (comment, member, upvote) \
>                                \VALUES ({n}, {memberNo}, {vote == \"up\"})")
>   output' ""
