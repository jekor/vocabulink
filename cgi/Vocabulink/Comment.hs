-- Copyright 2008, 2009, 2010, 2011 Chris Forno

-- This file is part of Vocabulink.

-- Vocabulink is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- Vocabulink is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

module Vocabulink.Comment ( renderComments, storeComment, replyToComment, contactUs
                          ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Utils

import Prelude hiding (div, span, id)

data Comment = Comment { commentNo       :: Integer
                       , commentLevel    :: Integer
                       , commentUsername :: String
                       , commentEmail    :: String
                       , commentTime     :: UTCTime
                       , commentBody     :: String
                       }

-- Each comment uses (Pandoc-extended) Markdown syntax.

commentBox :: Comment -> App Html
commentBox c = do
  let indent = show (fromIntegral (commentLevel c) * (1.3 :: Double)) ++ "em"
  return $
    div ! id (stringValue $ "comment-" ++ show (commentNo c))
        ! class_ "comment"
        ! customAttribute "comment" (stringValue $ show $ commentNo c)
        ! style (stringValue $ "margin-left: " ++ indent) $ do
      div ! class_ "metadata" $ do
        span ! class_ "username" $ string $ commentUsername c
        span ! class_ "timestamp" $ string $ prettyPrint (commentTime c)
      gravatar 48 $ commentEmail c
      button ! class_ "reply light" $ "Reply"
      div ! class_ "speech-bubble left body" $ markdownToHtml (commentBody c)
      clear

-- Storing a comment establishes and returns its unique comment number.

-- TODO: Move back into the App monad once we can handle transactions in it.

storeComment :: Handle -> Integer -> String -> Maybe Integer -> IO Integer
storeComment h memberNo body parent =
  case body of
    "" -> error "Empty comment body"
    _  -> case parent of
            Nothing -> fromJust <$> $(queryTuple
                         "INSERT INTO comment (author, body) \
                                      \VALUES ({memberNo}, {body}) \
                         \RETURNING comment_no") h
            Just p' -> fromJust <$> $(queryTuple
                         "INSERT INTO comment (author, body, parent_no) \
                                      \VALUES ({memberNo}, {body}, {p'}) \
                         \RETURNING comment_no") h

getComments :: Integer -> App [Comment]
getComments root = map commentFromValues <$> $(queryTuples'
  "SELECT * FROM comment_tree({root})")

-- TODO: This could be replaced by a function like:
-- (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f) -> (a, b, c, d, e, f)
-- Which can be done with Template Haskell.
commentFromValues :: (Maybe Integer, Maybe Integer, Maybe String,
                      Maybe String, Maybe UTCTime, Maybe String) -> Comment
commentFromValues (n, l, u, e, t, b) =
  Comment { commentNo       = fromJust n
          , commentLevel    = fromJust l
          , commentUsername = fromJust u
          , commentEmail    = fromJust e
          , commentTime     = fromJust t
          , commentBody     = fromJust b
          }

-- This handles rendering a proper tree of comments as well as comments
-- branching from a fake root comment. We examine the level of the first
-- comment coming back from getComments to determine which we're dealing with.

renderComments :: Integer -> App Html
renderComments root = do
  cs <- getComments root
  let cs'  = if length cs > 0 && commentLevel (head cs) == 0
               then cs -- true root
               else map (\c -> c {commentLevel = commentLevel c - 1}) cs -- pseudo root
  invitation <- invitationLink "Comment"
  cs'' <- mapM commentBox cs'
  return $ div ! id (stringValue $ "comments-" ++ show root) ! class_ "comments" $
    mconcat (cs'' ++ [invitation])

-- Replying to a comment is also a complex matter (are you noticing a trend
-- here?). The complexity is mainly because we need to update information in a
-- couple relations. Determining the number of comments or the time of the
-- latest comment in a thread is possible with SQL, but it can be expensive. So
-- when adding a comment to a thread we update the number of comments in the
-- thread and the last comment time.

-- If the comment is posted successfully, we need to remove the topic page from
-- the cache so that it gets regenerated on the next request.

replyToComment :: Integer -> App CGIResult
replyToComment parent = withRequiredMember $ \m -> do
  body <- getRequiredInput "body"
  h <- asks appDB
  n <- liftIO $ storeComment h (memberNumber m) body (Just parent)
  row <- commentFromValues' <$$> $(queryTuple'
    "SELECT c.comment_no, 0 as level, m.username, m.email, \
           \c.time, c.body \
    \FROM comment c, member m \
    \WHERE m.member_no = c.author \
      \AND c.comment_no = {n}")
  case row of
    Nothing -> error "Error posting comment."
    Just _  -> redirect =<< referrerOrVocabulink
 where commentFromValues' (n, l, u, e, t, b) =
         Comment {  commentNo        = n,
                    commentLevel     = fromJust l,
                    commentUsername  = u,
                    commentEmail     = fromJust e,
                    commentTime      = t,
                    commentBody      = fromJust b }

-- It's common to add a little hyperlink teaser for actions that require
-- verification. For example "login to reply" or "verify email to reply".

invitationLink :: String -> App Html
invitationLink text = do
  member <- asks appMember
  case member of
    Nothing -> do
      return $ a ! class_ "login-required" $ string ("Login to " ++ text)
    Just m  -> case memberEmail m of
                 Nothing -> do
                   url <- reversibleRedirect "/member/confirmation"
                   return $ a ! class_ "invitation" ! href (stringValue url) $
                              string ("Verify Email to " ++ text)
                 Just _  -> return mempty

contactUs :: App CGIResult
contactUs = do
  message <- getRequiredInput "message"
  url <- getRequiredInput "url"
  member <- asks appMember
  email <- case member >>= memberEmail of
             Nothing -> getRequiredInput "email"
             Just e  -> return e
  supportAddress <- fromJust <$> getOption "supportaddress"
  -- TODO: Add a reply-to header.
  res' <- liftIO $ sendMail supportAddress "Contact!" $
            unlines [ "Email: " ++ email
                    , "URL: " ++ url
                    , "Message: " ++ message ]
  case res' of
    Nothing  -> error ("Error sending message. \
                       \Please contact " ++ supportAddress ++ " for support.")
    Just _   -> outputNothing
