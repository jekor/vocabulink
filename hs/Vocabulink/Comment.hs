module Vocabulink.Comment ( renderComments, storeComment
                          ) where

import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Html
import Vocabulink.Utils

import Prelude hiding (div, span, id)

data Comment = Comment { commentNo       :: Int32
                       , commentLevel    :: Int32
                       , commentUsername :: String
                       , commentEmail    :: String
                       , commentTime     :: UTCTime
                       , commentBody     :: String
                       }

-- Each comment uses (Pandoc-extended) Markdown syntax.

commentBox :: Comment -> Html
commentBox c = do
  div ! id (toValue $ "comment-" ++ show (commentNo c))
      ! class_ "comment"
      ! customAttribute "comment" (toValue $ commentNo c)
      ! style (toValue $ "margin-left: " ++ indent) $ do
    div ! class_ "metadata" $ do
      span ! class_ "username" $ toMarkup $ commentUsername c
      span ! class_ "timestamp" $ toMarkup $ prettyPrint (commentTime c)
    gravatar 48 $ commentEmail c
    button ! class_ "reply light" $ "Reply"
    div ! class_ "speech-bubble left body" $ fromRight "Failed to parse comment." (markdownToHtml (commentBody c))
 where indent = show (fromIntegral (commentLevel c) * (1.3 :: Double)) ++ "em"

-- Storing a comment establishes and returns its unique comment number.

storeComment :: E (Int32 -> String -> Maybe Int32 -> IO Int32)
storeComment memberNo body parent = do
  when (body == "") $ error "Empty comment body"
  fromJust <$> query ?db
 where query = case parent of
                 Nothing -> $(queryTuple "INSERT INTO comment (author, body) \
                                                      \VALUES ({memberNo}, {body}) \
                                         \RETURNING comment_no")
                 Just p' -> $(queryTuple "INSERT INTO comment (author, body, parent_no) \
                                                      \VALUES ({memberNo}, {body}, {p'}) \
                                         \RETURNING comment_no")

getComments :: E (Int32 -> IO [Comment])
getComments root = map commentFromValues <$> $(queryTuples "SELECT * FROM comment_tree({root})") ?db
 where commentFromValues (n, l, u, e, t, b) = Comment (fromJust n) (fromJust l) (fromJust u) (fromJust e) (fromJust t) (fromJust b)

-- This handles rendering a proper tree of comments as well as comments
-- branching from a fake root comment. We examine the level of the first
-- comment coming back from getComments to determine which we're dealing with.

renderComments :: E (Int32 -> IO Html)
renderComments root = do
  cs <- getComments root
  let cs'  = if length cs > 0 && commentLevel (head cs) == 0
               then cs -- true root
               else map (\c -> c {commentLevel = commentLevel c - 1}) cs -- pseudo root
  return $ div ! id (toValue $ "comments-" ++ show root) ! class_ "comments" $
    mconcat (map commentBox cs' ++ [invitationLink "Comment"])

-- It's common to add a little hyperlink teaser for actions that require
-- verification. For example "login to reply" or "verify email to reply".
-- Currently this is only used for comments.

invitationLink :: E (String -> Html)
invitationLink text =
  case ?member of
    Nothing -> a ! class_ "login-required" ! href "" $ toMarkup ("Login to " ++ text)
    Just m  -> case memberEmail m of
                 Nothing -> a ! class_ "verified" ! href ""
                              $ toMarkup ("Verify Email to " ++ text)
                 Just _  -> mempty
