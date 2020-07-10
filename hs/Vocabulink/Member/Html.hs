module Vocabulink.Member.Html ( gravatar, memberAvatar, memberByName
                              , memberPage, dashboardPage
                              ) where

import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, span, id)

gravatar :: Int -- ^ size (square) in pixels
         -> String -- ^ email address
         -> Html
gravatar size' email =
  img ! width (toValue size')
      ! height (toValue size')
      ! class_ "avatar"
      ! src (toValue $ "http://www.gravatar.com/avatar/" ++ gravatarHash email ++ "?s=" ++ show size' ++ "&d=wavatar&r=x")

memberAvatar :: Int -- ^ size (square) in pixels
             -> Member
             -> Maybe Html
memberAvatar size' member' =
  (a ! href (toValue $ "/user/" ++ memberName member')) . gravatar size' <$> memberEmail member'

-- This is here because we can't use db in Member.hs (cyclical imports).
memberByName :: E (String -> IO (Maybe Member))
memberByName name' = uncurryN Member <$$> $(queryTuple
  "SELECT member_no, username, email FROM member \
  \WHERE username = {name'}") ?db

memberPage :: E (Member -> IO Html)
memberPage m = do
  let isSelf = maybe False (== m) ?member
      avatar = fromMaybe mempty (memberAvatar 128 m)
  stories <- latestStories m
  studyStats' <- studyStats m
  stdPage (memberName m ++ "'s Page") [JS "member-page", CSS "member-page", CSS "link"] mempty $ do
    div ! id "member-details" $ do
      avatar
      span ! class_ "username" $ toMarkup $ memberName m
      when isSelf $ unordList [
        a ! href "http://gravatar.com" $ "Change Avatar",
        a ! id "change-email" ! href "" $ "Change Email Address",
        a ! id "change-password" ! href "" $ "Change Password",
        a ! id "delete-account" ! href "" $ "Delete Account" ]
    div ! id "study-stats" $ do
      h2 "Study Stats"
      studyStats'
    div ! id "latest-stories" $ do
      h2 $ toMarkup ("Latest Stories by " ++ memberName m)
      case stories of
        [] -> "no stories"
        _  -> unordList stories ! class_ "stories"

latestStories :: E (Member -> IO [Html])
latestStories m = map renderStory <$> $(queryTuples
  "SELECT story_no, link_no, story FROM linkword_story \
  \WHERE author = {memberNumber m} \
  \ORDER BY edited DESC LIMIT 10") ?db
 where renderStory (sn, ln, s) = a ! href (toValue $ "/link/" ++ show (ln :: Int32) ++ "#" ++ show (sn :: Int32))
                                   $ fromRight "Failed to parse story." (markdownToHtml s)

studyStats :: E (Member -> IO Html)
studyStats m = do
  numLinks <- fromJust . fromJust <$> $(queryTuple
    "SELECT COUNT(*) FROM link_to_review WHERE member_no = {memberNumber m}") ?db
  numReviews <- fromJust . fromJust <$> $(queryTuple
    "SELECT COUNT(*) FROM link_review WHERE member_no = {memberNumber m}") ?db
  return $ tableOfPairs [ ("# of links in review", prettyPrint (numLinks :: Int64))
                        , ("# of reviews", prettyPrint (numReviews :: Int64))
                        ]

dashboardPage :: E (IO Html)
dashboardPage = stdPage "Dashboard" [JS "dashboard", CSS "dashboard"] mempty $ do
                  mempty
