-- Copyright 2011, 2012 Chris Forno

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

-- | Member Page

module Vocabulink.Member.Page (memberPage, dashboardPage) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, span, id)

memberPage :: String -> App CGIResult
memberPage username = do
  member <- memberByName username
  isSelf <- maybe False (\ m -> memberName m == username) <$> asks appMember
  case member of
    Nothing -> outputNotFound
    Just m  -> do
      let avatar = fromMaybe mempty (memberAvatar 60 m)
      stories <- latestStories m
      studyStats' <- studyStats m
      stdPage (memberName m ++ "'s Page") [CSS "member-page", CSS "link"] mempty $ do
        div ! id "avatar" $ do
          avatar
          span ! class_ "username" $ toHtml $ memberName m
          when isSelf $ do br
                           span $ do "Change your avatar at "
                                     a ! href "http://gravatar.com" $ "gravatar.com"
        multiColumn
          [div $ do
             h2 "Study Stats"
             studyStats',
             mempty]
        div $ do
          h2 $ toHtml ("Latest Stories by " ++ memberName m)
          case stories of
            [] -> "no stories"
            _  -> unordList stories ! class_ "stories"

latestStories :: Member -> App [Html]
latestStories m = map renderStory <$> $(queryTuples'
  "SELECT story_no, link_no, story FROM linkword_story \
  \WHERE author = {memberNumber m} \
  \ORDER BY edited DESC LIMIT 10")
 where renderStory (sn, ln, s) = a ! href (toValue $ "/link/" ++ show ln ++ "#" ++ show sn)
                                   $ markdownToHtml s

studyStats :: Member -> App Html
studyStats m = do
  numLinks <- fromJust . fromJust <$> $(queryTuple'
    "SELECT COUNT(*) FROM link_to_review WHERE member_no = {memberNumber m}")
  numReviews <- fromJust . fromJust <$> $(queryTuple'
    "SELECT COUNT(*) FROM link_review WHERE member_no = {memberNumber m}")
  return $ tableOfPairs [("# of links in review", prettyPrint (numLinks::Integer))
                        ,("# of reviews", prettyPrint (numReviews::Integer))
                        ]

dashboardPage :: App CGIResult
dashboardPage = withRequiredMember $ \_ -> do
  stdPage "Dashboard" [JS "dashboard", CSS "dashboard"] mempty $ do
    mempty
