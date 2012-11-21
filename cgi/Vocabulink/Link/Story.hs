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

-- | Linkword Stories

module Vocabulink.Link.Story ( addStory, linkWordStories, renderStory
                             , getStory, editStory
                             ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Utils

import Prelude hiding (div, id, span)

addStory :: Integer -- ^ link number
         -> String -- ^ story
         -> App ()
addStory n s = withRequiredMember $ \ m -> do
  $(execute' "INSERT INTO linkword_story (link_no, author, story) \
                                 \VALUES ({n}, {memberNumber m}, {s})")

-- | Fetch all of the stories available for a given link.
linkWordStories :: Integer -- ^ link number
                -> App [(Integer, String, Member, Day)] -- ^ list of (story number, story, author, edited) pairs
linkWordStories linkNo = map tuplify <$> $(queryTuples'
    "SELECT story_no, story, edited, member_no, username, email \
    \FROM linkword_story s INNER JOIN member m ON (m.member_no = s.author) \
    \WHERE link_no = {linkNo} \
    \ORDER BY story_no ASC")
 where tuplify (n, s, e, n', u, em) = (n, s, Member n' u em, e)

renderStory :: Integer -- ^ story number
            -> String -- ^ body
            -> Member -- ^ author
            -> Day -- ^ date edited
            -> Html -- ^ output block
renderStory n s member edited =
  div ! class_ "linkword-story-container" $ do
    a ! id (toValue n) $ mempty
    div ! class_ "linkword-story" $ do
      blockquote $ markdownToHtml s
      div ! class_ "signature" $ do
        fromJust $ memberAvatar 32 member
        div ! class_ "details" $ do
          a ! class_ "username" ! href (toValue $ "/user/" ++ memberName member)
            $ toHtml (memberName member)
          br
          span ! class_ "date" $ toHtml $ prettyPrint edited

getStory :: Integer -- ^ story number
         -> App (Maybe String) -- ^ unformatted body
getStory n = $(queryTuple' "SELECT story FROM linkword_story WHERE story_no = {n}")

-- This needs to be in a permissions system.
storyEditable :: Integer -> App Bool
storyEditable n = do
  member <- asks appMember
  case member of
    Nothing -> return False
    Just m  -> do
      author <- $(queryTuple' "SELECT author FROM linkword_story \
                              \WHERE story_no = {n}")
      case author of
        Nothing -> return False
        Just a' -> return $ a' == memberNumber m || memberNumber m == 1

editStory :: Integer -- ^ story number
          -> String -- ^ new unformatted body
          -> App CGIResult
editStory n s = do
  editable <- storyEditable n
  if editable
    then do $(execute' "UPDATE linkword_story \
                       \SET story = {s}, edited = NOW() \
                       \WHERE story_no = {n}")
            outputNothing
    else outputUnauthorized -- Might be 404, but not concerned with that now.
