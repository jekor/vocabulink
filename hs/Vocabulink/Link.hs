-- Copyright 2008, 2009, 2010, 2011, 2012, 2013 Chris Forno

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

-- Links are the center of interest in our program. Most activities revolve
-- around them.

module Vocabulink.Link ( Link(..), linkDetails, linkTypeName, compactLinkJSON
                       , languagePairLinks, linksContaining
                       , pronounceable
                       , Story(..), addStory, getStory, editStory, linkStories
                       , linksTable, linkPage, linksPage
                       ) where

import Vocabulink.Comment
import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Html
import Vocabulink.Page
import Vocabulink.Utils

import Text.Blaze.Html5 (audio, source)
import Text.Blaze.Html5.Attributes (preload)

import System.IO.Unsafe (unsafePerformIO)

-- TODO: Get rid of these by just using Aeson directly.
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson.Generic
import qualified Data.Aeson.Types
import qualified Data.Text
import qualified Data.Vector

import Prelude hiding (span, id, div)

data Link = Link { linkNumber     :: Integer
                 , linkLearn      :: String
                 , linkKnown      :: String
                 , linkLearnLang  :: String
                 , linkKnownLang  :: String
                 , linkSoundalike :: Bool
                 , linkWord       :: Maybe String
                 }
  deriving Eq

linkTypeName :: Link -> String
linkTypeName link
  | isJust (linkWord link) = "linkword"
  | linkSoundalike link    = "soundalike"
  | otherwise              = "association"

pronounceable :: Link -> Bool
{-# NOINLINE pronounceable #-}
pronounceable link = unsafePerformIO $ isFileReadable $ pronunciationFile link "ogg"

pronunciationFile :: Link -> String -> FilePath
pronunciationFile link filetype = mainDir </> "audio" </> "pronunciation" </> (show $ linkNumber link) <.> filetype

-- Displaying Links

-- <h1 class="link linkword">
--     <span class="foreign" title="Esperanto">nur</span>
--     <span class="link" title="linkword">newer</span>
--     <span class="familiar" title="English">only</span>
-- </h1>

-- <h2 class="link soundalike">
--     <span class="foreign" title="Esperanto">lingvo</span>
--     <span class="link" title="soundalike"></span>
--     <span class="familiar" title="English">language</span>
-- </h2>

instance ToMarkup Link where
  toMarkup link = do
    let learnLanguage = fromMaybe "Unknown Language" $ lookup (linkLearnLang link) languages
        knownLanguage = fromMaybe "Unknown Language" $ lookup (linkKnownLang link) languages
    h1 ! class_ (toValue $ "link " ++ linkTypeName link) $ do
      span ! class_ "foreign" ! customAttribute "lang" (toValue $ linkLearnLang link) ! title (toValue learnLanguage) $ do
        toMarkup $ linkLearn link
        pronunciation
      span ! class_ "link" ! title (toValue $ linkTypeName link) $ linkExtra
      span ! class_ "familiar" ! customAttribute "lang" (toValue $ linkKnownLang link) ! title (toValue knownLanguage) $ toMarkup $ linkKnown link
   where linkExtra = maybe mempty toMarkup $ linkWord link
         pronunciation = if pronounceable link
                           then button ! id "pronounce" ! class_ "button light" $ do
                                  audio ! preload "auto" $ do
                                    source ! src (toValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (linkNumber link) ++ ".ogg")
                                    source ! src (toValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (linkNumber link) ++ ".mp3")
                                  sprite "icon" "audio"
                           else mempty

$(deriveToJSON (lowercase . drop 4) ''Link)

compactLinkJSON :: Link -> Value
compactLinkJSON link =
  -- I opted for an array representation because it's more compact. We could be
  -- sending a lot of these in a single response.
  -- We don't need to send the language information as the new review process only
  -- operates on 1 set of languages at a time.
  let e = linkExtra link
  in [aesonQQ| [ <| linkNumber link |>
               , <| linkLearn link |>
               , <| linkKnown link |>
               , <<e>>
               ] |]
 where linkExtra l
         | isJust (linkWord l) = [aesonQQ| {"linkword": <| fromJust (linkWord l) |>} |]
         | linkSoundalike l    = [aesonQQ| {"soundalike": <| True |>} |]
         | otherwise           = Null

linkDetails :: E (Integer -> IO (Maybe Link))
linkDetails linkNo =
  uncurryN Link <$$> $(queryTuple
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE link_no = {linkNo} AND NOT deleted") ?db

languagePairLinks :: E (String -> String -> IO [Link])
languagePairLinks learnLang knownLang =
  map (uncurryN Link) <$> $(queryTuples
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE learn_lang = {learnLang} AND known_lang = {knownLang} \
      \AND NOT deleted") ?db

linksContaining :: E (String -> IO [Link])
linksContaining q = do
  let fuzzy = "%" ++ q ++ "%"
  exacts <- map (uncurryN Link) <$> $(queryTuples
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE NOT deleted \
      \AND (learn ILIKE {q} OR known ILIKE {q} OR linkword ILIKE {q} \
        \OR unaccent(learn) ILIKE {q} OR unaccent(known) ILIKE {q} OR unaccent(linkword) ILIKE {q})") ?db
  closes <- map (uncurryN Link) <$> $(queryTuples
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE NOT deleted \
      \AND (learn ILIKE {fuzzy} OR known ILIKE {fuzzy} OR linkword ILIKE {fuzzy} \
        \OR unaccent(learn) ILIKE {fuzzy} OR unaccent(known) ILIKE {fuzzy} OR unaccent(linkword) ILIKE {fuzzy}) \
    \ORDER BY char_length(learn)") ?db
  return $ nub $ exacts ++ closes

data Story = Story { storyNumber :: Integer
                   , storyBody :: String
                   , storyAuthor :: Member
                   , storyEdited :: UTCTime
                   }

instance ToMarkup Story where
  toMarkup story =
    div ! class_ "linkword-story-container" $ do
      a ! id (toValue $ storyNumber story) $ mempty
      div ! class_ "linkword-story" $ do
        blockquote $ markdownToHtml (storyBody story)
        div ! class_ "signature" $ do
          fromJust $ memberAvatar 32 (storyAuthor story)
          div ! class_ "details" $ do
            a ! class_ "username" ! href (toValue $ "/user/" ++ memberName (storyAuthor story))
              $ toMarkup (memberName $ storyAuthor story)
            br
            span ! class_ "date" $ toMarkup $ prettyPrint $ storyEdited story

$(deriveToJSON (drop 5) ''Story)

addStory :: E (Integer -> String -> IO ())
addStory linkNo story = withVerifiedMember $ \ m -> do
  $(execute "INSERT INTO linkword_story (link_no, author, story) \
                                \VALUES ({linkNo}, {memberNumber m}, {story})") ?db

-- Return the unformatted body of the story.
-- TODO: Why isn't this returning a Story value?
getStory :: E (Integer -> IO (Maybe String))
getStory storyNo = $(queryTuple "SELECT story FROM linkword_story WHERE story_no = {storyNo}") ?db

-- This needs to be in a permissions system.
storyEditable :: E (Integer -> IO Bool)
storyEditable n =
  case ?member of
    Nothing -> return False
    Just m  -> do
      author <- $(queryTuple "SELECT author FROM linkword_story \
                             \WHERE story_no = {n}") ?db
      case author of
        Nothing -> return False
        Just a' -> return $ a' == memberNumber m || memberNumber m == 1

-- TODO: Throw an error when not authorized.
editStory :: E (Integer -> String -> IO ())
editStory n s = withVerifiedMember $ \m -> do
  $(execute "UPDATE linkword_story \
            \SET story = {s}, edited = NOW() \
            \WHERE story_no = {n} AND (author = {memberNumber m} OR {memberNumber m} = 1)") ?db

linkStories :: E (Integer -> IO [Story])
linkStories linkNo = map mkStory <$> $(queryTuples
    "SELECT story_no, story, edited, member_no, username, email \
    \FROM linkword_story s INNER JOIN member m ON (m.member_no = s.author) \
    \WHERE link_no = {linkNo} \
    \ORDER BY story_no ASC") ?db
 where mkStory (n, s, e, n', u, em) = Story n s (Member n' u em) e

linksTable :: [Link] -> Html
linksTable links = table ! class_ "links" $ do
  thead $ do
    tr $ do
      th "Foreign"
      th "Familiar"
      th "Linkword"
  tbody $ mconcat $ map linkRow links
 where linkRow link = let url = "/link/" ++ show (linkNumber link) in
         tr ! class_ (toValue $ "inline-link " ++ linkTypeName link) $ do
           td $ a ! href (toValue url) $ toMarkup $ linkLearn link
           td $ a ! href (toValue url) $ toMarkup $ linkKnown link
           td $ a ! href (toValue url) $ toMarkup $ fromMaybe "" $ linkWord link

-- Each link gets its own URI and page. Most of the extra code in the following is
-- for handling the display of link operations (``review'', ``delete'', etc.),
-- dealing with retrieval exceptions, etc.

-- For the link's owner, we'll send along the source of the link in a hidden
-- textarea for in-page editing.

linkPage :: E (Link -> IO Html)
linkPage link = do
  row <- $(queryTuple "SELECT root_comment \
                      \FROM link_comment \
                      \WHERE link_no = {linkNumber link}") ?db
  comments <- maybe (return mempty) renderComments row
  stories <- do ss <- linkStories $ linkNumber link
                return $ mconcat $ map toMarkup ss
  let learnLanguage = fromMaybe "Unknown Language" $ lookup (linkLearnLang link) languages
      knownLanguage = fromMaybe "Unknown Language" $ lookup (linkKnownLang link) languages
  return $ stdPage (linkLearn link ++ " → " ++ linkKnown link ++ " — " ++ learnLanguage ++ " to " ++ knownLanguage) [CSS "link", JS "link"] mempty $ do
    toMarkup link
    div ! id "linkword-stories" $ do
      div ! class_ "header" $ h2 "Linkword Stories:"
      stories
    div ! id "comments" $ do
      h3 "Comments"
      comments

linksPage :: E (String -> [Link] -> Html)
linksPage title' links =
  simplePage title' [JS "link", CSS "link", ReadyJS initJS] $ do
    linksTable links
 where initJS = unlines
                  ["var options = {};"
                  ,"var pageHash = /^#page(\\d+)$/.exec(window.location.hash);"
                  ,"if (pageHash) {"
                  ,"  options.startPage = parseInt(pageHash[1], 10);"
                  ,"}"
                  ,"$('table').longtable(options).bind('longtable.pageChange', function (_, n) {"
                  ,"  window.location.hash = 'page' + n;"
                  ,"});"
                  ]
