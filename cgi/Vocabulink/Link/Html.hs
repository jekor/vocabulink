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

module Vocabulink.Link.Html ( renderLink, linksTable, linkJSON, compactLinkJSON
                            , linkPage, linksPage
                            , wordCloud
                            ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Comment
import Vocabulink.Config
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Link.Pronunciation
import Vocabulink.Link.Story
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Control.Monad.State (State, runState, get, put)
import Data.Aeson.Types (object, Value(..))
import qualified Data.Aeson.Generic
import Data.List (find, genericLength)
import Data.Map (fromList)
import Data.Text (pack)
import qualified Data.Vector
import System.Random
import Text.Blaze.Html5 (audio, source)
import Text.Blaze.Html5.Attributes (preload)

import Prelude hiding (div, span, id, words)

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

renderLink :: Link -> Bool -> App Html
renderLink link pronounceable' = do
  learnLanguage <- fromMaybe "Unknown Language" <$> langName (learn_lang link)
  knownLanguage <- fromMaybe "Unknown Language" <$> langName (known_lang link)
  return $ h1 ! class_ (toValue $ "link " ++ linkTypeName link) $ do
    span ! class_ "foreign" ! customAttribute "lang" (toValue $ learn_lang link) ! title (toValue learnLanguage) $ do
      toHtml $ learn link
      pronunciation
    span ! class_ "link" ! title (toValue $ linkTypeName link) $
      renderLinkExtra link
    span ! class_ "familiar" ! customAttribute "lang" (toValue $ known_lang link) ! title (toValue knownLanguage) $ toHtml $ known link
 where renderLinkExtra :: Link -> Html
       renderLinkExtra link' = case linkword link' of
                                 Nothing -> mempty
                                 Just w  -> toHtml w
       pronunciation = if pronounceable'
                         then button ! id "pronounce" ! class_ "button light" $ do
                                audio ! preload "auto" $ do
                                  source ! src (toValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (link_no link) ++ ".ogg") $ mempty
                                  source ! src (toValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (link_no link) ++ ".mp3") $ mempty
                                sprite "icon" "audio"
                         else mempty

linksTable :: [Link] -> Html
linksTable links = table ! class_ "links" $ do
  thead $ do
    tr $ do
      th "Foreign"
      th "Familiar"
      th "Linkword"
  tbody $ mconcat $ map linkRow links
 where linkRow link = let url = "/link/" ++ show (link_no link) in
         tr ! class_ (toValue $ "inline-link " ++ linkTypeName link) $ do
           td $ a ! href (toValue url) $ toHtml $ learn link
           td $ a ! href (toValue url) $ toHtml $ known link
           td $ a ! href (toValue url) $ toHtml $ fromMaybe "" $ linkword link

linkJSON :: Integer -> App CGIResult
linkJSON linkNo = do
  db <- asks appDB
  link' <- liftIO $ linkDetails linkNo db
  case link' of
    Nothing -> outputNotFound
    Just link -> do
      stories <- map storyJSON <$$> linkWordStories $ link_no link
      outputJSON $ fromList [("linkNumber"::String, toJSON $ link_no link)
                            ,("learn", toJSON $ learn link)
                            ,("known", toJSON $ known link)
                            ,("learnLang", toJSON $ learn_lang link)
                            ,("knownLang", toJSON $ known_lang link)
                            ,("soundalike", toJSON $ soundalike link)
                            ,("linkword", toJSON $ linkword link)
                            ,("stories", toJSON $ stories)
                            ]
 where storyJSON (storyNo, body, member, edited) =
         toJSON (storyNo, markdownToHtmlString body, (memberName member, gravatarHash <$> memberEmail member), showGregorian edited)

compactLinkJSON :: Link -> App Value
compactLinkJSON link = do
  -- I opted for an array representation because it's more compact. We could be
  -- sending a lot of these in a single response.
  -- We don't need to send the language information as the new review process only
  -- operates on 1 set of languages at a time.
  let e = linkExtra link
  return [aesonQQ| [ <| link_no link |>
                   , <| learn link |>
                   , <| known link |>
                   , <<e>>
                   ] |]
 where linkExtra l
         | isJust (linkword l) = [aesonQQ| {"linkword": <| fromJust (linkword l) |>} |]
         | soundalike l        = [aesonQQ| {"soundalike": <| True |>} |]
         | otherwise           = Null

-- Each link gets its own URI and page. Most of the extra code in the following is
-- for handling the display of link operations (``review'', ``delete'', etc.),
-- dealing with retrieval exceptions, etc.

-- For the link's owner, we'll send along the source of the link in a hidden
-- textarea for in-page editing.

linkPage :: Integer -> App CGIResult
linkPage linkNo = do
  memberNo <- memberNumber <$$> asks appMember
  db <- asks appDB
  row <- liftIO $ linkDetails linkNo db
  case row of
    Nothing -> outputNotFound
    Just link -> do
      hasPronunciation <- pronounceable linkNo
      row' <- $(queryTuple' "SELECT root_comment \
                            \FROM link_comment \
                            \WHERE link_no = {linkNo}")
      comments <- case row' of
                    Just root  -> renderComments root
                    Nothing    -> return mempty
      stories <- do ss <- linkWordStories linkNo
                    return $ mconcat $ map (\ (n, x, y, z) -> renderStory n x y z) ss
      rendered <- renderLink link hasPronunciation
      stdPage (learn link ++ " → " ++ known link ++ " — " ++ ("FIXME"::String) {- learn_language link -} ++ " to " ++ ("FIXME"::String) {- known_language link -}) [CSS "link", JS "link"] mempty $ do
        rendered
        div ! id "linkword-stories" $ do
          div ! class_ "header" $ h2 "Linkword Stories:"
          stories
        div ! id "comments" $ do
          h3 "Comments"
          comments

linksPage :: String -> [Link] -> App CGIResult
linksPage title' links = do
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

-- Generate a cloud of words from links in the database.

data WordStyle = WordStyle (Float, Float) (Float, Float) Int Int
  deriving (Show, Eq)

wordCloud :: [(String, Integer)] -> Int -> Int -> Int -> Int -> Int -> App Html
wordCloud words width' height' fontMin fontMax numClasses = do
  gen <- liftIO getStdGen
  let (styles, (newGen, _)) = runState (mapM (wordStyle . fst) words) (gen, [])
  liftIO $ setStdGen newGen
  return $ mconcat $ catMaybes $ zipWith (\ w s -> liftM (wordTag w) s) words styles
 where wordTag :: (String, Integer) -> WordStyle -> Html
       wordTag (word, linkNo) (WordStyle (x, y) _ classNum fontSize) =
         let style' = "font-size: " ++ show fontSize ++ "px; "
                   ++ "left: " ++ show x ++ "%; " ++ "top: " ++ show y ++ "%;" in
         a ! href (toValue $ "/link/" ++ show linkNo)
           ! class_ (toValue $ "class-" ++ show classNum)
           ! style (toValue style')
           $ toHtml word
       wordStyle :: String -> State (StdGen, [WordStyle]) (Maybe WordStyle)
       wordStyle word = do
         let fontRange = fontMax - fontMin
         fontSize <- (\ s -> fontMax - round (logBase 1.15 ((s * (1.15 ^ fontRange)::Float) + 1))) <$> getRandomR 0.0 1.0
         let widthP  = (100.0 / fromIntegral width')  * genericLength word * fromIntegral fontSize
             heightP = (100.0 / fromIntegral height') * fromIntegral fontSize
         x        <- getRandomR 0 (max (100 - widthP) 1)
         y        <- getRandomR 0 (max (100 - heightP) 1)
         class'   <- getRandomR 1 numClasses
         (gen, prev) <- get
         let spiral' = spiral 30.0 (x, y)
             styles  = filter inBounds $ map (\ pos -> WordStyle pos (widthP, heightP) class' fontSize) spiral'
             style'  = find (\ s -> not $ any (`overlap` s) prev) styles
         case style' of
           Nothing -> return Nothing
           Just style'' -> do
             put (gen, style'':prev)
             return $ Just style''
       getRandomR :: Random a => a -> a -> State (StdGen, [WordStyle]) a
       getRandomR min' max' = do
         (gen, styles) <- get
         let (n', newGen) = randomR (min', max') gen
         put (newGen, styles)
         return n'
       inBounds :: WordStyle -> Bool
       inBounds (WordStyle (x, y) (w, h) _ _) = x >= 0 && y >= 0 && x + w <= 100 && y + h <= 100
       overlap :: WordStyle -> WordStyle -> Bool
       -- We can't really be certain of when a word is overlapping,
       -- since the words will be rendered by the user's browser.
       -- However, we can make a guess.
       overlap (WordStyle (x1, y1) (w1', h1') _ _) (WordStyle (x2, y2) (w2', h2') _ _) =
         let hInter = (x2 > x1 && x2 < x1 + w1') || (x2 + w2' > x1 && x2 + w2' < x1 + w1') || (x2 < x1 && x2 + w2' > x1 + w1')
             vInter = (y2 > y1 && y2 < y1 + h1') || (y2 + h2' > y1 && y2 + h2' < y1 + h1') || (y2 < y1 && y2 + h2' > y1 + h1') in
         hInter && vInter
       spiral :: Float -> (Float, Float) -> [(Float, Float)]
       spiral maxTheta = spiral' 0.0
        where spiral' theta (x, y) =
                if theta > maxTheta
                  then []
                  else let r  = theta * 3
                           x' = (r * cos theta) + x
                           y' = (r * sin theta) + y in
                       (x', y') : spiral' (theta + 0.1) (x, y)
