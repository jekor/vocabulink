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

module Vocabulink.Link.Html ( renderLink, linksTable
                            , linkPage, linksPage, languagePairsPage
                            , wordCloud
                            ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Comment
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Link.Pronunciation
import Vocabulink.Link.Story
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Control.Monad.State (State, runState, get, put)
import Data.List (find, genericLength, sortBy, groupBy)
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
  return $ h1 ! class_ (toValue $ "link " ++ linkTypeName link) $ do
    span ! class_ "foreign" ! customAttribute "lang" (toValue $ learn_lang link) ! title (toValue $ learn_language link) $ do
      toHtml $ learn link
      pronunciation
    span ! class_ "link" ! title (toValue $ linkTypeName link) $
      renderLinkExtra link
    span ! class_ "familiar" ! customAttribute "lang" (toValue $ known_lang link) ! title (toValue $ known_language link) $ toHtml $ known link
 where renderLinkExtra :: Link -> Html
       renderLinkExtra link' = case linkword link' of
                                 Nothing -> mempty
                                 Just w  -> toHtml w
       pronunciation = if pronounceable'
                         then button ! id "pronounce" ! class_ "button light" $ do
                                audio ! preload "auto" $ do
                                  source ! src (toValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (link_no link) ++ ".ogg") $ mempty
                                  source ! src (toValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (link_no link) ++ ".mp3") $ mempty
                                img ! src "http://s.vocabulink.com/img/icon/audio.png"
                         else mempty

linksTable :: [Link] -> Html
linksTable links = table ! class_ "links" $ do
  thead $ do
    tr $ do
      th "Foreign"
      th "Familiar"
      th "Link Type"
  tbody $ mconcat $ map linkRow links
 where linkRow link = let url = "/link/" ++ show (link_no link) in
         tr ! class_ (toValue $ "inline-link " ++ linkTypeName link) $ do
           td $ a ! href (toValue url) $ toHtml $ learn link
           td $ a ! href (toValue url) $ toHtml $ known link
           td $ a ! href (toValue url) $ toHtml $ linkTypeName link

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
      ops <- linkOperations link
      row' <- $(queryTuple' "SELECT root_comment \
                            \FROM link_comment \
                            \WHERE link_no = {linkNo}")
      comments <- case row' of
                    Just root  -> renderComments root
                    Nothing    -> return mempty
      stories <- do ss <- linkWordStories linkNo
                    return $ mconcat $ map (\ (n, x, y, z) -> renderStory n x y z) ss
      rendered <- renderLink link hasPronunciation
      stdPage (learn link ++ " → " ++ known link ++ " — " ++ learn_language link ++ " to " ++ known_language link) [CSS "link", JS "link"] mempty $ do
        div ! id "link-head-bar" $ do
          h2 $ a ! href (toValue $ "/links?ol=" ++ learn_lang link ++ "&dl=" ++ known_lang link)
             $ toHtml $ learn_language link ++ " to " ++ known_language link ++ ":"
          div ! id "link-ops" $ do
            ops
        rendered
        div ! id "linkword-stories" $ do
          div ! class_ "header" $ h2 "Linkword Stories:"
          stories
        div ! id "comments" $ do
          h3 "Comments"
          comments

linkOperations :: Link -> App Html
linkOperations link = do
  member <- asks appMember
  reviewing' <- reviewing link
  let review  = linkAction "add to review" "add"
  return $ do
    case (member, reviewing') of
      (_,       True) -> review False ! title "already reviewing this link"
      (Just _,  _)    -> review True  ! id "link-op-review"
                                      ! title "add this link to be quizzed on it later"
      (Nothing, _)    -> review False ! title "login to review"
 where reviewing :: Link -> App Bool
       reviewing l = do
         member <- asks appMember
         case member of
           Nothing -> return False
           Just m  -> (/= []) <$> $(queryTuples'
             "SELECT link_no FROM link_to_review \
             \WHERE member_no = {memberNumber m} AND link_no = {link_no l} \
             \LIMIT 1")

linkAction :: String -> String -> Bool -> Html
linkAction label' icon' enabled =
  let icon = "http://s.vocabulink.com/img/icon/" ++
             icon' ++
             (enabled ? "" $ "-disabled") ++
             ".png" in
  a ! class_ (toValue $ ("operation login-required "::String) ++ (enabled ? "enabled" $ "disabled")) ! href "" $ do
    img ! src (toValue icon) ! class_ "icon"
    toHtml label'

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

languagePairsPage :: App CGIResult
languagePairsPage = do
  db <- asks appDB
  languages' <- liftIO ((groupBy groupByName . sortBy compareNames) <$> linkLanguages db)
  simplePage "Links By Language" [CSS "link"] $ do
    mconcat $ map renderLanguageGroup $ sortBy compareSize languages'
 where compareNames ((_, ol1), (_, dl1), _) ((_, ol2), (_, dl2), _) =
         if dl1 == dl2
            then compare ol1 ol2
            else compare dl1 dl2
       compareSize g1 g2 = compare (languageSize g2) (languageSize g1)
       languageSize = sum . map (\(_, _, c) -> c)
       groupByName ((_, _), (_, dl1), _) ((_, _), (_, dl2), _) = dl1 == dl2
       renderLanguageGroup g = div ! class_ "group-box languages" $ do
         h2 $ toHtml $ "in " ++ groupLanguage g ++ ":"
         multiColumnList 3 $ map renderLanguage g
       groupLanguage = (\((_, _), (_, n), _) -> n) . head
       renderLanguage ((oa, on), (da, _), _) =
         a ! class_ "faint-gradient-button blue language-button" ! href (toValue $ "/links?ol=" ++ oa ++ "&dl=" ++ da)
           $ toHtml on

-- Generate a cloud of words from links in the database.

data WordStyle = WordStyle (Float, Float) (Float, Float) Int Int
  deriving (Show, Eq)

wordCloud :: Int -> Int -> Int -> Int -> Int -> Int -> App Html
wordCloud n width' height' fontMin fontMax numClasses = do
  words <- $(queryTuples'
    "SELECT learn, link_no FROM link \
    \WHERE NOT deleted AND link_no IN \
     \(SELECT DISTINCT link_no FROM linkword_story) \
    \ORDER BY random() LIMIT {n}")
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
