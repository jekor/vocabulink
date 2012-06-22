-- Copyright 2009, 2010, 2011, 2012 Chris Forno

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

-- As Vocabulink gains more content, search becomes more important. I've been
-- noticing it painfully myself lately, and I know where everything is!

module Vocabulink.Search (searchPage) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Link.Html
import Vocabulink.Member.Auth
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, span, id)

-- For now, we're making use of Google Custom Search.

searchPage :: App CGIResult
searchPage = do
  q <- getRequiredInput "q"
  links <- linksContaining q
  linksTable <- partialLinksTable' links
  stdPage (q ++ " - Search Results") [JS "link", CSS "link", CSS "search", ReadyJS initJS] mempty $ do
    div ! id "results" $ do
      h2 $ string ("Found " ++ show (length links) ++ " Links Containing \"" ++ q ++ "\"")
      linksTable
 where initJS = "$('table.links').longtable();"

-- TODO: Consolidate with partialLinksTable'
partialLinksTable' :: [(PartialLink, Maybe Integer)] -> App Html
partialLinksTable' links = do
  rows <- mapM linkRow links
  return $ table ! class_ "links" $ do
    thead $ do
      tr $ do
        th "Language"
        th "Foreign"
        th "Familiar"
        th "Link Type"
        th "Rank"
    tbody $ mconcat rows
 where linkRow (link, rank) = do
         lang <- linkForeignLanguage $ pLink link
         let url = "/link/" ++ show (linkNumber $ pLink link)
         return $ tr ! class_ (stringValue $ "partial-link " ++ (linkTypeName $ pLink link)) $ do
           td $ a ! href (stringValue url) $ string $ lang
           td $ a ! href (stringValue url) $ string $ linkForeignPhrase $ pLink link
           td $ a ! href (stringValue url) $ string $ linkFamiliarPhrase $ pLink link
           td $ a ! href (stringValue url) $ string $ linkTypeName $ pLink link
           td $ a ! href (stringValue url) $ string $ maybe "" show rank

linksContaining :: String -> App [(PartialLink, Maybe Integer)]
linksContaining q = do
  let q' = "%" ++ q ++ "%"
  exacts <- map partialLinkFromTuple' <$> $(queryTuples'
    "SELECT link_no, link_type, author, \
           \foreign_phrase, familiar_phrase, \
           \foreign_language, familiar_language, \
           \MIN(rank) \
    \FROM link \
    \LEFT JOIN link_frequency USING (link_no) \
    \WHERE NOT deleted \
      \AND (foreign_phrase ~~* {q} OR familiar_phrase ~~* {q} \
        \OR unaccent(foreign_phrase) ~~* {q} OR unaccent(familiar_phrase) ~~* {q}) \
    \GROUP BY link_no, link_type, author, foreign_phrase, familiar_phrase, foreign_language, familiar_language")
  closes <- map partialLinkFromTuple' <$> $(queryTuples'
    "SELECT link_no, link_type, author, \
           \foreign_phrase, familiar_phrase, \
           \foreign_language, familiar_language, \
           \MIN(rank) \
    \FROM link \
    \LEFT JOIN link_frequency USING (link_no) \
    \WHERE NOT deleted \
      \AND (foreign_phrase !~~* {q} AND familiar_phrase !~~* {q} \
        \AND unaccent(foreign_phrase) !~~* {q} AND unaccent(familiar_phrase) !~~* {q}) \
      \AND (foreign_phrase ~~* {q'} OR familiar_phrase ~~* {q'} \
        \OR unaccent(foreign_phrase) ~~* {q'} OR unaccent(familiar_phrase) ~~* {q'}) \
    \GROUP BY link_no, link_type, author, foreign_phrase, familiar_phrase, foreign_language, familiar_language \
    \ORDER BY char_length(foreign_phrase)")
  return $ exacts ++ closes
 where partialLinkFromTuple' (a,b,c,d,e,f,g, rank) = (partialLinkFromTuple (a,b,c,d,e,f,g), rank)
