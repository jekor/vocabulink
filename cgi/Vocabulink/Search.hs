-- Copyright 2009, 2010, 2011 Chris Forno

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
  member <- asks appMember
  links <- linksContaining q
  links' <- mapM renderPartialLink links
  stdPage (q ++ " - Search Results") [CSS "search"] mempty $ do
    div ! id "main-content" $ do
      div ! id "cse-search-results" $ mempty
    div ! id "sidebar" $ do
      case memberEmail =<< member of
        Just _ -> div ! id "new-link" $ do
                    a ! href (stringValue $ "/link/new?foreign=" ++ q) $
                      string ("→ Create a new link with \"" ++ q ++ "\"")
        _      -> mempty
      div ! class_ "sidebox" $ do
        h3 $ string ("Found " ++ show (length links) ++ " Links Containing \"" ++ q ++ "\"")
        unordList links' ! class_ "links"
    script ! type_ "text/javascript" $ preEscapedString (unlines
      [ "var googleSearchIframeName = \"cse-search-results\";"
      , "var googleSearchFormName = \"cse-search-box\";"
      , "var googleSearchFrameWidth = 600;"
      , "var googleSearchDomain = \"www.google.com\";"
      , "var googleSearchPath = \"/cse\";"
      ])
    script ! src "http://www.google.com/afsonline/show_afs_search.js" $ mempty
