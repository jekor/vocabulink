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

-- | Common HTML Functions

-- Much of Vocabulink consists of simple, program-generated HTML. Rather than
-- use templates or HTML in strings, we use an HTML combinator library
-- (Text.XHtml.Strict). This makes it almost certain that our HTML will be well
-- formed (although we have no guarantee that it will be valid). But more
-- importantly, it allows us to use abstraction to get higher-level HTML-based
-- functions. An example of this is |linkList|.

module Vocabulink.Html ( unordList, definitionList, multiColumn, multiColumnList, menu
                       , clear, markdownToHtml
                       {- Text.Blaze.Html5 -}
                       , Html, (!), string, stringValue, preEscapedString, customAttribute
                       , div, p, h1, h2, h3, hr, blockquote, script
                       , span, a, img, br, strong
                       , table, thead, tbody, tfoot, tr, td, th
                       , form, input, button, textarea, label, fieldset, legend
                       {- Text.Blaze.Html5.Attributes -}
                       , id, class_, href, type_, src, style, title
                       , width, height, alt, accesskey, colspan
                       , method, action, name, value, required, placeholder, autofocus
                       , tabindex, enctype, readonly, disabled
                       ) where

import Vocabulink.Utils

import Text.Blaze.Html5 ( Html, (!), string, stringValue, preEscapedString, customAttribute
                        , div, p, h1, h2, h3, hr, blockquote, script
                        , span, a, img, br, strong
                        , table, thead, tbody, tfoot, tr, td, th
                        , form, input, select, option, button
                        , ul, li, dl, dt, dd
                        , textarea, label, fieldset, legend
                        )
import Text.Blaze.Html5.Attributes ( id, class_, href, type_, src, style, title
                                   , width, height , alt, accesskey, colspan
                                   , method, action, name, value, required, placeholder, autofocus
                                   , tabindex, enctype, readonly, disabled
                                   )
import Text.Pandoc ( readMarkdown, writeHtmlString, defaultParserState
                   , defaultWriterOptions, writerHtml5
                   )

import Prelude hiding (div, id, span)

-- List Helpers

unordList :: [Html] -> Html
unordList items = ul $ mconcat $ map li items

definitionList :: [(Html, Html)] -> Html
definitionList items = dl $ mconcat $ map (\ (x, y) -> dt x >> dd y) items

multiColumn :: [Html] -> Html
multiColumn cls =
  let num = case length cls of
              1 -> "one"
              2 -> "two"
              3 -> "three"
              _ -> "unsupported" in
  div ! class_ (stringValue $ num ++ "-column") $
    mconcat (map (div ! class_ "column") cls ++ [clear])

multiColumnList :: Int -> [Html] -> Html
multiColumnList 1 xs  = multiColumn [unordList xs]
multiColumnList 2 xs  =
  let (col1, col2) = partitionHalves xs in
  multiColumn [unordList col1, unordList col2]
multiColumnList 3 xs  =
  let (col1, col2, col3) = partitionThirds xs in
  multiColumn [unordList col1, unordList col2, unordList col3]
multiColumnList _ _   = error "Unsupported number of columns."

-- Form Helpers

-- Sometimes you just want a select list where the displayed options match
-- their values.

menu :: [(String, String)] -> Html
menu choices = select $ mconcat
  [ option ! value (stringValue $ fst choice) $ string (snd choice) | choice <- choices ]

-- Other Markup

clear :: Html
clear = div ! class_ "clear" $ mempty

-- A modified version of Markdown (Pandoc Markdown) is used in comments and
-- link bodies. We need to sanitize incoming HTML so that we don't end up with
-- XSS attacks.

markdownToHtml :: String -> Html
markdownToHtml = preEscapedString . writeHtmlString defaultWriterOptions {writerHtml5 = True} . readMarkdown defaultParserState
