% Copyright 2008, 2009, 2010 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\section{Html}

Much of Vocabulink consists of simple, program-generated HTML. Rather than use
templates or HTML in strings, we use an HTML combinator library
(Text.XHtml.Strict). This makes it almost certain that our HTML will be well
formed (although we have no guarantee that it will be valid). But more
importantly, it allows us to use abstraction to get higher-level HTML-based
functions. An example of this is |linkList|.

> module Vocabulink.Html (Html, (!), string, stringValue, preEscapedString,
>                         div, span, p, a, img, h1, h2, h3, hr, br,
>                         table, thead, tbody, tfoot, tr, td, th,
>                         id, class_, href, type_, src, style, title,
>                         width, height, alt, accesskey, colspan,
>                         unordList, multiColumn, multiColumnList,
>                         clear, markdownToHtml) where

> import Vocabulink.Utils

> import Text.Blaze.Html5 ((!), Html, string, stringValue, preEscapedString,
>                          div, span, p, a, img, h1, h2, h3, hr, br,
>                          ul, li, table, tr, th, td, thead, tbody, tfoot)
> import Text.Blaze.Html5.Attributes (id, class_, href, type_, src, style,
>                                     width, height, title, alt, accesskey,
>                                     colspan)
> import Text.Pandoc (readMarkdown, writeHtmlString, defaultParserState,
>                     defaultWriterOptions, stateSanitizeHTML)

> import Prelude hiding (div, id, span)

\section{List Helpers}

> unordList :: [Html] -> Html
> unordList items =
>   let lis = mconcat $ map li items in
>   ul $ lis

> multiColumn :: [Html] -> Html
> multiColumn cls =
>   let num = case length cls of
>               1 -> "one"
>               2 -> "two"
>               3 -> "three"
>               _ -> "unsupported" in
>   div ! class_ (stringValue $ num ++ "-column") $ mconcat $
>     (map (div ! class_ "column") cls ++ [clear])

> multiColumnList :: Int -> [Html] -> Html
> multiColumnList 2 xs  =
>   let (col1, col2) = every2nd xs in
>   multiColumn [unordList col1, unordList col2]
> multiColumnList 3 xs  =
>   let (col1, col2, col3) = every3rd xs in
>   multiColumn [unordList col1, unordList col2, unordList col3]
> multiColumnList _ _   = error "Unsupported number of columns."

\subsection{Other Markup}

> clear :: Html
> clear = div ! class_ "clear" $ mempty

A modified version of Markdown (Pandoc Markdown) is used in comments and link
bodies. We need to sanitize incoming HTML so that we don't end up with XSS
attacks.

> markdownToHtml :: String -> Html
> markdownToHtml = preEscapedString . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState {stateSanitizeHTML = True}
