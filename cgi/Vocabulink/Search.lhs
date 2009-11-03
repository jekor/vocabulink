% Copyright 2009 Chris Forno

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

\section{Search}

As Vocabulink gains more content, search becomes more important. I've been
noticing it painfully myself lately, and I know where everything is!

> module Vocabulink.Search (searchPage) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.CGI
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Utils

For now, we're making use of Google Custom Search.

> searchPage :: App CGIResult
> searchPage = do
>   q <- getRequiredInput "q"
>   links   <- fromMaybe [] <$> linksContaining q
>   links'  <- mapM renderPartialLink links
>   stdPage (q ++ " - Search Results") [CSS "search"] [] [
>     thediv ! [identifier "main-content"] << [
>       thediv ! [identifier "cse-search-results"] << noHtml ],
>     thediv ! [identifier "sidebar"] << [
>       thediv ! [identifier "new-link"] << [
>         anchor ! [href ("/link/new?fval0=" ++ q)] << ("→ Create a new link with \"" ++ q ++ "\"") ],
>       thediv ! [theclass "sidebox"] << [
>         h3 << ("Found " ++ show (length links) ++ " Links Containing \"" ++ q ++ "\""),
>         unordList links' ! [theclass "links"] ] ],
>     script ! [thetype "text/javascript"] << primHtml (unlines [
>       "var googleSearchIframeName = \"cse-search-results\";",
>       "var googleSearchFormName = \"cse-search-box\";",
>       "var googleSearchFrameWidth = 600;",
>       "var googleSearchDomain = \"www.google.com\";",
>       "var googleSearchPath = \"/cse\";" ]),
>     script ! [src "http://www.google.com/afsonline/show_afs_search.js"] << noHtml ]

This is a close parallel to the original search for Vocabulink. We use it so
that we can display a link to a specialized link search only when results are
available.

> linksContaining :: String -> App (Maybe [PartialLink])
> linksContaining q = do
>   ts <- queryTuples'  "SELECT link_no, link_type, author, \
>                              \origin, destination, \
>                              \origin_language, destination_language \
>                       \FROM link \
>                       \WHERE NOT deleted \
>                         \AND (origin ILIKE ? OR destination ILIKE ?) \
>                       \LIMIT 20"
>                       [toSql q, toSql q]
>   return $ mapMaybe partialLinkFromValues `liftM` ts
