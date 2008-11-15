> module Vocabulink.Html (Dependency(..), stdPage, pager, simpleChoice, onclick, formName) where

> import Vocabulink.App
> import Vocabulink.Utils ((?))

> import {-# SOURCE #-} Vocabulink.Review (numLinksToReview)

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Monad.Reader (asks)
> import Network.FastCGI (CGIResult, output, getVar, requestURI)
> import Network.URI (uriPath)
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.XHtml.Strict

This is a common pattern.

> data Dependency = CSS String | JS String

stdPage expects title to already be UTF8 encoded if necessary.

> stdPage :: String -> [Dependency] -> [Html] -> App CGIResult
> stdPage t deps h = do
>   headerB <- headerBar
>   output $ renderHtml $ header <<
>     (thetitle << t +++ concatHtml (map includeDep ((CSS "page"):deps))) +++
>     body << (headerB +++ concatHtml h)

> includeDep :: Dependency -> Html
> includeDep (CSS css) =
>   thelink ! [href ("http://s.vocabulink.com/" ++ css ++ ".css"),
>              rel "stylesheet", thetype "text/css"] << noHtml
> includeDep (JS js) =
>   script ! [src ("http://s.vocabulink.com/" ++ js ++ ".js"),
>             thetype "text/javascript"] << noHtml

> headerBar :: App Html
> headerBar = do
>   username <- asks memberName
>   review <- reviewBox
>   return $ thediv ! [identifier "header-bar"] <<
>     [ anchor ! [theclass "logo", href "/"] << "Vocabulink",
>       loginBox username,
>       searchBox,
>       review,
>       thediv ! [theclass "clear"] << noHtml ]

Create a login or logout form based on whether or not the user's logged in.

> loginBox :: Maybe String -> Html
> loginBox username =
>   case username of
>     Nothing -> form ! [theclass "login-box login", action "/member/login", method "post"] <<
>                  [ label << "Username:",
>                    textfield "username",
>                    label << "Password:",
>                    password "password",
>                    submit "" "Log In" ]
>     Just n  -> form ! [theclass "login-box logout", action "/member/logout", method "post"] <<
>                  [ stringToHtml n,
>                    submit "" "Log Out" ]

> searchBox :: Html
> searchBox = form ! [theclass "search-box", action "/search", method "get"] <<
>   [ textfield "q", submit "" "Search" ]

> reviewBox :: App Html
> reviewBox = do
>   memberNo <- asks memberNumber
>   case memberNo of
>     Nothing  -> return noHtml
>     Just memberNo' -> do
>       n <- numLinksToReview memberNo'
>       let r = case n of
>                 0  -> anchor ! [href "/links", theclass "review-box"] << "No links to review"
>                 n' -> anchor ! [href "/review/next", theclass "review-box"] <<
>                         [ (strong << show n') +++ " links to review" ]
>       return r

It's nice to abstract away creating an element to page the results of a
multi-page query. This will preserve all of the query string in the links it
generates while it replaces the "n" (number of records per page) and "page"
(the page we're on) variables.

First, we the query string.

> pageQueryString :: Int -> Int -> String -> String
> pageQueryString n pg q  =
>   let q1 = q  =~ nRegex  ? subRegex (mkRegex nRegex)  q  ("n=" ++ show n) $
>                            q ++ ("&n=" ++ show n)
>       q2 = q1 =~ pgRegex ? subRegex (mkRegex pgRegex) q1 ("pg=" ++ show pg) $
>                            q1 ++ ("&pg=" ++ show pg) in
>   "?" ++ q2
>     where nRegex  = "n=[^&]+"
>           pgRegex = "pg=[^&]+"

And now for the HTML.

> pager :: Int -> Int -> Int -> App Html
> pager n pg total = do
>   q <- getVar "QUERY_STRING"
>   uri <- requestURI
>   let pth  = uriPath uri
>       q'   = maybe "" decodeString q
>       prev = pageQueryString n (pg - 1) q'
>       next = pageQueryString n (pg + 1) q'
>   return $ paragraph ! [theclass "pager"] << thespan ! [theclass "controls"] <<
>     [ (pg > 1 ? anchor ! [href (pth ++ prev), theclass "prev"] $ thespan ! [theclass "prev"]) << "Previous",
>       ((pg * n < total) ? anchor ! [href (pth ++ next), theclass "next"] $ thespan ! [theclass "next"]) << "Next" ]

Sometimes you just want a choice box where the displayed choices match their values.

> simpleChoice :: String -> [String] -> Html
> simpleChoice n choices =
>   select ! [identifier n, name n] << [ option ! [value choice] << choice | choice <- choices ]

> onclick :: String -> HtmlAttr
> onclick = strAttr "onclick"

Let's keep our form names safe.

> formName :: String -> String
> formName = map replaceChar
>     where replaceChar c =
>               case c of
>                 ' ' -> '-'
>                 c'  -> c'