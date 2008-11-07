> module Vocabulink.Html where

> import Vocabulink.Utils

> import Codec.Binary.UTF8.String
> import Network.CGI
> import Network.URI
> import Text.Regex
> import Text.Regex.Posix
> import Text.XHtml.Strict

This is a common pattern.

> outputHtml :: Html -> CGI CGIResult
> outputHtml = output . renderHtml

> data Dependency = CSS String | JS String

page expects title to already be UTF8 encoded if necessary.

> page :: String -> [Dependency] -> ([Html] -> Html)
> page t ds = \b -> header <<
>   (thetitle << t +++ concatHtml (map includeDep ds)) +++
>   body << b

> stdPage :: String -> Maybe String -> [Dependency] -> ([Html] -> Html)
> stdPage t username deps = \b -> header <<
>   (thetitle << t +++ concatHtml (map includeDep deps)) +++
>   body << headerBar username +++ concatHtml b

> includeDep :: Dependency -> Html
> includeDep (CSS css) =
>   thelink ! [href ("http://s.vocabulink.com/" ++ css ++ ".css"),
>              rel "stylesheet", thetype "text/css"] << noHtml
> includeDep (JS js) =
>   script ! [src ("http://s.vocabulink.com/" ++ js ++ ".js"),
>             thetype "text/javascript"] << noHtml

> headerBar :: Maybe String -> Html
> headerBar username =
>   thediv ! [identifier "header-bar"] <<
>     [ loginBox username ]

Create a login or logout form based on whether or not the user's logged in.

> loginBox :: Maybe String -> Html
> loginBox username =
>   case username of
>     Nothing -> form ! [theclass "loginout login", action "/member/login", method "post"] <<
>                  [ label << "Username:",
>                    textfield "username",
>                    label << "Password:",
>                    password "password",
>                    submit "" "Log In" ]
>     Just n  -> form ! [theclass "loginout logout", action "/member/logout", method "post"] <<
>                  [ stringToHtml n,
>                    submit "" "Log Out" ]

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

> pager :: Int -> Int -> Int -> CGI Html
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
