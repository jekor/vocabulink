> module Vocabulink.Html (Dependency(..), stdPage, pager, simpleChoice, onclick,
>                         accesskey, formName) where

> import Vocabulink.App
> import Vocabulink.Utils ((?))

> import {-# SOURCE #-} Vocabulink.Review (numLinksToReview)

> import Codec.Binary.UTF8.String (decodeString, encodeString)
> import Control.Monad.Reader (asks)
> import Data.Time.Clock (getCurrentTime)
> import Data.Time.Format (formatTime)
> import Network.FastCGI (CGIResult, output, getVar, requestURI)
> import Network.URI (uriPath)
> import System.Locale (defaultTimeLocale)
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.XHtml.Strict

This is a common pattern.

> data Dependency = CSS String | JS String

stdPage expects title to already be UTF8 encoded if necessary.

> stdPage :: String -> [Dependency] -> [Html] -> App CGIResult
> stdPage t deps h = do
>   headerB <- headerBar
>   footerB <- footerBar
>   output $ renderHtml $ header <<
>     (thetitle << t +++ concatHtml (map includeDep ((CSS "page"):deps))) +++
>     body << (headerB +++ concatHtml h +++ footerB)

> includeDep :: Dependency -> Html
> includeDep (CSS css) =
>   thelink ! [href ("http://s.vocabulink.com/" ++ css ++ ".css"),
>              rel "stylesheet", thetype "text/css"] << noHtml
> includeDep (JS js) =
>   script ! [src ("http://s.vocabulink.com/" ++ js ++ ".js"),
>             thetype "text/javascript"] << noHtml

It's common to use an unordered list to present a series of links. For example, both the standard header and footer use this.

> linkList :: (HTML a) => [a] -> Html
> linkList items = ulist ! [theclass "links"] << map (li <<) items

> headerBar :: App Html
> headerBar = do
>   username <- asks memberName
>   review <- reviewBox
>   return $ thediv ! [identifier "header-bar"] <<
>     [ anchor ! [theclass "logo", href "/", accesskey "1"] << "Vocabulink",
>       topLinks,
>       loginBox username,
>       searchBox,
>       review,
>       thediv ! [theclass "clear"] << noHtml ]

Here are some links we want in the header of every page.

Get Started | Articles | Help

> topLinks :: Html
> topLinks = linkList
>   [ anchor ! [href "/article/how-to-get-started-with-vocabulink"] << "Get Started",
>     anchor ! [href "/articles/"] << "Articles",
>     anchor ! [href "/help"] << "Help" ]

Create a login or logout form based on whether or not the user's logged in.

> loginBox :: Maybe String -> Html
> loginBox username =
>   case username of
>     Nothing -> form ! [theclass "login-box login", action "/member/login", method "post"] <<
>                  [ label << "Username:",
>                    textfield "username",
>                    label << "Password:",
>                    password "password",
>                    submit "" "Log In",
>                    stringToHtml " ",
>                    submit "join" "Join" ]
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

> footerBar :: App Html
> footerBar = do
>   copy <- copyrightNotice
>   return $ thediv ! [identifier "footer-bar"] <<
>     [ linkList
>       [ anchor ! [href "/help"] << "help",
>         anchor ! [href "/privacy"] << "privacy policy",
>         anchor ! [href "/copyrights"] << "copyright policy",
>         anchor ! [href "/disclaimer"] << "disclaimer"],
>       copy ]

> copyrightNotice :: App Html
> copyrightNotice = do
>   year <- liftIO currentYear
>   return $ paragraph ! [theclass "copyright"] <<
>     [ stringToHtml $ encodeString "© 2008–",
>       stringToHtml (year ++ " "),
>       anchor ! [href "http://jekor.com/"] << "Chris Forno" ]

> currentYear :: IO String
> currentYear = do
>   now <- getCurrentTime
>   return $ formatTime defaultTimeLocale "%Y" now

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
>     [ (pg > 1 ? anchor ! [href (pth ++ prev), theclass "prev"] $ thespan ! [theclass "prev"]) << "Previous", stringToHtml " ",
>       ((pg * n < total) ? anchor ! [href (pth ++ next), theclass "next"] $ thespan ! [theclass "next"]) << "Next" ]

Sometimes you just want a choice box where the displayed choices match their values.

> simpleChoice :: String -> [String] -> Html
> simpleChoice n choices =
>   select ! [identifier n, name n] << [ option ! [value choice] << choice | choice <- choices ]

> onclick :: String -> HtmlAttr
> onclick = strAttr "onclick"

> accesskey :: String -> HtmlAttr
> accesskey = strAttr "accesskey"

Let's keep our form names safe.

> formName :: String -> String
> formName = map replaceChar
>     where replaceChar c =
>               case c of
>                 ' ' -> '-'
>                 c'  -> c'