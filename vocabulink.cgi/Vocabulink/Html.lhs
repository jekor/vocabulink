\section{Html}

Much of Vocabulink consists of simple, program-generated HTML. Rather than use
templates or HTML in strings, we use an HTML combinator library
(Text.XHtml.Strict). This makes it easier to write valid markup (although it
doesn't guarantee it). But more importantly, it allows us to use abstraction to
get higher-level HTML-based functions. An example of this is |linkList|.

> module Vocabulink.Html (  Dependency(..), stdPage, headerBar, footerBar,
>                           displayStaticFile, pager, simpleChoice, onclick,
>                           accesskey, formName,
>                           (<<), (+++), (!),
>  {- Text.XHtml.Strict -}  Html, noHtml, primHtml, stringToHtml, concatHtml,
>                           identifier, theclass, thediv, thespan,
>                           paragraph, pre, h1, h2, h3, br, anchor, href,
>                           image, unordList, form, action, method, fieldset,
>                           hidden, label, textfield, password, button, submit,
>                           textarea, widget,
>                           thestyle, src, width, height, value, name,
>                           cols, rows) where

> import Vocabulink.App
> import Vocabulink.Utils

We display the number of links that are waiting for review for logged in users
in the standard page header. Reviewing is currently the primary function of
Vocabulink, and we want it prominently displayed.

> import {-# SOURCE #-} Vocabulink.Review (numLinksToReview)

Currently Text.XHtml does not automatically handle UTF-8 output. We have to
carefully encode everything we need to. If we don't, non-ASCII (non-iso8859-1?)
characters will be converted to entities. This automatic conversion may be a
nice fallback, but it can mask an underlying problem.

> import Codec.Binary.UTF8.String (decodeString, encodeString)
> import Control.Monad.Reader (asks)
> import Network.FastCGI (CGIResult, output, getVar, requestURI)
> import Network.URI (uriPath)
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.XHtml.Strict

Most pages depend on some external CSS and/or JavaScript files.

> data Dependency = CSS String | JS String

|stdPage| takes a title, a list of dependencies, and list of HTML objects to
place into the body of the page. It automatically adds a standard header and
footer. It also includes @page.css@.

|stdPage| expects title to already be encoded as UTF-8.

> stdPage :: String -> [Dependency] -> [Html] -> App CGIResult
> stdPage t deps h = do
>   headerB  <- headerBar
>   footerB  <- footerBar
>   output $ renderHtml $ header <<
>     (thetitle << t +++ concatHtml (map includeDep ((CSS "page"):deps))) +++
>     body << (headerB +++ concatHtml h +++ footerB)

The standard header bar shows the Vocabulink logo (with a link to the root
page), a list of links (currently static, but eventually configurable by the
member), and either a login box and join button or a count of links waiting for
review and a logout button.

> headerBar :: App Html
> headerBar = do
>   username <- asks memberName
>   review <- reviewBox
>   return $ thediv ! [identifier "header-bar"] <<
>     [ anchor ! [theclass "logo", href "/", accesskey "1"] << "Vocabulink",
>       topLinks,
>       maybe loginBox logoutBox username,
>       searchBox,
>       review,
>       thediv ! [theclass "clear"] << noHtml ]

The footer bar is more simple. It just includes some links to static content.

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

Each dependency is expressed as the path from the root of the static subdomain
(@s.deviantart.com@) to the file. Do not include the file suffix (@.css@ or
@.js@), it will be appended automatically. These are meant for inclusion in the
@<head>@ of the page.

> includeDep :: Dependency -> Html
> includeDep (CSS css) =
>   thelink ! [href ("http://s.vocabulink.com/" ++ css ++ ".css"),
>              rel "stylesheet", thetype "text/css"] << noHtml
> includeDep (JS js) =
>   script ! [src ("http://s.vocabulink.com/" ++ js ++ ".js"),
>             thetype "text/javascript"] << noHtml

Not everything on Vocabulink is dynamic. We don't want to include long text
(something that markup is good at) in our source code. To display a static
file, we simply read it into memory, convert it into an Html primative, and
add it to our standard page.

The static file must be be a valid fragment of XHTML.

> displayStaticFile :: String -> FilePath -> App CGIResult
> displayStaticFile t path = do
>   bodyHtml <- liftIO $ readFile path
>   stdPage t [] [primHtml bodyHtml]

Here are the links we want in the header of every page.

> topLinks :: Html
> topLinks = linkList
>   [  anchor ! [href "/article/how-to-get-started-with-vocabulink"] << "Get Started",
>      anchor ! [href "/articles/"] << "Articles",
>      anchor ! [href "/help"] << "Help" ]

This provides a simple login box for members.

> loginBox :: Html
> loginBox = form ! [  theclass "auth-box login", action "/member/login",
>                      method "post"] <<
>              [  label << "Username:",
>                 textfield "username",
>                 label << "Password:",
>                 password "password",
>                 submit "" "Log In",
>                 stringToHtml " ",
>                 submit "join" "Join" ]

...and a simple logout button for logged-in members (with an indicator of the
currently logged-in member's username).

> logoutBox :: String -> Html
> logoutBox username = form ! [  theclass "auth-box logout", action "/member/logout",
>                                method "post"] <<
>                        [  stringToHtml username,
>                           submit "" "Log Out" ]

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

> copyrightNotice :: App Html
> copyrightNotice = do
>   year <- liftIO currentYear
>   return $ paragraph ! [theclass "copyright"] <<
>     [ stringToHtml $ encodeString "© 2008–",
>       stringToHtml ((show year) ++ " "),
>       anchor ! [href "http://jekor.com/"] << "Chris Forno" ]

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

\subsection{Higher-Level Combinators}

It's common to use an unordered list to present a series of links. For example, both the standard header and footer use this.

> linkList :: (HTML a) => [a] -> Html
> linkList items = ulist ! [theclass "links"] << map (li <<) items

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