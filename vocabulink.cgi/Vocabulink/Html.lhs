\section{Html}

Much of Vocabulink consists of simple, program-generated HTML. Rather than use
templates or HTML in strings, we use an HTML combinator library
(Text.XHtml.Strict). This makes it easier to write valid markup (although it
doesn't guarantee it). But more importantly, it allows us to use abstraction to
get higher-level HTML-based functions. An example of this is |linkList|.

> module Vocabulink.Html (  Dependency(..), stdPage, displayStaticFile,
>                           linkList, options, accesskey, safeID,
>                           pager, currentPage,
>  {- Text.XHtml.Strict -}  Html, noHtml, primHtml, stringToHtml, concatHtml,
>                           (<<), (+++), (!),
>                           identifier, theclass, thediv, thespan,
>                           paragraph, pre, h1, h2, h3, br, anchor, href,
>                           image, unordList, form, action, method, fieldset,
>                           hidden, label, textfield, password, button, submit,
>                           textarea, select, widget,
>                           thestyle, src, width, height, value, name,
>                           cols, rows) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Member.Auth
> import Vocabulink.Utils

> import {-# SOURCE #-} Vocabulink.Review (numLinksToReview)

Currently Text.XHtml does not automatically handle UTF-8 output. We have to
carefully encode everything we need to. If we don't, non-ASCII (non-iso8859-1?)
characters will be converted to entities. This automatic conversion may be a
nice fallback, but it can mask an underlying problem.

> import Codec.Binary.UTF8.String (decodeString, encodeString)
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

Here are the links we want in the header of every page.

> topLinks :: Html
> topLinks = linkList
>   [  anchor ! [href "/article/how-to-get-started-with-vocabulink"] << "Get Started",
>      anchor ! [href "/articles/"] << "Articles",
>      anchor ! [href "/help"] << "Help" ]

The footer bar is more simple. It just includes some links to static content.

> footerBar :: App Html
> footerBar = do
>   copy <- copyrightNotice
>   return $ thediv ! [identifier "footer-bar"] <<
>     [ linkList
>       [  anchor ! [href "/help"] << "help",
>          anchor ! [href "/privacy"] << "privacy policy",
>          anchor ! [href "/copyrights"] << "copyright policy",
>          anchor ! [href "/disclaimer"] << "disclaimer"],
>       copy ]

We want a copyright notice at the bottom of every page. Since this is a
copyright notice for dynamic content, we want it to be up-to-date with the
generation time (now).

> copyrightNotice :: App Html
> copyrightNotice = do
>   year <- liftIO currentYear
>   return $ paragraph ! [theclass "copyright"] <<
>     [ stringToHtml $ encodeString "© 2008–",
>       stringToHtml ((show year) ++ " "),
>       anchor ! [href "http://jekor.com/"] << "Chris Forno" ]

This provides a simple login box for members. Actually, it's a box for a
username, a box for a password, a login button, and a join button. We want to
make joining simple, so you can enter a username and password and then click
join and it will redirect you to the new member form with the username
pre-filled. Pre-filling the password field would require either a POST-based
redirect (which, if possible, will probably distract the user) or storing
state. I've also not seen the behavior before which might also confuse the
user.

> loginBox :: Html
> loginBox = form ! [  theclass "auth-box login", action "/member/login",
>                      method "post"] <<
>              [  label << "Username:",  textfield "username",
>                 label << "Password:",  password "password",
>                 submit "" "Log In", stringToHtml " ", submit "join" "Join" ]

For logged-in members, we provide a simple logout button (with an indicator of
your username to show that you're logged in).

> logoutBox :: String -> Html
> logoutBox username = form ! [  theclass "auth-box logout", action "/member/logout",
>                                method "post"] <<
>                        [  stringToHtml username, submit "" "Log Out" ]

Students with a goal in mind will want to search for words they're studying
rather than browse randomly. We display a search box for them at the top of the
page. This also is currently the only way to create new links (aside from
entering in the URL manually), but that might change in the future.

> searchBox :: Html
> searchBox = form ! [theclass "search-box", action "/search", method "get"] <<
>   [ textfield "q", submit "" "Search" ]

We display the number of links that are waiting for review for logged in
members in the standard page header. Reviewing is currently the primary
function of Vocabulink, and we want it prominently displayed.

The idea is that a member will go to the site, and we want them to be instantly
reminded that they have links to review. Or, if a link for review becomes due
while they are browsing another part of the site, we want them to be notified.

> reviewBox :: App Html
> reviewBox = withMemberNumber noHtml $ \memberNo -> do
>   n <- numLinksToReview memberNo
>   return $ case n of
>             0  -> anchor !  [href "/links", theclass "review-box"] <<
>                               "No links to review"
>             n' -> anchor !  [href "/review/next", theclass "review-box"] <<
>                               [(strong << show n') +++ " links to review" ]

Not everything on Vocabulink is dynamic. We don't want to include long text
(something that markup is good at) in our source code. To display a static
file, we simply read it into memory, convert it into an Html primative, and
add it to our standard page.

The static file must be be a valid fragment of XHTML.

> displayStaticFile :: String -> FilePath -> App CGIResult
> displayStaticFile t path = do
>   bodyHtml <- liftIO $ readFile path
>   stdPage t [] [primHtml bodyHtml]

\subsection{Higher-Level Combinators}

It's common to use an unordered list to present a series of links. For example,
both the standard header and footer use this.

> linkList :: (HTML a) => [a] -> Html
> linkList items = ulist ! [theclass "links"] << map (li <<) items

Sometimes you just want a select list where the displayed options match their values.

> options :: [String] -> [Html]
> options choices = [ option ! [value choice] << choice | choice <- choices ]

Curiously, the accesskey attribute is missing from Text.XHtml.

> accesskey :: String -> HtmlAttr
> accesskey = strAttr "accesskey"

Sometimes we may be dynamically generating identifiers/names. An example of
this is: when creating a new link, we add a (hidden) form for each different
link type. This helps keep us out of trouble.

> safeID :: String -> String
> safeID = translate [(' ', '-')]

\subsection{Paging}

We'd like to have a consistent way of ``paging'' lists that don't fit on a
single page. This could be for search results, a set of links, articles, etc.

This reads the page query parameters and returns them along with the current
offset (as a convenience).

A reasonable default is 10 items per page. We also don't want to chew up
resources retrieving too many items, so we cap the max at 100.

> currentPage :: App (Int, Int, Int)
> currentPage = do
>   pg  <- readInputDefault 1 "pg"
>   n'  <- readInputDefault 10 "n"
>   let  n''     = n' > 100 ? 100 $ n'
>        n       = n'' < 1 ? 1 $ n''
>        offset  = (pg - 1) * n
>   return (pg, n, offset)

This will handle the query string in the links it generates while it replaces
the @n@ (number of items per page) and @page@ (the page we're on) parameters.
We give it the page we're currently on, the number of items per page, and the
total number of items available, and it does the rest.

This doesn't actually display clickable numeric links such as you'd see on a
Google search results page. It only provides the client with ``previous'' and
``next''. This is for faster database queries.

> pager :: Int -> Int -> Int -> App Html
> pager pg n total = do
>   q'   <- getVar "QUERY_STRING"
>   uri  <- requestURI
>   let  path  = uriPath uri
>        q     = maybe "" decodeString q'
>        prev  = pageQueryString n (pg - 1) q
>        next  = pageQueryString n (pg + 1) q
>   return $ paragraph ! [theclass "pager"] << thespan ! [theclass "controls"] <<
>     [  (pg > 1 ? anchor ! [href (path ++ prev), theclass "prev"] $
>         thespan ! [theclass "prev"]) << "Previous", stringToHtml " ",
>        ((pg * n < total) ? anchor ! [href (path ++ next), theclass "next"] $
>         thespan ! [theclass "next"]) << "Next" ]

Creating the query string involves keeping the existing query string intact as
much as possible. We even want the position of the parameters to stay the same
if they're already there.

> pageQueryString :: Int -> Int -> String -> String
> pageQueryString n pg q  =
>   let q1  = q   =~ nRegex   ?  subRegex (mkRegex nRegex)   q   ("n=" ++ show n) $
>                                q   ++ ("&n=" ++ show n)
>       q2  = q1  =~ pgRegex  ?  subRegex (mkRegex pgRegex)  q1  ("pg=" ++ show pg) $
>                                q1  ++ ("&pg=" ++ show pg) in
>   "?" ++ q2
>     where nRegex   = "n=[^&]+"
>           pgRegex  = "pg=[^&]+"