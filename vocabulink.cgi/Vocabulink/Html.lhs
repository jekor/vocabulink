\section{Html}

Much of Vocabulink consists of simple, program-generated HTML. Rather than use
templates or HTML in strings, we use an HTML combinator library
(Text.XHtml.Strict). This makes it almost certain that our HTML will be well
formed (although we have guarantee that it will be valid). But more
importantly, it allows us to use abstraction to get higher-level HTML-based
functions. An example of this is |linkList|.

> module Vocabulink.Html (  Dependency(..), stdPage, simplePage, displayStaticFile,
>                           linkList, options, accesskey,
>                           runForm, formLabel, formLabel',
>                           pager, currentPage,
>  {- Text.XHtml.Strict -}  Html, noHtml, primHtml, stringToHtml, concatHtml,
>                           (<<), (+++), (!),
>                           identifier, theclass, thediv, thespan, style,
>                           paragraph, pre, h1, h2, h3, br, anchor, href,
>                           image, unordList, form, action, method, fieldset,
>                           hidden, label, textfield, password, button, submit,
>                           textarea, select, widget,
>                           thestyle, src, width, height, value, name,
>                           cols, rows,
>                           table, thead, tbody, tfoot, th, tr, td,
>  {- Text.Formlets -}      AppForm, runFormState, nothingIfNull,
>                           check, ensure, ensures, checkM, ensureM,
>                           plug,
>  {- Text.XHtml.Strict.Formlets -} XHtmlForm) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Review.Html
> import Vocabulink.Utils

Currently Text.XHtml does not automatically handle UTF-8 output. We have to
carefully encode everything we need to. If we don't, non-ASCII (non-iso8859-1?)
characters will be converted to entities. This automatic conversion may be a
nice fallback, but it can mask an underlying problem.

> import Control.Applicative.Error
> import Control.Arrow (second)
> import Network.URI (uriPath)
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.Formlets (  runFormState, plug, nothingIfNull,
>                         check, ensure, ensures, checkM, ensureM)
> import Text.XHtml.Strict
> import Text.XHtml.Strict.Formlets (XHtmlForm)

Most pages depend on some external CSS and/or JavaScript files.

> data Dependency = CSS String | JS String

|stdPage| takes a title, a list of dependencies, and list of HTML objects to
place into the body of the page. It automatically adds a standard header and
footer. It also includes @page.css@.

|stdPage| expects title to already be encoded as UTF-8.

> stdPage :: String -> [Dependency] -> [Html] -> [Html] -> App CGIResult
> stdPage t deps head' body' = do
>   headerB  <- headerBar
>   footerB  <- footerBar
>   output $ renderHtml $ header <<
>     (  thetitle << (encodeString t) +++
>        concatHtml (map includeDep ([CSS "page"] ++ deps)) +++
>        concatHtml head') +++
>     body << (headerB +++ concatHtml body' +++ footerB)

Often we want a simple page where the title and header are the same.

> simplePage :: String -> [Dependency] -> [Html] -> App CGIResult
> simplePage t deps h = stdPage t deps [] $ [ h1 << t ] ++ h

Each dependency is expressed as the path from the root of the static subdomain
(for now, @s.vocabulink.com@) to the file. Do not include the file suffix
(@.css@ or @.js@), it will be appended automatically. These are meant for
inclusion in the @<head>@ of the page.

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
>     [  anchor ! [theclass "logo", href "/", accesskey "1"] << "Vocabulink",
>        topLinks,
>        maybe loginBox logoutBox username,
>        searchBox,
>        review,
>        thediv ! [theclass "clear"] << noHtml ]

Here are the links we want in the header of every page.

> topLinks :: Html
> topLinks = linkList
>   [  anchor ! [href "/article/how-to-get-started-with-vocabulink"] <<
>        "Get Started",
>      anchor ! [href "/articles/"] << "Articles",
>      anchor ! [href "/help"] << "Help" ]

The footer bar is more simple. It just includes some links to static content.

> footerBar :: App Html
> footerBar = do
>   copy <- copyrightNotice
>   return $ thediv ! [identifier "footer-bar"] <<
>     [  linkList
>        [  anchor ! [href "/help"] << "help",
>           anchor ! [href "/privacy"] << "privacy policy",
>           anchor ! [href "/copyrights"] << "copyright policy",
>           anchor ! [href "/disclaimer"] << "disclaimer"],
>        copy ]

We want a copyright notice at the bottom of every page. Since this is a
copyright notice for dynamic content, we want it to be up-to-date with the
generation time (now).

> copyrightNotice :: App Html
> copyrightNotice = do
>   year <- liftIO currentYear
>   return $ paragraph ! [theclass "copyright"] <<
>     [  stringToHtml $ encodeString "© 2008–",
>        stringToHtml ((show year) ++ " "),
>        anchor ! [href "http://jekor.com/"] << "Chris Forno" ]

This provides a simple login box for members. Actually, it's a box for a
username, a box for a password, a login button, and a join button.

We have to put the join button before the login form to get them to display in
the correct order because they're both floated to the right.

Until we switch this over to using formlets, we need to manually set the inputs
to @input0@ and @input1@ since that's what the formlets-based login page
expects.

> loginBox :: Html
> loginBox = form ! [  theclass "auth-box", action "/member/join",
>                      method "GET"] <<
>              [  submit "" "Join" ] +++
>            form ! [  theclass "auth-box login", action "/member/login",
>                      method "POST"] <<
>              [  label << "Username:",  textfield "input0",
>                 label << "Password:",  password "input1",
>                 submit "" "Log In" ]

For logged-in members, we provide a simple logout button (with an indicator of
your username to show that you're logged in).

> logoutBox :: String -> Html
> logoutBox username = form ! [  theclass "auth-box logout", action "/member/logout",
>                                method "POST"] <<
>                        [  stringToHtml username, submit "" "Log Out" ]

Students with a goal in mind will want to search for words they're studying
rather than browse randomly. We display a search box for them at the top of the
page. This also is currently the only way to create new links (aside from
entering in the URL manually), but that might change in the future.

> searchBox :: Html
> searchBox = form ! [theclass "search-box", action "/links", method "GET"] <<
>   [ textfield "contains" ! [accesskey "s"], submit "" "Search" ]

Not everything on Vocabulink is dynamic. We don't want to include long text
(something that markup is good at) in our source code. To display a static
file, we simply read it into memory, convert it into an Html primative, and
add it to our standard page.

The static file must be be a valid fragment of XHTML.

For now, our static files actually resemble articles, so we'll contain them as
such.

> displayStaticFile :: String -> FilePath -> App CGIResult
> displayStaticFile t path = do
>   bodyHtml <- liftIO $ readFile path
>   stdPage t [CSS "article"] [] [thediv ! [theclass "article"] << primHtml bodyHtml]

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

\subsection{Formlet Helpers}

> type AppForm a = XHtmlForm (AppT IO) a

We ofter want to "wrap" a label around a form component. This doesn't currently
set a @for@ attribute.

> formLabel :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel text = plug (\xhtml -> paragraph << (label << (text ++ ": ") +++ xhtml))

Here's an alterate version of the above which doesn't wrap the form with a
paragraph.

> formLabel' :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel' text = plug (\xhtml -> label << (text ++ ": ") +++ xhtml)

Take a form and a submit button label, run it, and return either the form to
display (with errors, if any) or the result of the form.

``Running'' the form involves taking the form inputs from the ``environment''
(the CGI input variables) and ``passing'' them to the form. The form then
attempts to validate against the environment. If it fails, it returns a form
(as Html) to display to the client, but if it succeeds it returns a value of
the type of the form.

> runForm :: XHtmlForm (AppT IO) a -> String -> App (Either Html a)
> runForm frm message = do
>   env <- map (second Left) <$> getInputs
>   let (res, markup, _) = runFormState env "" frm
>   status  <- res
>   xhtml   <- markup
>   meth    <- requestMethod
>   case status of
>     Failure failures  -> do
>       uri <- requestURI
>       return $ Left $ errors +++ form ! [action (uriPath uri), method "POST"] <<
>                                     [  xhtml, submit "" message ]
>      where errors = case meth of
>                       "GET"  -> noHtml
>                       _      -> unordList failures
>     Success result    -> return $ Right result

\subsection{Paging}

We'd like to have a consistent way of ``paging'' lists that don't fit on a
single page. This could be for search results, a set of links, articles, etc.

This reads the page query parameters and returns them along with the current
offset (as a convenience).

A reasonable default is 10 items per page. We also don't want to chew up
resources retrieving too many items, so we cap the max at 100.

Limiting the paging elements to Int bounds is necessary for the functions that
use the pager (they often need to |take| some number of tuples from a list, for
instance) and does not limit the design of our HTTP interface much, if at all.
I cannot think of an instance where we'd need to go past the 65,000th page
unless we were paging through every link in the system.

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