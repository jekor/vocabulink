% Copyright 2008, 2009 Chris Forno

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

> module Vocabulink.Html (  Dependency(..), stdPage, simplePage, clear,
>                           linkList, breadcrumbs, options, menu, tableRows,
>                           accesskey, readonly, helpButton, markdownToHtml,
>                           AppForm, runForm, runForm', formLabel, formLabel',
>                           checkbox', tabularInput, tabularSubmit,
>                           nonEmptyAndLessThan,
>                           pager, currentPage, fileUpload, uploadFile, forID,
>  {- Text.XHtml.Strict -}  Html, noHtml, primHtml, stringToHtml, concatHtml,
>                           (<<), (+++), (!), showHtmlFragment,
>                           identifier, theclass, thediv, thespan, style,
>                           paragraph, pre, h1, h2, h3, br, anchor, href, script,
>                           image, unordList, form, action, method, enctype,
>                           hidden, label, textfield, password, button, submit,
>                           fieldset, legend, afile, textarea, select, widget,
>                           thestyle, src, width, height, value, name, thetype,
>                           cols, rows, colspan, caption, disabled, alt,
>                           table, thead, tbody, tfoot, th, tr, td, hr,
>                           emphasize, strong,
>  {- Text.Formlets -}      runFormState, nothingIfNull,
>                           check, ensure, ensures, checkM, ensureM,
>                           plug, File,
>  {- Text.XHtml.Strict.Formlets -} XHtmlForm) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Review.Html
> import Vocabulink.Utils

> import qualified Data.ByteString.Lazy as BS
> import Data.List (intersperse, find)
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.Formlets (  runFormState, plug, nothingIfNull,
>                         check, ensure, ensures, checkM, ensureM)
> import Text.Pandoc (  readMarkdown, writeHtml, defaultParserState,
>                       defaultWriterOptions )
> import Text.Formlets as F
> import Text.Formlets (File)
> import Text.XHtml.Strict hiding (content, menu)
> import qualified Text.XHtml.Strict.Formlets as XF (input)
> import Text.XHtml.Strict.Formlets (XHtmlForm)

Most pages depend on some external CSS and/or JavaScript files.

> data Dependency = CSS String | JS String
>                   deriving Eq

We want to allow the client browser to cache CSS and JavaScript for as long as
possible, but we want to bust the cache when we update them. We can get the
best of both worlds by setting large expiration times and by using version
numbers.

To do this, we'll add a version number to each static file as a query string.
nginx will ignore this and serve the same file, but the client browser
\textit{should} ignore it.

Any file not part of this list will be assumed to be version 1. It would be
nice to get this working automatically via Darcs at some point.

> dependencyVersions :: [(Dependency, Integer)]
> dependencyVersions = [  (JS "raphael", 2),
>                         (JS "comment", 4),
>                         (JS "link", 3),
>                         (JS "member", 2),
>                         (JS "form", 2),
>                         (JS "forum", 2),
>                         (JS "link", 2),
>                         (JS "link-graph", 2),
>                         (JS "review", 2),
>                         (CSS "forum", 5),
>                         (CSS "link", 4),
>                         (CSS "page", 4),
>                         (CSS "article", 2) ]

> dependencyVersion :: Dependency -> Integer
> dependencyVersion = fromMaybe 1 . flip lookup dependencyVersions

|stdPage| takes a title, a list of dependencies, and list of HTML objects to
place into the body of the page. It automatically adds a standard header and
footer. It also includes @page.css@ and conditionally includes an Internet
Explorer-specific stylesheet for the few cases when there's no other way to
work around a defect in Internet Explorer that would otherwise seriously impact
usability.

If any JavaScript files are required, |stdPage| will automatically add a
@<noscript>@ warning to the top of the page.

> stdPage :: String -> [Dependency] -> [Html] -> [Html] -> App CGIResult
> stdPage title' deps head' body' = do
>   headerB  <- headerBar
>   footerB  <- footerBar
>   setHeader "Content-Type" "text/html; charset=utf-8"
>   memberNo <- asks appMemberNo
>   let  deps' = (CSS "page":JS "functional.min":JS "jquery.corner":deps) ++ (isJust memberNo ? [JS "member"] $ [])
>        (cssDeps, jsDeps) = partition (\x -> case x of
>                                               (CSS _) -> True
>                                               (JS  _) -> False) deps'
>        xhtml = renderHtml $ header <<
>                  [  thetitle << title',
>                     concatHtml $ map includeDep cssDeps,
>                     primHtml  "<!--[if IE]>\
>                               \<link rel=\"stylesheet\" type=\"text/css\" \
>                               \href=\"http://s.vocabulink.com/css/ie.css\" />\
>                               \<![endif]-->",
>                     concatHtml head' ] +++
>                  body << [  script ! [  thetype "text/javascript",
>                                         src "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js" ] << noHtml,
>                             concatHtml $ map includeDep jsDeps,
>                             script ! [  thetype "text/javascript"] << "Functional.install(); $(document).ready(function() {$('div.sidebox').corner();});",
>                             headerB,
>                             jsNotice,
>                             thediv ! [identifier "body"] << concatHtml body',
>                             footerB ]
>   output' xhtml
>  where jsNotice = case find (\e -> case e of
>                                      JS _  -> True
>                                      _     -> False) deps of
>                     Nothing  -> noHtml
>                     Just _   -> noscript << paragraph <<
>                       "This page requires JavaScript for some functionality."

Often we just need a simple page where the title and header are the same.

> simplePage :: String -> [Dependency] -> [Html] -> App CGIResult
> simplePage t deps h = stdPage t deps [] $ (h1 << t) : h

Each dependency is expressed as the path from the root of the static files
subdomain (for now, @s.vocabulink.com@) to the file. Do not include the file
suffix (@.css@ or @.js@); it will be appended automatically. These are meant
for inclusion in the @<head>@ of the page.

|includeDep| also needs to check dependency versions for cache busting.

> includeDep :: Dependency -> Html
> includeDep d =
>   let v = '?' : show (dependencyVersion d) in
>   case d of
>     CSS  css  -> thelink ! [  href ("http://s.vocabulink.com/css/" ++ css ++ ".css" ++ v),
>                               rel "stylesheet", thetype "text/css"] << noHtml
>     JS   js   -> script ! [  src ("http://s.vocabulink.com/js/" ++ js ++ ".js" ++ v),
>                              thetype "text/javascript"] << noHtml

The standard header bar shows the Vocabulink logo (currently just some text), a
list of hyperlinks, a search box, and either a login/sign up button or a logout
button. If the page is being served to a logged-in member it also includes a
notice about the number of links that the member has waiting for review.

> headerBar :: App Html
> headerBar = do
>   username <- asks appMemberName
>   review <- reviewBox
>   return $ thediv ! [identifier "header-bar"] <<
>     [  anchor ! [theclass "logo", href "/", accesskey "1"] << [
>          stringToHtml "Vocabulink", br,
>          thespan ! [theclass "tagline"] << "learn languages through fiction" ],
>        topLinks,
>        maybe loginBox logoutBox username,
>        searchBox,
>        review,
>        clear ]

Here are the hyperlinks we want in the header of every page.

> topLinks :: Html
> topLinks = linkList
>   [  anchor ! [href "/forums"] << "Forums", stringToHtml "|",
>      anchor ! [href "/articles"] << "Articles", stringToHtml "|",
>      anchor ! [href "/languages"] << "Browse Links", stringToHtml "|",
>      anchor ! [href "/help"] << "Help" ]

The footer bar is more simple. It just includes some hyperlinks to static
content.

> footerBar :: App Html
> footerBar = do
>   copy <- copyrightNotice
>   return $ thediv ! [identifier "footer-bar"] <<
>     [  linkList
>        [  anchor ! [href "/help"] << "help",
>           anchor ! [href "/privacy"] << "privacy policy",
>           anchor ! [href "/terms-of-use"] << "terms of use",
>           anchor ! [href "/source"] << "source"],
>        copy,
>        googleAnalyticsTag ]

We want a copyright notice at the bottom of every page. Since this is a
copyright notice for dynamic content, we want it to be up-to-date with the
generation time (now).

> copyrightNotice :: App Html
> copyrightNotice = do
>   year <- liftIO currentYear
>   return $ paragraph ! [theclass "copyright"] <<
>     [  stringToHtml "Copyright 2008–",
>        stringToHtml (show year ++ " "),
>        anchor ! [href "http://jekor.com/"] << "Chris Forno" ]

We use Google Analytics for tracking site usage. It requires the JavaScript tag
to be placed on every page.

> googleAnalyticsTag :: Html
> googleAnalyticsTag = primHtml $ unlines [
>   "<script type=\"text/javascript\">",
>   "var gaJsHost = ((\"https:\" == document.location.protocol) ?\
>                  \ \"https://ssl.\" : \"http://www.\");",
>   "document.write(unescape(\"%3Cscript src='\" + gaJsHost \
>   \+ \"google-analytics.com/ga.js' \
>   \type='text/javascript'%3E%3C/script%3E\"));",
>   "</script>",
>   "<script type=\"text/javascript\">",
>   "try {",
>   "var pageTracker = _gat._getTracker(\"UA-73938-2\");",
>   "pageTracker._trackPageview();",
>   "} catch(err) {}</script>" ]

The following are just login and signup buttons.

> loginBox :: Html
> loginBox = thespan ! [theclass "auth-box login"] << [
>   anchor ! [href "/member/login"] << "Login", stringToHtml " | ",
>   anchor ! [href "/member/signup"] << "Sign Up" ]

For logged-in members, we provide a logout button (with an indicator of
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
>   [ textfield "contains" ! [accesskey "s"], stringToHtml " ",
>     submit "" "Search Links" ]

\subsection{Higher-Level Combinators}

It's common to use an unordered list to present a series of hyperlinks. For
example, both the standard header and footer use this.

> linkList :: (HTML a) => [a] -> Html
> linkList items = ulist ! [theclass "hyperlinks"] << map (li <<) items

Breadcrumbs are a common navigation element. This only handles wrapping the
provided elements in an appropriate ordered list and adding decorations. Adding
the anchors is up to you.

> breadcrumbs :: [Html] -> Html
> breadcrumbs items = ulist ! [theclass "breadcrumbs"] << map (li <<) items'
>   where items' = intersperse (stringToHtml " » ") items

Sometimes you just want a select list where the displayed options match their values.

> options :: [String] -> [Html]
> options choices = [ option ! [value choice] << choice | choice <- choices ]

> menu :: String -> [(String, String)] -> Html
> menu n choices = select ! [name n] <<
>   [ option ! [value $ fst choice] << snd choice | choice <- choices ]

This automatically adds ``odd'' and ``even'' CSS classes to each table row.

> tableRows :: [Html] -> [Html]
> tableRows = map decorate . zip [1..]
>   where decorate (a,b) = tr ! [theclass (odd (a :: Integer) ? "odd" $ "even")] << b

\subsection{Tiny Helpers}

> clear :: Html
> clear = thediv ! [theclass "clear"] << noHtml

Some attributes are missing from Text.XHtml.

> accesskey :: String -> HtmlAttr
> accesskey = strAttr "accesskey"

> readonly :: HtmlAttr
> readonly = emptyAttr "readonly"

> forID :: String -> HtmlAttr
> forID = strAttr "for"

It's nice to have little help buttons and such where necessary. Making them
easier to create means that we're more likely to do so, which leads to a more
helpful user interface.

Currently this uses an icon from the
\href{http://www.famfamfam.com/lab/icons/mini/}{FamFamFam ``Mini''} set.

> helpButton :: String -> Maybe String -> Html
> helpButton url label' = anchor ! [href url, theclass "button"] << [
>                           image ! [src "http://s.vocabulink.com/icon_info.gif"],
>                           maybe noHtml (\x -> stringToHtml $ ' ' : x) label' ]

\subsection{Other Markup}

A modified version of Markdown (Pandoc Markdown) is used in comments and link
bodies.

> markdownToHtml :: String -> Html
> markdownToHtml = writeHtml defaultWriterOptions . readMarkdown defaultParserState

\subsection{Form Builders}

For complex forms, we use tables. Tables allow for proper alignment that makes
the form much easier to read. This type of form tends to have a number of
common elements that we can abstract out.

One thing that we're don't currently do is hook the label to the control using
the ``for'' attribute.

> tabularInput :: String -> Html -> Html
> tabularInput l i = tr << [  th << (label << (l ++ ":")),
>                             td << i ]

We want any submit button centered on a row of its own.

> tabularSubmit :: String -> Html
> tabularSubmit l = tr << td ! [colspan 2] << submit "" l

\subsection{Formlet Helpers}

Formlets are a great tool for abstracting and building complex forms. But the
library is still a bit rough around the edges. These helpers are by no means
elegant, but they help get the job done.

All of the formlets we build have this type:

> type AppForm a = XHtmlForm (AppT IO) a

We ofter want to ``wrap'' a label around a form component. Note that this
doesn't currently set a @for@ attribute either.

> formLabel :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel text = plug (\xhtml -> label << (text ++ ": ") +++ xhtml)

Here's an alterate version of the above which also adds a paragraph.

> formLabel' :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel' text = plug (\xhtml -> paragraph << (label << (text ++ ": ") +++ xhtml))

Curiously, the formlets library is missing a checkbox implementation. Thanks to
\href{http://chrisdone.com/blog/html/2008-12-14-haskell-formlets-composable-web-form-construction-and-validation.html}{Chris Done}
for this one.

> checkbox' :: Monad m => String -> XHtmlForm m (Maybe String)
> checkbox' l = optionalInput box where
>   box name' =  input ! [thetype "checkbox", name name'] +++ l

> nonEmptyAndLessThan :: Int -> String -> [(String -> Bool, String)]
> nonEmptyAndLessThan i t =
>   [  ((> 0)  . length, t ++ " must not be empty."),
>      ((< i)  . length, t ++ " must be " ++ show i ++ "characters or shorter") ]

We use |runForm| for most of the heavy lifting. It takes a form and a submit
button label, runs the form, and returns either the form to display (with
errors, if any) or the result of the form.

``Running'' the form involves taking the form inputs from the ``environment''
(the CGI input variables) and ``passing'' them to the form. The form then
attempts to validate against the environment. If it fails, it returns a form
(as Html) to display to the client, but if it succeeds it returns a value of
the type of the form.

|s| is either a label or some custom Html for the submit button (or noHtml if
you don't want a submit button).

> runForm :: XHtmlForm (AppT IO) a -> Either String Html -> App (Either Html a)
> runForm frm s = do
>   (status, xhtml) <- runForm' frm
>   case status of
>     Failure failures  -> do
>       uri   <- requestURI
>       meth  <- requestMethod
>       let submit' = case s of
>                       Left s'  -> submit "" s'
>                       Right h  -> h
>       return $ Left $ form ! [action (uriPath uri), method "POST"] <<
>                         [  (meth == "GET" ? noHtml $ unordList failures),
>                            xhtml, submit' ]
>     Success result    -> return $ Right result

This is a slimmer wrapper around runFormState for when you want to get access
to the errors before they're packed into the returned Html. This is also handy
when implementing ``preview'' functionality for forms.

> runForm' :: XHtmlForm (AppT IO) a -> App (Failing a, Html)
> runForm' frm = do
>   names <- getInputNames
>   env <- zip names . map fromJust <$> mapM getTextOrFileInput names
>   let (res, markup, _) = runFormState env "" frm
>   status  <- res
>   xhtml   <- markup
>   return (status, xhtml)

\subsection{Paging}

We'd like to have a consistent way of ``paging'' lists that don't fit on a
single page. This can be used to page search results, a set of links, articles,
etc.

This reads the page query parameters and returns them along with the current
offset (as a convenience). In the absence of certain parameters, we fall back
to reasonable defaults like 10 items per page. We also don't want to chew up
resources retrieving too many items, so we cap the max that a client can
request at 100.

Limiting the paging elements to Int bounds is necessary for the functions that
use the pager (they often need to |take| some number of tuples from a list, for
instance) and does not limit the design of our HTTP interface much, if at all.
I cannot think of an instance where we'd need to go past the 65,000th page of
anything.

> currentPage :: App (Int, Int, Int)
> currentPage = do
>   pg  <- readInputDefault 1 "pg"
>   n'  <- readInputDefault 10 "n"
>   let  n''     = n' > 100 ? 100 $ n'
>        n       = n'' < 1 ? 1 $ n''
>        offset  = (pg - 1) * n
>   return (pg, n, offset)

This will handle the query string in the hyperlinks it generates while it
replaces the @n@ (number of items per page) and @page@ (the page we're on)
parameters. We give it the page we're currently on, the number of items per
page, and the total number of items available, and it does the rest.

This doesn't actually display clickable numeric hyperlinks such as you'd see on
a Google search results page. It only provides the client with ``previous'' and
``next''. We do this because determining the number of pages in a result can be
expensive.

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
>   '?' : q2
>     where nRegex   = "n=[^&]+"
>           pgRegex  = "pg=[^&]+"

\subsection{File Uploads}

Standard file inputs for forms don't quite fit our needs because they require
the client to upload the file each time they submit the form. When there are
errors or during an iterative preview process this could be an annoyance.

Instead, we'll use some JavaScript to submit the file once in the background
and store its filename in a standard input. This way, once the file has been
uploaded the input will reflect the name of the file on the server side.

> fileUpload :: String -> String -> AppForm String
> fileUpload target l = plug (\xhtml -> concatHtml [
>   script ! [thetype "text/javascript"] << primHtml (unlines [
>     "connect(window, 'onload', function() {",
>       "new AjaxUpload('file-upload', {action: '" ++ target ++ "',",
>                                      "onSubmit: submitFile,",
>                                      "onComplete: fileSubmitted});",
>       "$('file-upload').disabled = false;});",
>     "function submitFile() {setStyle($$('.upload-file')[0], \
>         \{'background': \"url('http://s.vocabulink.com/img/wait-bar.gif') \
>                         \no-repeat center center\"}); \
>       \$('file-upload').disabled = true;",
>     "}",
>     "function fileSubmitted(f,r) {",
>       "var inputFile = $$('.upload-file')[0];",
>       "if (r.substr(0," ++ show (length target) ++ ") == '" ++ target ++ "') {",
>         "inputFile.value = r.substring(" ++ show (length target) ++ " + 1, r.length);",
>         "inputFile.disabled = false;",
>         "inputFile.readOnly = true;",
>       "} else {",
>         "alert('Error uploading file.');",
>       "}",
>       "setStyle(inputFile, {'background': 'none'});",
>       "$('file-upload').disabled = false;}" ]),
>   xhtml ! [theclass "upload-file", thestyle "width: 50%", readonly],
>   button ! [  identifier "file-upload", disabled,
>               thestyle "text-align: center" ] << l ])
>     (XF.input Nothing)

> uploadFile :: String -> App CGIResult
> uploadFile path = do
>   dir <- (++ path) . fromJust <$> getOption "staticdir"
>   filename <- getInputFilename "userfile"
>   case filename of
>     Nothing  -> error "Missing filename."
>     Just f   -> do
>       content' <- fromJust <$> getInputFPS "userfile"
>       let f'    = "/tmp." ++ urlify (basename f)
>           file  = dir ++ f'
>       liftIO $ BS.writeFile file content'
>       output' $ path ++ f'
