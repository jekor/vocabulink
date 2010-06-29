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

> module Vocabulink.Html (  stdPage, simplePage, clear,
>                           linkList, breadcrumbs, options, menu, tableRows,
>                           accesskey, readonly, helpButton, markdownToHtml,
>                           AppForm, runForm, runForm', formLabel, formLabel',
>                           checkbox', tabularInput, tabularSubmit,
>                           nonEmptyAndLessThan, noscript,
>                           pager, currentPage, fileUpload, uploadFile, forID,
>                           invitationLink, loggedInVerifiedButton,
>                           multiColumn, multiColumnList,
>  {- Text.XHtml.Strict -}  Html, noHtml, primHtml, stringToHtml, concatHtml,
>                           (<<), (+++), (!), showHtmlFragment,
>                           identifier, theclass, thediv, thespan, style,
>                           paragraph, pre, h1, h2, h3, br, anchor, href, script,
>                           image, title, ulist, li, unordList,
>                           form, action, method, enctype,
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
> import Vocabulink.Article
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Review.Html
> import Vocabulink.Utils

> import qualified Data.ByteString.Lazy as BS
> import Data.List (intersperse)
> import Text.Formlets (  runFormState, plug, nothingIfNull, optionalInput,
>                         check, ensure, ensures, checkM, ensureM, File)
> import Text.Pandoc (  readMarkdown, writeHtml, defaultParserState,
>                       defaultWriterOptions, stateSanitizeHTML )
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.XHtml.Strict hiding (content, menu)
> import qualified Text.XHtml.Strict.Formlets as XF (input)
> import Text.XHtml.Strict.Formlets (XHtmlForm)

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
>   setHeader "Content-Type" "text/html; charset=utf-8"
>   headerB  <- headerBar
>   footerB  <- footerBar
>   memberEmail <- asks appMemberEmail
>   let  deps' = deps ++ [CSS "lib.common", JS "lib.common"] ++ (isJust memberEmail ? [CSS "lib.member", JS "lib.member"] $ [])
>        (cssDeps, jsDeps) = partition (\x -> case x of
>                                               (CSS _) -> True
>                                               (JS  _) -> False) deps'
>   cssDeps'  <- mapM includeDep cssDeps
>   jsDeps'   <- mapM includeDep jsDeps
>   let  xhtml = renderHtml $ header <<
>                  [  concatHtml cssDeps',
>                     thetitle << title',
>                     thelink ! [rel "icon", thetype "image/png",
>                                href "http://s.vocabulink.com/img/favicon.png"] << noHtml,
>                     concatHtml head' ] +++
>                  body << [  script ! [src "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"] << noHtml,
>                             script ! [src "http://www.google-analytics.com/ga.js"] << noHtml,
>                             concatHtml jsDeps',
>                             thediv ! [identifier "head"] << headerB,
>                             case jsDeps of
>                               []  -> noHtml
>                               _   -> noscript << paragraph <<
>                                        "This page requires JavaScript for some functionality.",
>                             thediv ! [identifier "body"] << concatHtml body',
>                             thediv ! [identifier "foot"] << footerB ]
>   output' xhtml

Often we just need a simple page where the title and header are the same.

> simplePage :: String -> [Dependency] -> [Html] -> App CGIResult
> simplePage t deps = stdPage t deps [] . ((h1 << t) :)

It's common to add a little hyperlink teaser for actions that require
verification. For example "login to reply" or "verify email to reply".

> invitationLink :: String -> App Html
> invitationLink text = do
>   memberNo <- asks appMemberNo
>   memberEmail <- asks appMemberEmail
>   case (memberNo, memberEmail) of
>     (Just _,  Just _)  -> return noHtml
>     (Just _,  _)       -> do
>       url <- reversibleRedirect "/member/confirmation"
>       return $ anchor ! [theclass "invitation", href url] <<
>         ("Verify Email to " ++ text)
>     (_     ,  _)       -> do
>       url <- reversibleRedirect "/member/login"
>       return $ anchor ! [theclass "invitation", href url] <<
>         ("Login to " ++ text)

In other cases, we're going to place an action button that should redirect when
the client is not a verified member.

> loggedInVerifiedButton :: String -> App Html
> loggedInVerifiedButton text = do
>   confirm  <- reversibleRedirect "/member/confirmation"
>   login    <- reversibleRedirect "/member/login"
>   url <- loggedInVerified "#" confirm login
>   return $ anchor ! [theclass "button", href url] << text

Each dependency is expressed as the path from the root of the static files
subdomain (for now, @s.vocabulink.com@) to the file. Do not include the file
suffix (@.css@ or @.js@); it will be appended automatically. These are meant
for inclusion in the @<head>@ of the page.

|includeDep| also needs to check dependency versions for cache busting.

> includeDep :: Dependency -> App Html
> includeDep d = do
>   version <- dependencyVersion d
>   return $ case version of
>     Nothing  -> script ! [thetype "text/javascript"] << primHtml
>                   ("alert('Dependency \"" ++ show d ++ "\" not found.');")
>     Just v   ->
>       case d of
>         CSS  css  -> thelink ! [  href ("http://s.vocabulink.com/css/" ++ css ++ ".css?" ++ v),
>                                   rel "stylesheet", thetype "text/css" ] << noHtml
>         JS   js   -> script ! [src ("http://s.vocabulink.com/js/" ++ js ++ ".js?" ++ v)]
>                        << noHtml

The standard header bar shows the Vocabulink logo (currently just some text), a
list of hyperlinks, a search box, and either a login/sign up button or a logout
button. If the page is being served to a logged-in member it also includes a
notice about the number of links that the member has waiting for review.

> headerBar :: App Html
> headerBar = do
>   username <- asks appMemberName
>   review <- reviewBox
>   return $ concatHtml [
>     anchor ! [href "/", accesskey "1"] <<
>       (image ! [  theclass "logo",
>                   alt "Vocabulink: Learn Languages through Fiction",
>                   src "http://s.vocabulink.com/img/logo-compact.png" ]),
>     thediv ! [identifier "head-decoration"] << noHtml,
>     thediv ! [identifier "head-bar"] << [
>       searchBox,
>       review,
>       maybe loginBox logoutBox username,
>       clear ],
>     clear ]

The footer displays a number of common (or what we believe to be common)
hyperlinks for English speakers.

> languageLinks :: App [Html]
> languageLinks = do
>   ts <- queryTuples'  "SELECT abbr, name \
>                       \FROM language_frequency_to_english \
>                       \ORDER BY freq DESC LIMIT 7" []
>   return $ case ts of
>     Nothing   -> []
>     Just ts'  -> map languageLink ts' ++ [anchor ! [href "/languages"] << "more..."]
>       where languageLink [abbr', name']  =
>               anchor ! [href ("/links?ol=" ++ (fromSql abbr') ++ "&dl=en")] << ((fromSql name')::String)
>             languageLink _             = error "Malformed tuple."

> footerBar :: App Html
> footerBar = do
>   langLinks <- languageLinks
>   articles <- maybe [] (map articleLinkHtml . take 3) <$> getArticles
>   forums <- footerForums
>   copy <- liftIO $ copyrightNotice
>   return $ concatHtml [
>     multiColumn [
>       h2 << "Browse links by language:" +++
>       multiColumnList 2 langLinks,
>       h2 << "Latest Articles:" +++
>       unordList (articles ++ [anchor ! [href "/articles"] << "more..."]),
>       h2 << "Forums:" +++
>       multiColumnList 2 (forums ++ [anchor ! [href "/forums"] << "more..."]) ] ! [identifier "handy-links"],
>     linkList [
>       anchor ! [href "/help"] << "help",
>       anchor ! [href "/privacy"] << "privacy policy",
>       anchor ! [href "/terms-of-use"] << "terms of use",
>       anchor ! [href "/source"] << "source" ],
>     paragraph << [
>       copy,
>       thespan ! [identifier "design-attribution"] << [
>         stringToHtml "Design by: ",
>         anchor ! [href "http://www.designcharisma.com"] << "Design Charisma" ] ] ]
>  where articleLinkHtml a =
>          anchor ! [href ("/article/" ++ articleFilename a)] <<
>            articleTitle a

> footerForums :: App [Html]
> footerForums = do
>   ts <- queryTuples'  "SELECT name, title FROM forum \
>                       \WHERE group_name = 'Vocabulink'" []
>   return $ case ts of
>     Nothing   -> []
>     Just ts'  -> map forumLink ts'
>       where forumLink [n', t']  = anchor ! [href ("/forum/" ++ fromSql n')] << ((fromSql t')::String)
>             forumLink _         = error "Malformed forum."

We want a copyright notice at the bottom of every page. Since this is a
copyright notice for dynamic content, we want it to be up-to-date with the
generation time (now).

> copyrightNotice :: IO Html
> copyrightNotice = do
>   year <- currentYear
>   return $ thespan ! [theclass "copyright"] <<
>     [  stringToHtml "Copyright 2008–",
>        stringToHtml (show year ++ " "),
>        anchor ! [href "http://jekor.com/"] << "Chris Forno" ]

The following are just login and signup buttons.

> loginBox :: Html
> loginBox = thespan ! [theclass "auth-box login"] << [
>   anchor ! [identifier "login-button"] << "Log in", stringToHtml " | ",
>   anchor ! [href "/member/signup"] << "Sign up" ]

For logged-in members, we provide a logout button (with an indicator of
your username to show that you're logged in).

> logoutBox :: String -> Html
> logoutBox username = form ! [  theclass "auth-box logout", action "/member/logout",
>                                method "post"] <<
>                        [  stringToHtml username, submit "" "Log Out" ]

Students with a goal in mind will want to search for words they're studying
rather than browse randomly. We display a search box for them at the top of the
page. This also is currently the only way to create new links (aside from
entering in the URL manually), but that might change in the future.

Currently, this uses a combination of our search logic and Google Custom Search.

> searchBox :: Html
> searchBox = form ! [identifier "cse-search-box", theclass "search-box", action "/search"] <<
>   thediv << [
>     hidden "cx"   "011479832181784786223:2ibwsl9f6ca",
>     hidden "cof"  "FORID:9",
>     hidden "ie"   "UTF-8",
>     textfield "q" ! [accesskey "s"], stringToHtml " ",
>     submit "sa" "Search" ] +++
>   script ! [src "http://www.google.com/cse/brand?form=cse-search-box&lang=en"] << noHtml

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
> helpButton url label' = anchor ! [href url, theclass "help-button"] << [
>                           image ! [src "http://s.vocabulink.com/icon_info.gif"],
>                           maybe noHtml (stringToHtml . (' ' :)) label' ]

\subsection{Other Markup}

A modified version of Markdown (Pandoc Markdown) is used in comments and link
bodies. We need to sanitize incoming HTML so that we don't end up with XSS
attacks.

> markdownToHtml :: String -> Html
> markdownToHtml = writeHtml defaultWriterOptions . readMarkdown defaultParserState {stateSanitizeHTML = True}

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
>       return $ Left $ form ! [action (uriPath uri), method "post"] <<
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

> fileUpload :: String -> AppForm String
> fileUpload l = plug (\xhtml -> concatHtml [
>   xhtml ! [theclass "upload-file", thestyle "width: 50%", readonly],
>   button ! [  identifier "upload-file-button", disabled,
>               thestyle "text-align: center" ] << l ])
>     (XF.input Nothing)

For now, you're responsible for providing a safe FilePath to upload to.

TODO: Do a check right before writeFile to ensure a safe path.

> uploadFile :: FilePath -> App CGIResult
> uploadFile path = do
>   dir <- (</> "upload" </> path) <$> asks appDir
>   filename <- getInputFilename "userfile"
>   case filename of
>     Nothing  -> error "Missing filename."
>     Just f   -> do
>       content' <- fromJust <$> getInputFPS "userfile"
>       let f'    = "/tmp." ++ urlify (basename f)
>           file  = dir ++ f'
>       liftIO $ BS.writeFile file content'
>       output' $ path ++ f'

> multiColumn :: [Html] -> Html
> multiColumn cls =
>   let num = case length cls of
>               1 -> "one"
>               2 -> "two"
>               3 -> "three"
>               _ -> "unsupported" in
>   thediv ! [theclass (num ++ "-column")] <<
>     (map (thediv ! [theclass "column"] <<) cls ++ [clear])

> multiColumnList :: Int -> [Html] -> Html
> multiColumnList 2 xs  =
>   let (col1, col2) = every2nd xs in
>   multiColumn [unordList col1, unordList col2]
> multiColumnList 3 xs  =
>   let (col1, col2, col3) = every3rd xs in
>   multiColumn [unordList col1, unordList col2, unordList col3]
> multiColumnList _ _   = stringToHtml "Unsupported number of columns."
