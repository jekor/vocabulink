\documentclass[oneside,draft]{article}
%include polycode.fmt
\usepackage[T1]{fontenc}
\usepackage{ucs}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage[pdftex]{graphicx}
\usepackage[x11names, rgb]{xcolor}
\usepackage{tikz}
\usetikzlibrary{decorations,arrows,shapes}
\usepackage[margin=1.4in]{geometry}

\hypersetup{colorlinks=true}

\title{Vocabulink}
\author{Chris Forno (jekor)}
\date{February 24, 2009}

\begin{document}
\maketitle

\section{Introduction}

This is the main Vocabulink program. It is spawned as a FastCGI process and
handles all web requests for vocabulink.com.

Vocabulink.cgi is essentially a multi-user application that operates via the
web. It's structured like a standalone application inasmuch as it handles
multiple requests in a single multi-threaded process. Yet, it's structured like
a CGI program in that it communicates to the outside world through a web
server. It's also designed with the assumption that it may be only 1 of many
processes and that it doesn't have exclusive access to resources such as a
database.

\subsection{Architecture}

Requests arrive via a webserver. They are passed to the vocabulink.fcgi process
(this program) on TCP port 10033.

Upon receiving a request (connection), we immediately fork a new thread. In
this thread, we establish a connection to a PostgreSQL server (for each
request). We then examine the thread for an authentication cookie. If it exists
and is valid, we assume that the request is from an authenticated member. We
pack both the database handle and the authenticated member information into our
``App'' monad (\autoref{App}).

> module Main where

\section{Our Modules}

These are the Vocabulink modules. They are grouped primarily based on division
of labor. The exception is the App module. The App module defines the App monad
and must make use of both database and CGI functions. In order to limit
cyclical dependencies (which can be a pain with the GHC compiler), it's broken
out into a separate module.

> import Vocabulink.App

Each of these modules will be described in its own section.

> import Vocabulink.Article
> import Vocabulink.DB
> import Vocabulink.CGI
> import Vocabulink.Forum
> import Vocabulink.Html hiding (method, options)
> import Vocabulink.Link
> import Vocabulink.Member
> import Vocabulink.Review
> import Vocabulink.Utils
> import Vocabulink.Widget
> import Vocabulink.Widget.MyLinks

\section{Other Modules}

Vocabulink makes use of a half dozen or so Haskell libraries. Even though we
don't use them all in this module, I'll describe them here so that they'll be
more familiar as they're introduced (and so that you can jump directly to the
section you're interested in after this introduction).

\begin{description}

\item[Codec.Binary.UTF8.String] Vocabulink would be pretty useless without
being able to handle the writing systems of other languages. We only make use
of 2 functions provided by this library: |encodeString| and |decodeString|.
|decodeString| takes a UTF-8 string---either from the webserver or from the
database---and converts it into a Unicode string that can be used by Haskell
natively. We use |encodeString| to go in the other direction. Whenever we write
out a string to the database, the webserver, or a log file; it needs to be
encoded to UTF-8. This is something that the type system does not (yet) handle
for us, so we need to be careful to correctly encode and decode strings.

\item[Data.ConfigFile] We need to have some parameters configurable at runtime.
This allows us to do things differently in test and production environments. It
also allows us to publish the source to the program without exposing sensitive
information.

\item[Network.URI] Various parts of the code may need to construct or
deconstruct URLs. Using this library should be safer than using various
string-mangling techniques throughout the code.

\item[Text.ParserCombinators.Parsec] We need to parse text quite a bit. The
dispatcher, the member authentication routines, and the article publishing
system all make use of Parsec; and probably more will in the future.

\end{description}

There are a few more, but they are only used by a single Vocabulink module\footnote{The Vocabulink module may re-export some functions provided by the module, but the other Vocabulink modules should be able to remain ignorant of that.}.

> import Control.Concurrent (forkIO)
> import Control.Monad (join)
> import Control.Monad.Error (runErrorT)
> import Data.ConfigFile (readfile, emptyCP, ConfigParser, CPError, options)
> import Data.List (find, intercalate, intersect)
> import Data.List.Split (splitOn)
> import Network.FastCGI (runFastCGIConcurrent')
> import Network.URI (URI(..), unEscapeString)

\section{Entry and Dispatch}

When the program starts, it immediately begin listening for connections.
|runFastCGIConcurrent'| spawns up to 10 threads. |handleErrors'| and |runApp|
will be explained later. The basically catch unhandled database errors and pack
information into the App monad.

Before forking, we read a configuration file. We pass this to runApp so that
all threads have access to global configuration information.

The first thing we do after forking is establish a database connection. The
database connection might be used immediately in order to log errors. It'll
eventually be passed to the App monad where it'll be packed into a reader
environment.

> main :: IO ()
> main = do  cp' <- getConfig
>            case cp' of
>              Left e    -> print e
>              Right cp  -> runFastCGIConcurrent' forkIO 10 (do
>                c <- liftIO connect
>                handleErrors' c (runApp c cp handleRequest))

The |configFile| is the one bit of configuration that's the same in all
environments.

> configFile :: String
> configFile = "/etc/vocabulink.conf"

These config vars are required for Vocabulink to do anything useful. We check
for them at load time and they can be safely read later with |forceEither $
get|.

> requiredConfigVars :: [String]
> requiredConfigVars = ["authtokensalt", "articledir"]

This retrieves the config file and makes sure that it contains all of the
required configuration parameters. This is so that we find out about errors
when starting the program rather than in individual threads later.

> getConfig :: IO (Either CPError ConfigParser)
> getConfig = runErrorT $ do
>   cp <- join $ liftIO $ readfile emptyCP configFile
>   opts <- options cp "DEFAULT"
>   if intersect requiredConfigVars opts == requiredConfigVars
>      then return cp
>      else error "Missing configuration options."

|handleRequest| ``digests'' the requested URI before passing it to the
 dispatcher. It also sets the response header. If we ever serve up non-HTML
 content, the header will need to be set at a lower level.

> handleRequest :: App CGIResult
> handleRequest = do
>   uri     <- requestURI
>   method  <- requestMethod
>   let path = pathList uri
>   dispatch' method path

We extract the path part of the URI, ``unescape it'' (convert % codes back to
characters), decode it (convert \mbox{UTF-8} characters to Unicode Chars), and finally
parse it into directory and filename components.

\begin{quote}@/some/directory/and/a/filename@\end{quote}

becomes

\begin{quote}|["some","directory","and","a","filename"]|\end{quote}

Note that the parser does not have to deal with query strings or fragments
because |uriPath| has already stripped them.

The one case this doesn't handle correctly is @//something@, because it's
handled differently by |Network.CGI|.

> pathList :: URI -> [String]
> pathList = splitOn "/" . decodeString . unEscapeString . uriPath

Before we actually dispatch the request, we use the opportunity to clean up the
URI and redirect the client if necessary. This handles cases like trailing
slashes. We want only one URI to point to a resource.

> dispatch' :: String -> [String] -> App CGIResult
> dispatch' method path =
>   case path of
>     ["",""]  -> frontPage -- "/"
>     ("":xs)  -> case find (== "") xs of
>                   Nothing  -> dispatch method xs
>                   Just _   -> redirect $ "/" ++ (intercalate "/" $ filter (/= "") xs)
>     _        -> output404 path

Here is where we dispatch each request to a function. We can match the request
on method and path components. This means that we can dispatch a request to one
function for a @GET@ and to another for a @POST@.

> dispatch :: String -> [String] -> App CGIResult

\subsection{Articles}

Some permanent URIs use the article system. You could call these elevated
articles. We use articles because the system for managing them exists already
(revision control, etc)

Each @.html@ file is actually an HTML fragment. These happen to be generated
from Muse Mode files by Emacs, but we don't really care where they come from.

> dispatch "GET" ["privacy"]     =  articlePage "privacy"
> dispatch "GET" ["help"]        =  articlePage "help"
> dispatch "GET" ["copyrights"]  =  articlePage "copyrights"
> dispatch "GET" ["disclaimer"]  =  articlePage "disclaimer"

Other articles can be created without recompilation. We just have to rescan the
filesystem for them. They also live in the @/article@ namespace (specifically
at @/article/title@).

> dispatch "POST"  ["articles"] = refreshArticles
> dispatch "GET"   ["articles"] = articlesPage

> dispatch "GET" ["article",x] = articlePage x

\subsection{Link Pages}

Vocabulink revolves around links---the associations between words or ideas. As
with articles, we have different functions for retrieving a single link or a
listing of links. However, the dispatching is complicated by the fact that
members can operate upon links (we need to handle the @POST@ method).

If we could rely on the @DELETE@ method being supported, this would be a little
less ugly. However, I've decided to only use @GET@ and @POST@. All other
methods are appended as an extra path component (here, as |method'|). I'm not
100\% satisfied with this design decision, but I haven't thought of a better way
yet.

For clarity, this dispatches:

\begin{center}
\begin{tabular}{lcl}
@GET  /link/10@           & $\rightarrow$ & linkPage \\
@GET  /link/something@    & $\rightarrow$ & not found \\
@GET  /link/10/something@ & $\rightarrow$ & not found \\
@POST /link/10/delete@    & $\rightarrow$ & deleteLink
\end{tabular}
\end{center}

> dispatch method path@("link":x:method') = do
>   case maybeRead x of
>     Nothing  -> output404 path
>     Just n   -> case (method, method') of
>                   ("GET"   ,[])          -> linkPage n
>                   ("POST"  ,["delete"])  -> deleteLink n
>                   (_       ,_)           -> output404 path

\subsection{Searching}

Retrieving a listing of links is easier.

Searching means forms and forms mean query strings. So if there's a @contains@
in the query string for the links page, it will do a search.

> dispatch "GET" ["links"] = do
>   contains <- getInput "contains"
>   maybe linksPage linksContainingPage contains

Creating a new link is a 2-step process. First, the member must request a page
on which to enter information about the link. Then they @POST@ the details to
establish the link.

> dispatch "GET"   ["link"] = newLink
> dispatch "POST"  ["link"] = newLink

\subsection{Link Review}

Members review their links by interacting with the site in a vaguely REST-ish
way. The intent behind this is that in the future they will be able to review
their links through different means such as a desktop program or a phone
application.

Because of the use of |withRequiredMemberNumber|, a logged out member will be
redirected to a login page when attempting to review.

\begin{center}
\begin{tabular}{lcl}
retrieve the next link for review & $\rightarrow$ & @GET  /review/next@ \\
mark link as reviewed             & $\rightarrow$ & @POST /review/n@ \\
add a link for review             & $\rightarrow$ & @POST /review/n/add@
\end{tabular}
\end{center}

> dispatch method path@("review":rpath) =
>   withRequiredMemberNumber $ \memberNo ->
>     case (method,rpath) of
>       ("GET"   ,["next"])   -> nextReview memberNo
>       ("POST"  ,(x:xs))     -> do
>          case maybeRead x of
>            Nothing  -> outputError 400
>                        "Links are identified by numbers only." []
>            Just n   -> case xs of
>                          ["add"]  -> newReview memberNo n
>                          []       -> linkReviewed memberNo n
>                          _        -> output404 path
>       (_       ,_)          -> output404 path

\subsection{Membership}

Becoming a member is simply a matter of filling out a form.

> dispatch "GET"   ["member","join"]  = registerMember
> dispatch "POST"  ["member","join"]  = registerMember

Logging in is a similar process.

> dispatch "GET"   ["member","login"]  = login
> dispatch "POST"  ["member","login"]  = login

And logging out can be done without a form.

> dispatch "POST" ["member","logout"]  = logout

\subsection{Forums}

As Vocabulink is still growing (and in the future), it's important to help new
members along to get feedback from them. For this, Vocabulink has forums.
Forums have been used in communities, especially language-learning communities
for a long time.

> dispatch "GET"   ["forums"] = forumsPage
> dispatch "POST"  ["forums"] = forumsPage

> dispatch "POST"  ["forum","new"] = createForum
> dispatch "GET"   ["forum",x] = forumPage x

> dispatch "GET"   ["forum",x,"new"] = newTopicPage x
> dispatch "POST"  ["forum",x,"new"] = newTopicPage x

> dispatch "GET"   path@["forum",x,y] =
>   case maybeRead y of
>     Nothing  -> output404 path
>     Just n   -> forumTopicPage x n

\subsection{Everything Else}

It would be nice to automatically respond with "Method Not Allowed" on URIs
that exist but don't make sense for the requested method (presumably @POST@).
However, we need to take a simpler approach because of how the dispatch method
was designed (pattern matching is limited). We output a qualified 404 error.

> dispatch _ path = output404 path

Finally, we get to an actual page of the site: the front page. Currently, it's
just a test of the widget system that displays the "MyLinks" widget if the
client is logged in (and nothing otherwise). It gets the common header, footer,
and associated functionality by using the stdPage function.

> frontPage :: App CGIResult
> frontPage = do
>   memberNo <- asks appMemberNo
>   w <- maybe (return noHtml) (\_ -> renderWidget (MyLinks 10)) memberNo
>   simplePage "Welcome to Vocabulink" [] [w]

%include Vocabulink/Utils.lhs
%include Vocabulink/CGI.lhs
%include Vocabulink/App.lhs
%include Vocabulink/DB.lhs
%include Vocabulink/Html.lhs
%include Vocabulink/Member/AuthToken.lhs
%include Vocabulink/Member.lhs
%include Vocabulink/Link.lhs
%include Vocabulink/Review.lhs
%include Vocabulink/Review/Html.lhs
%include Vocabulink/Review/SM2.lhs
%include Vocabulink/Article.lhs
%include Vocabulink/Widget.lhs
%include Vocabulink/Widget/MyLinks.lhs
%include Vocabulink/Forum.lhs

\end{document}