\documentclass[oneside]{article}
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
\date{January 16, 2009}

\begin{document}
\maketitle

\section{Introduction}

This is the main Vocabulink program. It is spawned as a FastCGI process and
handles all web requests for vocabulink.com.

Vocabulink.cgi is essentially a multi-user application which operates via the
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

\subsection{Our Modules}

These are the Vocabulink modules. They are grouped primarily based on division
of labor. The exception is the App module. The App module defines the App monad
and must make use of both database and CGI functions. In order to limit
cyclical dependencies (which can be a pain with the GHC compiler), it's broken
out into a separate module.

> import Vocabulink.App

Each of these modules will be described in its own section.

> import Vocabulink.Article
> import Vocabulink.CGI
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Member
> import Vocabulink.Review
> import Vocabulink.Utils
> import Vocabulink.Widget
> import Vocabulink.Widget.MyLinks

\subsection{Other Modules}

Vocabulink makes use of a half dozen or so Haskell libraries. Even though we don't use them all in this module, I'll describe them here so that they'll be more familiar as they're introduced (and so that you can jump directly to the section you're interested in after this introduction).

\begin{description}
\item[Codec.Binary.UTF8.String] Vocabulink would be pretty useless without being able to handle the writing systems of other languages. We only make use of 2 functions provided by this library: |encodeString| and |decodeString|. |decodeString| takes a UTF-8 string---either from the webserver or from the database---and converts it into a Unicode string that can be used by Haskell natively. We use |encodeString| to go in the other direction. Whenever we write out a string to the database, the webserver, or a log file; it needs to be encoded to UTF-8. This is something that the type system does not (yet) handle for us, so we need to be careful to correctly encode and decode strings.
\item[Network.URI] Various parts of the code may need to construct or deconstruct URLs. Using this library should be safer than using various string-mangling techniques throughout the code.
\item[Text.ParserCombinators.Parsec] We need to parse text quite a bit. The dispatcher, the member authentication routines, and the article publishing system all make use of Parsec; and probably more will in the future.
\item[System.Time] This is actually a deprecated library for handling time. However, the database library HDBC requires it. It's unfortunate, because this library is a bit of a mess.
\end{description}

There are a few more, but they are only used by a single Vocabulink module\footnote{The Vocabulink module may re-export some functions provided by the module, but the other Vocabulink modules should be able to remain ignorant of that.}.

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Concurrent (forkIO)
> import Control.Monad.Reader (asks)
> import Data.List (intercalate)
> import Data.Maybe (isJust)
> import Network.FastCGI (runFastCGIConcurrent')
> import Network.URI (unEscapeString, uriPath)
> import Text.ParserCombinators.Parsec (Parser, parse, char, sepEndBy, many, noneOf)

When the program starts, it should immediately begin listening for connections.
|runFastCGIConcurrent'| spawns up to 10 threads. |handleErrors'| and |runApp|
will be explained later. The basically catch unhandled database errors and pack information into the App monad.

TODO: Before public launch, the thread limit needs to be increased.

> main :: IO ()
> main =  runFastCGIConcurrent' forkIO 10 (handleErrors' (runApp dispatch'))

Work begins in the dispatcher.

> dispatch' :: App CGIResult
> dispatch' =  do uri <- requestURI
>                 method' <- requestMethod
>                 setHeader "Content-Type" "text/html; charset=utf-8"
>                 case (pathPart uri) of
>                   Left err    -> outputError 500 (show err) []
>                   Right []    -> outputError 400 "Request not understood." []
>                   Right path' -> dispatch method' path'
>     where pathPart = (parse pathComponents "") . decodeString . unEscapeString . uriPath

> staticPath :: FilePath
> staticPath = "/home/chris/project/vocabulink/static/"

> dispatch :: String -> [String] -> App CGIResult
> dispatch "GET" [""] = testPage

> dispatch "GET" ["privacy"] = displayStaticFile "Privacy Policy" $ staticPath ++ "privacy.html"
> dispatch "GET" ["help"] = displayStaticFile "Help" $ staticPath ++ "help.html"
> dispatch "GET" ["copyrights"] = displayStaticFile "Copyright Policy" $ staticPath ++ "copyrights.html"
> dispatch "GET" ["disclaimer"] = displayStaticFile "Disclaimer" $ staticPath ++ "disclaimer.html"

> dispatch "GET" ["blah","di"] = testPage
> dispatch "GET" ["lexeme",""] = outputError 404 "Lexeme is required." []
> dispatch "GET" ["lexeme",x] = lexemePage x
> dispatch "GET" ["link"] = newLinkPage
> dispatch "GET" ["links"] = linksPage
> dispatch "GET" ["search"] = searchPage

> dispatch "GET" ["article",x] = articlePage x
> dispatch "GET" ["articles"] = articlesPage

Each link for review can be added to a set. Most people will only use their
default (unnamed) set.

> dispatch method' ("review":xs) =
>   withMemberNumber $ \memberNo ->
>     case (method',xs) of
>       ("GET",["next"])   -> reviewLink memberNo
>       ("POST",["set",x]) -> newReview memberNo x
>       ("POST",[x])       -> linkReviewed' memberNo x
>       (m,x)              -> output404 (m:x)

> dispatch method' ("link":x:o) = do
>   n <- liftIO $ intFromString x
>   case n of
>     Nothing -> outputError 400 "Links are identified by numbers only." []
>     Just n' -> case (method', o) of
>                  ("GET",[]) -> linkPage n'
>                  ("POST",["delete"]) -> deleteLink n'
>                  (m,y) -> output404 (m:y)

> dispatch "GET"  ["member","join"] = newMemberPage
> dispatch "POST" ["member","join"] = addMember'
> dispatch "GET"  ["member","login"] = loginPage
> dispatch "POST" ["member","login"] = login
> dispatch "POST" ["member","logout"] = logout
> dispatch "GET" path' = if last path' == "" -- Redirect trailing /
>                           then redirect $ "/" ++ (intercalate "/" $ init path')
>                           else output404 path'

It would be nice to automatically respond with "Method Not Allowed" on pages
that exist but don't take the POST/whatever method (as opposed to responding
with 404).

> dispatch "POST" ["link"] = linkLexemes'

> dispatch "POST" _ = outputError 404 "Resource not found or POST not allowed on it." []

> dispatch _ _ = outputMethodNotAllowed ["GET", "POST"]

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepEndBy (many (noneOf "/")) (char '/')

Use this only if you know that the static file will be a valid fragment of XHTML.

> displayStaticFile :: String -> FilePath -> App CGIResult
> displayStaticFile t path = do
>   body' <- liftIO $ readFile path
>   stdPage t [] [primHtml body']

> testPage :: App CGIResult
> testPage = do
>   memberNo <- asks memberNumber
>   w <- isJust memberNo ? renderWidget (MyLinks 10) $ return noHtml
>   stdPage "Welcome to Vocabulink" []
>     [ h1 << "Welcome to Vocabulink",
>       w ]

%include Vocabulink/Utils.lhs
%include Vocabulink/CGI.lhs
%include Vocabulink/DB.lhs
%include Vocabulink/Html.lhs
%include Vocabulink/App.lhs
%include Vocabulink/Member.lhs
%include Vocabulink/Member/Auth.lhs
%include Vocabulink/Link.lhs
%include Vocabulink/Link/Types.lhs
%include Vocabulink/Review.lhs
%include Vocabulink/Review/Html.lhs
%include Vocabulink/Review/SM2.lhs
%include Vocabulink/Article.lhs
%include Vocabulink/Widget.lhs
%include Vocabulink/Widget/MyLinks.lhs

\end{document}