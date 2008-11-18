> module Main where

> import Vocabulink.App
> import Vocabulink.Article (articlePage, articlesPage)
> import Vocabulink.CGI (handleErrors', output404)
> import Vocabulink.Html (stdPage)
> import Vocabulink.Link (lexemePage, newLinkPage, linkPage, linksPage, linkLexemes', searchPage, deleteLink)
> import Vocabulink.Member (withMemberNumber, login, logout, newMemberPage, addMember', loginPage)
> import Vocabulink.Review (newReview, reviewLink, linkReviewed')
> import Vocabulink.Utils (intFromString)

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Concurrent (forkIO)
> import Data.List (intercalate)
> import Network.CGI.Monad (cgiGet)
> import Network.CGI.Protocol (cgiInputs)
> import Network.FastCGI
> import Network.URI (unEscapeString, uriPath)
> import Text.ParserCombinators.Parsec (Parser, parse, char, sepEndBy, many, noneOf)
> import Text.XHtml.Strict

> main :: IO ()
> main =  runFastCGIConcurrent' forkIO 10 (handleErrors' (runApp dispatch'))

We handle all requests using a dispatcher.

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
>   vars <- getVars
>   inputs <- cgiGet cgiInputs
>   stdPage "Test Page" []
>     [ h1 << "Test Page",
>       paragraph << (pre << map (\x -> show x ++ "\n") vars) +++
>                    (pre << show inputs),
>       paragraph << anchor ! [href "."] << "test" ]
