> module Main where

> import Vocabulink.CGI
> import Vocabulink.Html
> import Vocabulink.Lexeme
> import Vocabulink.Link
> import Vocabulink.Member
> import Vocabulink.Review

> import Codec.Binary.UTF8.String
> import Control.Concurrent (forkIO)
> import Network.CGI.Monad
> import Network.CGI.Protocol
> import Network.FastCGI
> import Network.URI
> import Text.ParserCombinators.Parsec hiding (getInput, try)
> import Text.XHtml.Strict

> main :: IO ()
> main =  runFastCGIConcurrent' forkIO 10 (handleErrors' dispatch')

We handle all requests using a dispatcher.

> dispatch' :: CGI CGIResult
> dispatch' =  do uri <- requestURI
>                 method' <- requestMethod
>                 setHeader "Content-Type" "text/html; charset=utf-8"
>                 case (pathPart uri) of
>                   Left err    -> outputError 500 (show err) []
>                   Right []    -> outputError 400 "Request not understood." []
>                   Right path' -> dispatch method' path'
>     where pathPart = (parse pathComponents "") . decodeString . unEscapeString . uriPath

> dispatch :: String -> [String] -> CGI CGIResult
> dispatch "GET" [""] = testPage
> dispatch "GET" ["lexeme",x] = lexemePage x
> dispatch "GET" ["link"] = newLinkPage
> dispatch "GET" ["link",x] = linkPage x
> dispatch "GET" ["links"] = linksPage Nothing
> dispatch "GET" ["my","links"] = do
>   n <- loginNumber
>   linksPage (Just n)
> dispatch "GET" ["review","next"] = reviewLink
> dispatch "GET" ["member","join"] = output newMemberPage
> dispatch "GET" ["member","login"] = loginPage
> dispatch "GET" x = do
>   logCGI $ "404: " ++ (show x)
>   outputError 404 "" []

It would be nice to automatically respond with "Method Not Allowed" on pages
that exist but don't take the POST/whatever method (as opposed to responding
with 404).

> dispatch "POST" ["member","join"] = addMember'
> dispatch "POST" ["member","login"] = login'
> dispatch "POST" ["link"] = linkLexemes'

Each link for review can be added to a set. Most people will only use their
default (unnamed) set.

> dispatch "POST" ["review","set",x] = newReview x
> dispatch "POST" ["review",x] = linkReviewed' x

> dispatch "POST" _ = outputError 404 "Resource not found or POST not allowed on it." []

> dispatch _ _ = outputMethodNotAllowed ["GET", "POST"]

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepBy (many (noneOf "/")) (char '/')

> testPage :: CGI CGIResult
> testPage = do
>   username <- loginName
>   vars <- getVars
>   inputs <- cgiGet cgiInputs
>   outputHtml $ page "Test Page" []
>     [ h1 << ("Hello " ++ username),
>       paragraph << (pre << map (\x -> show x ++ "\n") vars) +++
>                  (pre << show inputs),
>       paragraph << anchor ! [href "."] << "test" ]
