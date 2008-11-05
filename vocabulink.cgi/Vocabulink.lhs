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
> dispatch "GET" ["blah","di"] = testPage
> dispatch "GET" ["lexeme",""] = outputError 404 "Lexeme is required." []
> dispatch "GET" ["lexeme",x] = lexemePage x
> dispatch "GET" ["link"] = newLinkPage
> dispatch "GET" ["link",x] = linkPage x
> dispatch "GET" ["links"] = linksPage Nothing
> dispatch "GET" ["my","links"] = do
>   n <- loginNumber
>   linksPage (Just n)

Each link for review can be added to a set. Most people will only use their
default (unnamed) set.

> dispatch method' ("review":xs) = do
>   memberNo <- loginNumber
>   if memberNo == 0
>      then redirectToLoginPage
>      else case (method',xs) of
>             ("GET",["next"])   -> reviewLink memberNo
>             ("POST",["set",x]) -> newReview memberNo x
>             ("POST",[x])       -> linkReviewed' memberNo x
>             (m,x)              -> output404 (m:x)

> dispatch "GET"  ["member","join"] = output newMemberPage
> dispatch "POST" ["member","join"] = addMember'
> dispatch "GET"  ["member","login"] = loginPage
> dispatch "POST" ["member","login"] = login'
> dispatch "POST" ["member","logout"] = logout'
> dispatch "GET" x = output404 x

It would be nice to automatically respond with "Method Not Allowed" on pages
that exist but don't take the POST/whatever method (as opposed to responding
with 404).

> dispatch "POST" ["link"] = linkLexemes'

> dispatch "POST" _ = outputError 404 "Resource not found or POST not allowed on it." []

> dispatch _ _ = outputMethodNotAllowed ["GET", "POST"]

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepBy (many (noneOf "/")) (char '/')

> output404 :: [String] -> CGI CGIResult
> output404 = outputError 404 "Resource not found."

> testPage :: CGI CGIResult
> testPage = do
>   username <- loginName
>   vars <- getVars
>   inputs <- cgiGet cgiInputs
>   outputHtml $ page "Test Page" []
>     [ h1 << ("Hello " ++ username),
>       logoutForm,
>       paragraph << (pre << map (\x -> show x ++ "\n") vars) +++
>                  (pre << show inputs),
>       paragraph << anchor ! [href "."] << "test" ]
