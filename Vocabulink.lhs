> module Main where

> import Vocabulink.CGI
> import Vocabulink.Pages

> import Vocabulink.Member
> import Vocabulink.Lexeme

> import Codec.Binary.UTF8.String
> import Control.Concurrent (forkIO)
> import Network.FastCGI
> import Network.URI
> import Text.ParserCombinators.Parsec hiding (getInput, try)

> main :: IO ()
> main =  runFastCGIConcurrent' forkIO 10 (handleErrors' dispatch')

We handle all requests using a dispatcher.

> dispatch' :: CGI CGIResult
> dispatch' =  do uri <- requestURI
>                 method <- requestMethod
>                 setHeader "Content-Type" "text/html; charset=utf-8"
>                 case (pathPart uri) of
>                   Left err    -> outputError 500 (show err) []
>                   Right []    -> outputError 400 "Request not understood." []
>                   Right path' -> dispatch method path'
>     where pathPart = (parse pathComponents "") . decodeString . unEscapeString . uriPath

> dispatch :: String -> [String] -> CGI CGIResult
> dispatch "GET" [""] = testPage
> dispatch "GET" ["lexeme",x] = lexemePage x
> dispatch "GET" ["member","join"] = output newMemberPage
> dispatch "GET" ["member","login"] = output loginPage
> dispatch "GET" x = do
>   logCGI $ "404: " ++ (show x)
>   outputError 404 "" []

It would be nice to automatically respond with "Method Not Allowed" on pages
that exist but don't take the POST/whatever method (as opposed to responding
with 404).

> dispatch "POST" ["member","join"] = addMember'
> dispatch "POST" ["member","login"] = login'
> dispatch "POST" _ = outputError 404 "Resource not found or POST not allowed on it." []

> dispatch _ _ = outputMethodNotAllowed ["GET", "POST"]

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepBy (many (noneOf "/")) (char '/')