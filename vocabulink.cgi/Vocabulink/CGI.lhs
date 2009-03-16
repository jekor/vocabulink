\section{CGI}
\label{CGI}

Our program is by nature a CGI application. This is handy and troublesome at
the same time. For one, we have to deal with producing some reasonable output
for the client of our program, even when given incomplete or incorrect inputs.
But, since we're dealing with each of many requests in their own threads, it's
not the end of the world if we generate an error or don't catch an exception.

To keep other modules from having to know exactly which CGI method we're using
(who knows, we may want to switch to SCGI later), we export some FastCGI
functions. This allows other modules to just import Vocabulink.CGI and allows
us the option of overriding its functions in the future (which we do already
with |readInput|). This is a common pattern in other modules.

> module Vocabulink.CGI (  getInput, getRequiredInput, getInputDefault,
>                          readInput, readRequiredInput, readInputDefault,
>                          getInputs, handleErrors', referrerOrVocabulink,
>                          urlify, outputUnauthorized, outputText, outputJSON,
>  {- Network.FastCGI -}   getInputFPS, getInputFilename,
>                          MonadCGI, CGIResult, requestURI, requestMethod,
>                          getVar, setHeader, output, redirect, remoteAddr,
>                          outputError, outputMethodNotAllowed,
>                          Cookie(..), getCookie, setCookie, deleteCookie,
>  {- Text.JSON -}         JSON, encode, toJSObject ) where

> import Vocabulink.DB
> import Vocabulink.Utils

> import Control.Exception (Exception(..))
> import Data.Char (toLower, isAlphaNum)
> import Text.JSON (JSON, encode, toJSObject)

We're going to hide some Network.CGI functions so that we can override them
with versions that automatically handle UTF-8-encoded input.

> import Network.FastCGI hiding (getInput, readInput, getInputs)
> import qualified Network.FastCGI as FCGI

It's quite probable that we're going to trigger an unexpected exception
somewhere in the program. This is especially likely because we're interfacing
with a database via strings. Rather than blow up, we'd like to catch and log
the exception before giving the client some sort of indication that something
went wrong.

|catchCGI| will handle exceptions in the CGI monad. If no exception is thrown,
 we'll close the database handle and return the CGI result.

> handleErrors' :: IConnection conn => conn -> CGI CGIResult -> CGI CGIResult
> handleErrors' c a =  catchCGI (do  r <- a
>                                    liftIO $ disconnect c
>                                    return r)
>                      (outputException' c)

Network.CGI provides |outputException| as a basic default error handler. This
is a slightly modified version that logs errors.

One case that needs to be tested is when an error message has non-ASCII
characters. I'm not sure how |outputInternalServerError| will handle it.

By the time we output the error to the client we no longer need the database
handle. This is the perfect place to close it, as it'll be the last thing we do
in the CGI monad (and the thrtead). If we didn't close it, we'd probably start
accumulating a pile of unused database handles.

> outputException' ::  (MonadCGI m, MonadIO m, IConnection conn) =>
>                      conn -> Exception -> m CGIResult
> outputException' c e = do
>   s <- liftIO $ logException c e
>   liftIO $ disconnect c
>   outputInternalServerError [s]

Usually we use the |withRequired| functions when an action requires that the
client be authenticated. However, sometimes (as with AJAX) we want to output an
actual 403 error.

> outputUnauthorized :: (MonadCGI m, MonadIO m) => m CGIResult
> outputUnauthorized = outputError 403 "Unauthorized" []

Also, we do not always output HTML. Sometimes we output JSON or HTML fragments.

> outputText :: (MonadCGI m, MonadIO m) => String -> m CGIResult
> outputText s = setHeader "Content-Type" "text/plain; charset=utf-8" >> output s

Output as JSON an associative list.

> outputJSON :: (MonadCGI m, MonadIO m, JSON a) => [(String, a)] -> m CGIResult
> outputJSON = outputText . encode . toJSObject

In some cases we'll need to redirect the client to where it came from after we
perform some action. We use this to make sure that we don't redirect them off
of the site.

> referrerOrVocabulink :: MonadCGI m => m String
> referrerOrVocabulink =
>   fromMaybe "http://www.vocabulink.com/" `liftM` getVar "HTTP_REFERER"

We need to handle UTF-8-encoded GET and POST parameters. The following are
enhanced versions of Network.CGI's |getInput| and |readInput| along with a few
helpers.

> getInput :: MonadCGI m => String -> m (Maybe String)
> getInput = liftM (>>= Just . decodeString) . FCGI.getInput

We need to do the same for getInputs. (It's used by |runForm| at the least.)

> getInputs :: MonadCGI m => m [(String, String)]
> getInputs = map decode `liftM` FCGI.getInputs
>     where decode (x, y) = (decodeString x, decodeString y)

Often we'll want an input from the client but are happy to fall back to a
default value.

> getInputDefault :: MonadCGI m => String -> String -> m String
> getInputDefault d p = getInput p >>= return . fromMaybe d

As a convenience, |readRequiredInput| will throw an error on a missing input.
It allows us to write simpler code, but eventually most calls to this should be
removed and we should more gracefully handle the error.

> getRequiredInput :: MonadCGI m => String -> m String
> getRequiredInput p =
>   getInputDefault (error $ "Parameter '" ++ p ++ "' is required.") p

The Read versions of the above handle automatically converting the requested
input to a required type (as long as that type is Readable).

> readInput :: (Read a, MonadCGI m) => String -> m (Maybe a)
> readInput = liftM (>>= maybeRead) . getInput

> readInputDefault :: (Read a, MonadCGI m) => a -> String -> m a
> readInputDefault d p = readInput p >>= return . fromMaybe d

> readRequiredInput :: (Read a, MonadCGI m) => String -> m a
> readRequiredInput p =
>   readInputDefault (error $ "Parameter '" ++ p ++ "' is required.") p

\subsection{Working with URLs}

Certain dynamic parts of the site, such as forum titles, are displayed to the
user in a friendly natural form but are also used in URLs. For those cases,
it's generally better to use URL-safe representations for both the URL and the
natural key in the database. This allows us, for example, to re-title a forum
without changing the URL it's located at. Also, since we have the natural key
from the URL, we don't need to do an extra database lookup to find the key from
a mapping table.

Note that this is not meant to handle arbitrary input from users. For that we
can use URL encoding. This is to make common URLs friendly.

Note that this is currently very restrictive until the need arises to permit
new characters.

> urlify :: String -> String
> urlify = map toLower . filter (\e -> isAlphaNum e || (e == '-')) . translate [(' ', '-')]