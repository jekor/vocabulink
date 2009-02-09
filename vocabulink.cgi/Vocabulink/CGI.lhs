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
>                          handleErrors', output404,
>                          refererOrVocabulink,
>  {- Network.FastCGI -}   requestURI, requestMethod, getVar,
>                          setHeader, redirect, remoteAddr,
>                          outputError, outputMethodNotAllowed,
>                          Cookie(..), deleteCookie) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Utils

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Exception (Exception(..))
> import Control.Monad (liftM)
> import Data.Maybe (fromMaybe)

We're going to hide some Network.CGI functions so that we can override them
with versions that automatically handle UTF-8-encoded input.

> import Network.FastCGI hiding (getInput, readInput)
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

404 errors are common enough that it makes sense to have a function just for
reporting them to the client. We also want to log 404 errors, as they may
indicate a problem or opportunity with the site.

This takes a list of Strings that are output as extra information to the
client.

> output404 :: [String] -> App CGIResult
> output404 s = do  logApp "404" (show s)
>                   outputError 404 "Resource not found." s

In some cases we'll need to redirect the client to where it came from after we
perform some action. We use this to make sure that we don't redirect them off
of the site.

> refererOrVocabulink :: App String
> refererOrVocabulink = do
>   ref <- getVar "HTTP_REFERER"
>   return $ fromMaybe "http://www.vocabulink.com/" ref

We need to handle UTF-8-encoded GET and POST parameters. The following are
enhanced versions of Network.CGI's |getInput| and |readInput| along with a few
helpers.

> getInput :: MonadCGI m => String -> m (Maybe String)
> getInput = liftM (>>= Just . decodeString) . FCGI.getInput

Often we'll want an input from the client but are happy to fall back to a
default value.

> getInputDefault :: MonadCGI m => String -> String -> m String
> getInputDefault d p = do
>   i <- getInput p
>   return $ fromMaybe d i

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
> readInputDefault d p = do
>   i <- readInput p
>   return $ fromMaybe d i

> readRequiredInput :: (Read a, MonadCGI m) => String -> m a
> readRequiredInput p =
>   readInputDefault (error $ "Parameter '" ++ p ++ "' is required.") p
