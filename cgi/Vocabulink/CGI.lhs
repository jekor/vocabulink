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

\section{CGI}
\label{CGI}

Our program is by nature a CGI application. This is handy and troublesome at
the same time. For one, we have to deal with producing some reasonable output
for the client of our program, even when given incomplete or incorrect inputs.
But, since we're dealing with each of many requests in their own threads, it's
not the end of the world if we generate an error or don't catch an exception.

> module Vocabulink.CGI (  getInput, getRequiredInput, getInputDefault,
>                          readInput, readRequiredInput, readInputDefault,
>                          getInputs, getBody, handleErrors', referrerOrVocabulink,
>                          urlify, outputUnauthorized, outputText, outputJSON,
>                          output', getTextOrFileInput, tryCGI',
>  {- Network.CGI -}       getInputFPS, getInputFilename,
>                          MonadCGI, CGIResult, requestURI, requestMethod,
>                          getVar, setHeader, output, redirect, remoteAddr,
>                          outputError, outputMethodNotAllowed,
>                          Cookie(..), getCookie, setCookie, deleteCookie,
>                          getInputNames, getInputContentType, parseContentType,
>  {- Network.URI -}       uriPath, uriQuery,
>  {- Text.JSON -}         JSON, encode, toJSObject ) where

> import Vocabulink.DB
> import Vocabulink.Utils

> import Control.Exception (Exception, try)
> import Control.Monad.Reader (ReaderT(..))
> import Control.Monad.Writer (WriterT(..))
> import Data.ByteString.Lazy.UTF8 (fromString, toString)
> import Data.Char (isAlphaNum)
> import Data.Monoid (mempty)
> import Network.URI (uriPath, uriQuery)
> import Text.Formlets as F
> import Text.JSON (JSON, encode, toJSObject)

We're going to hide some Network.CGI functions so that we can override them
with versions that automatically handle UTF-8-encoded input.

> import Network.CGI hiding (getInput, readInput, getInputs, getBody)
> import Network.CGI.Monad (CGIT(..))
> import Network.CGI.Protocol (CGIResult(..))
> import qualified Network.CGI as CGI

It's quite probable that we're going to trigger an unexpected exception
somewhere in the program. This is especially likely because we're interfacing
with a database via strings. Rather than blow up, we'd like to catch and log
the exception before giving the client some sort of indication that something
went wrong.

|catchCGI| will handle exceptions in the CGI monad. If no exception is thrown,
 we'll close the database handle and return the CGI result.

> handleErrors' :: IConnection conn => conn -> CGI CGIResult -> CGI CGIResult
> handleErrors' c a =  catchCGI' (do  r <- a
>                                     liftIO $ disconnect c
>                                     return r)
>                      (outputException' c)

Network.CGI provides |outputException| as a basic default error handler. This
is a slightly modified version that logs errors.

One case that needs to be tested is when an error message has non-ASCII
characters. I'm not sure how |outputInternalServerError| will handle it.

By the time we output the error to the client we no longer need the database
handle. This is the perfect place to close it, as it'll be the last thing we do
in the CGI monad (and the thread).

> outputException' ::  (MonadCGI m, MonadIO m, IConnection conn) =>
>                      conn -> SomeException -> m CGIResult
> outputException' c ex = do
>   liftIO $ logError "exception" (show ex)
>   liftIO $ disconnect c
>   outputInternalServerError [show ex]

Usually we use the |withRequired| functions when an action requires that the
client be authenticated. However, sometimes (as with AJAX) we want to output an
actual 403 error.

> outputUnauthorized :: (MonadCGI m, MonadIO m) => m CGIResult
> outputUnauthorized = outputError 403 "Unauthorized" []

The default |output| method provided by |Network.CGI| does not automatically
encode the input.

> output' :: MonadCGI m => String -> m CGIResult
> output' = return . CGIOutput . fromString

Also, we do not always output HTML. Sometimes we output JSON or HTML fragments.

> outputText :: (MonadCGI m, MonadIO m) => String -> m CGIResult
> outputText s = setHeader "Content-Type" "text/plain; charset=utf-8" >> output' s

To output JSON, we just need an associative list.

> outputJSON :: (MonadCGI m, MonadIO m, JSON a) => [(String, a)] -> m CGIResult
> outputJSON = outputText . encode . toJSObject

In some cases we'll need to redirect the client to where it came from after we
perform some action. We use this to make sure that we don't redirect them off
of the site.

> referrerOrVocabulink :: MonadCGI m => m String
> referrerOrVocabulink =
>   maybe "http://www.vocabulink.com/" decodeString `liftM` getVar "HTTP_REFERER"

We need to handle UTF-8-encoded GET and POST parameters. The following are
enhanced versions of Network.CGI's |getInput| and |readInput| along with a few
helpers.

> getInput :: MonadCGI m => String -> m (Maybe String)
> getInput = liftM (>>= Just . convertLineEndings . toString) . CGI.getInputFPS

We need to do the same for getInputs. (It's used by |runForm| at the least.)

> getInputs :: MonadCGI m => m [(String, String)]
> getInputs = map decode' `liftM` CGI.getInputsFPS
>     where decode' (x, y) = (decodeString x, convertLineEndings $ toString y)

Are you noticing a pattern here?

> getBody :: MonadCGI m => m String
> getBody = toString `liftM` CGI.getBodyFPS

Often we'll want an input from the client but are happy to fall back to a
default value.

> getInputDefault :: MonadCGI m => String -> String -> m String
> getInputDefault d = liftM (fromMaybe d) . getInput

As a convenience, |getRequiredInput| will throw an error on a missing input.
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
> readInputDefault d = liftM (fromMaybe d) . readInput

> readRequiredInput :: (Read a, MonadCGI m) => String -> m a
> readRequiredInput p =
>   readInputDefault (error $ "Parameter '" ++ p ++ "' is required.") p

File inputs are a bit of a hassle to deal with.

> getTextOrFileInput :: MonadCGI m => String -> m (Maybe (Either String File))
> getTextOrFileInput name = do
>   contentType' <- getInputContentType name
>   case contentType' of
>     Nothing  -> return Nothing
>     Just ct  -> case ct of
>                   "text/plain"  -> do
>                     val <- fromJust `liftM` getInput name
>                     return $ Just $ Left val
>                   ct'           -> do
>                     ct'' <- parseContentType ct'
>                     content'   <- getInputFPS name
>                     fileName'  <- getInputFilename name
>                     return $ Just $ Right File {
>                       content      = fromJust content',
>                       fileName     = fromJust fileName',
>                       contentType  = F.ContentType {  F.ctType = CGI.ctType ct'',
>                                                       F.ctSubtype = CGI.ctSubtype ct'',
>                                                       F.ctParameters = CGI.ctParameters ct'' } }

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
new characters. It converts spaces to hyphens and only allows alphanumeric
characters and hyphens in the resulting string.

> urlify :: String -> String
> urlify = map toLower . filter (\e -> isAlphaNum e || (e == '-') || (e == '.')) . translate [(' ', '-')]

\subsection{New Extensible Exceptions}

> catchCGI' :: Exception e => CGI a -> (e -> CGI a) -> CGI a
> catchCGI' c h = tryCGI' c >>= either h return

> tryCGI' :: Exception e => CGI a -> CGI (Either e a)
> tryCGI' (CGIT c) = CGIT (ReaderT (WriterT . f . runWriterT . runReaderT c ))
>     where
>       f = liftM (either (\ex -> (Left ex,mempty)) (first Right)) . try
