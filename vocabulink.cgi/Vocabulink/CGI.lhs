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
us the option of overriding its functions in the future. This is a common
pattern in other modules.

> module Vocabulink.CGI (handleErrors', logSqlError, output404, referer,
>                        CGIInputContext(..),
>  {- Network.FastCGI -} requestURI, requestMethod,
>                        setHeader, redirect,
>                        outputError, outputMethodNotAllowed) where

> import Vocabulink.App (App)

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Exception (Exception(..), try)
> import Data.Maybe (fromMaybe)

We don't want to import Vocabulink.DB, as it depends on this module, because
that would mean creating boot modules to break cyclic dependencies. We only
need to know about (uncaught) SQL exceptions anyway.

> import Database.HDBC (sqlExceptions, SqlError(..))
> import Network.FastCGI
> import System.IO.Error (isUserError, ioeGetErrorString)

It's quite probable that we're going to trigger an unexpected exception
somewhere in the program. This is especially likely because we're interfacing
with a database using text strings. Rather than blow up, we'd like to catch and
log the exception before giving the client some sort of indication that
something went wrong.

|catchCGI| will handle exceptions in the CGI monad.

> handleErrors' :: CGI CGIResult -> CGI CGIResult
> handleErrors' = flip catchCGI outputException'

Network.CGI provides |outputException| as a basic default error handler. This
is a slightly modified version that logs SQL-level errors and notifies the
client that a database error occurred. It also logs other errors.

One case that needs to be tested is when an error message has non-ASCII
characters. I'm not sure how either |logCGI| or |outputInternalServerError|
will handle it.

> outputException' :: (MonadCGI m, MonadIO m) => Exception -> m CGIResult
> outputException' e = es >>= outputInternalServerError
>     where es = case sqlExceptions e of
>                  Nothing  -> liftIO $ logError e >>= \e' -> return [e']
>                  Just se  -> liftIO $ logSqlError se >>= \e' -> return [e']

Our logging is very simple. At some point it would be nice to add information
on where the error was generated, as well as the time.

> logError :: Exception -> IO (String)
> logError (ErrorCall msg)  = logCGI msg >> return msg
> logError (IOException ie) = do
>   let ioe = if isUserError ie
>             then ioeGetErrorString ie
>             else show ie
>   logCGI ioe >> return ioe
> logError e                = logCGI (show e) >> return (show e)

> logSqlError :: SqlError -> IO (String)
> logSqlError e = do logCGI $ "SQL Error: " ++ (init (seErrorMsg e))
>                    return "Database Error"

> referer :: App String
> referer = do ref <- getVar "HTTP_REFERER"
>              return $ fromMaybe "http://www.vocabulink.com/" ref

|[String]| are output as extra information to the user.

> output404 :: [String] -> App CGIResult
> output404 = outputError 404 "Resource not found."

It's nice to have a single function that can retrieve an HTTP GET paramater for
us and do whatever's necessary to return a value in the context we need it in.
This idea came from Text.Regex's (=~).

> class CGIInputContext a where
>   getInputDefault :: a -> String -> App a
>   getInput' :: String -> App a

> instance CGIInputContext String where
>   getInputDefault d r = do i <- getInput r
>                            return $ maybe d (decodeString) i
>   getInput' r = getInputDefault (error $ "Parameter '" ++ r ++ "' is required.") r

> instance CGIInputContext (Maybe String) where
>   getInputDefault d r = do i <- getInput r
>                            return $ maybe d (Just . decodeString) i
>   getInput' = getInputDefault Nothing

> getInputDefaultNumeric :: (Num a, Read a) => a -> String -> App a
> getInputDefaultNumeric d r = do
>   s <- getInput r
>   case s of
>     Nothing -> return d
>     Just s' -> do
>       i <- liftIO $ try $ readIO s'
>       case i of
>         Left _   -> return d
>         Right i' -> return i'

> instance CGIInputContext Integer where
>   getInputDefault = getInputDefaultNumeric
>   getInput' r = getInputDefault (error $ "Parameter '" ++ r ++ "' is required and must be a whole number.") r

> instance CGIInputContext Int where
>   getInputDefault = getInputDefaultNumeric
>   getInput' r = getInputDefault (error $ "Parameter '" ++ r ++ "' is required and must be a whole number.") r

> instance CGIInputContext Double where
>   getInputDefault = getInputDefaultNumeric
>   getInput' r = getInputDefault (error $ "Parameter '" ++ r ++ "' is required and must be a number.") r
