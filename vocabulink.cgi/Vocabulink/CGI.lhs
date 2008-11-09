> module Vocabulink.CGI where

> import Vocabulink.App (App)

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Exception (Exception(..), try)
> import Data.Maybe (fromMaybe)
> import Database.HDBC (sqlExceptions, SqlError(..))
> import Network.FastCGI
> import System.IO.Error (isUserError, ioeGetErrorString)

> handleErrors' :: CGI CGIResult -> CGI CGIResult
> handleErrors' = flip catchCGI outputException'

> outputException' :: (MonadCGI m, MonadIO m) => Exception -> m CGIResult
> outputException' e = es >>= outputInternalServerError
>     where es = case sqlExceptions e of
>                  Nothing -> case e of
>                               ErrorCall msg  -> liftIO $ return [msg]
>                               IOException ie -> liftIO $ return $ ioe ie
>                               _              -> liftIO $ return [show e]
>                  Just se -> liftIO (logSqlError se >> return ["Database error."])
>           ioe ie = if isUserError ie then [ioeGetErrorString ie] else [show ie]

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

It would be nice to have a way to hijack outputError in order to change the
encoding, but I don't know of a way to short of modifying the source of
Network.CGI. I'm going to leave it be for now as I'll probably end up with my
own error output functions in time.

logSqlError will write the error to stderr where it should be picked up and added
to an appropriate logfile.

> logSqlError :: SqlError -> IO ()
> logSqlError e = do logCGI $ "SQL Error: " ++ (init (seErrorMsg e))
>                    return ()

-- | Use 'outputError' to output and log a 500 Internal Server Error.
outputInternalServerError :: (MonadIO m, MonadCGI m) =>
                             [String] -- ^ Error information.
                          -> m CGIResult
outputInternalServerError es = outputError 500 "Internal Server Error" es

-- | Output an error page to the user, with the given
--   HTTP status code in the response. Also logs the error information
--   using 'logCGI'.
outputError :: (MonadCGI m, MonadIO m) =>
               Int      -- ^ HTTP Status code
            -> String   -- ^ Status message
            -> [String] -- ^ Error information
            -> m CGIResult
outputError c m es = 
      do logCGI $ show (c,m,es)
         setStatus c m
         setHeader "Content-type" "text/html; charset=ISO-8859-1"
         page <- errorPage c m es 
         output $ renderHtml page

> referer :: App String
> referer = do ref <- getVar "HTTP_REFERER"
>              return $ fromMaybe "http://www.vocabulink.com/" ref
