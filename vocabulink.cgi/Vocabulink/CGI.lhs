> module Vocabulink.CGI where

> import Codec.Binary.UTF8.String
> import Control.Exception
> import Network.FastCGI

It's nice to have a single function that can retrieve an HTTP GET paramater for
us and do whatever's necessary to return a value in the context we need it in.
This idea came from Text.Regex's (=~).

> class CGIInputContext a where
>   getInputDefault :: String -> a -> CGI a
>   getInput' :: String -> CGI a

> instance CGIInputContext String where
>   getInputDefault r d = do s <- getInput r
>                            case s of
>                              Nothing -> return d
>                              Just "" -> return d
>                              Just s' -> return $ decodeString s'
>   getInput' r = getInputDefault r $ error $ r ++ " parameter is required."

> instance CGIInputContext (Maybe String) where
>   getInput' r = do s <- getInput r
>                    case s of
>                      Nothing -> return Nothing
>                      Just "" -> return Nothing
>                      Just x  -> return $ Just (decodeString x)
>
>   getInputDefault = error "Trying to get a default input for Maybe type."

> instance CGIInputContext Integer where
>   getInputDefault r d = do
>     s <- getInput r
>     case s of
>       Nothing -> return d
>       Just s' -> do
>         i <- liftIO $ try $ readIO s'
>         case i of
>           Left _   -> return d
>           Right i' -> return i'
>   getInput' r = getInputDefault r $ error $
>                   r ++ " parameter is required and must be an integer."

There should be some way to combine this and the above declaration.

> instance CGIInputContext Int where
>   getInputDefault r d = do
>     s <- getInput r
>     case s of
>       Nothing -> return d
>       Just s' -> do
>         i <- liftIO $ try $ readIO s'
>         case i of
>           Left _   -> return d
>           Right i' -> return i'
>   getInput' r = getInputDefault r $ error $
>                   r ++ " parameter is required and must be an integer."

> instance CGIInputContext Double where
>   getInputDefault r d = do
>     s <- getInput r
>     case s of
>       Nothing -> return d
>       Just s' -> do
>         i <- liftIO $ try $ readIO s'
>         case i of
>           Left _   -> return d
>           Right i' -> return i'
>   getInput' r = getInputDefault r $ error $
>                   r ++ " parameter is required and must be a number."

It would be nice to have a way to hijack outputError in order to change the
encoding, but I don't know of a way to short of modifying the source of
Network.CGI. I'm going to leave it be for now as I'll probably end up with my
own error output functions in time.

> handleErrors' :: CGI CGIResult -> CGI CGIResult
> handleErrors' r = do setHeader "Content-Type" "text/html; charset=utf-8"
>                      handleErrors r

With getting variables, if it fails, it should just error out.

> getVarE :: String -> CGI String
> getVarE s = do
>   var <- getVar s
>   case var of
>     Nothing -> error "Failed to retrieve environment variable."
>     Just v  -> return v

...or we could provide a default value.

> getVarDefault :: String -> String -> CGI String
> getVarDefault s d = do
>   var <- getVar s
>   case var of
>     Nothing -> return d
>     Just v  -> return v

> referer :: CGI String
> referer = getVarDefault "HTTP_REFERER" "http://www.vocabulink.com/"