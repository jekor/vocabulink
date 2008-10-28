> module Vocabulink.CGI where

> import Codec.Binary.UTF8.String

> import Network.FastCGI

Make sure that we output only UTF-8 encoded text.

> output' :: MonadCGI m => String -> m CGIResult
> output' = output . encodeString

> outputError' :: (MonadCGI m, MonadIO m) => Int -> String -> [String] -> m CGIResult
> outputError' code err msgs = outputError code (encodeString err) msgs

It's nice to have a single function that can retrieve an HTTP GET paramater for
us and do whatever's necessary to return a value in the context we need it in.
This idea came from Text.Regex's (=~).

> class CGIInputContext o where
>   getInput' :: String -> CGI o

> instance CGIInputContext String where
>   getInput' r = do s <- getInput r
>                    case s of
>                      Nothing -> error $ required r
>                      Just "" -> error $ required r
>                      Just x  -> return $ decodeString x
>                   where required x = x ++ " parameter is required."

> instance CGIInputContext (Maybe String) where
>   getInput' r = do s <- getInput r
>                    case s of
>                      Nothing -> return Nothing
>                      Just "" -> return Nothing
>                      Just x  -> return $ Just (decodeString x)

It would be nice to have a way to hijack outputError in order to change the
encoding, but I don't know of a way to short of modifying the source of
Network.CGI. I'm going to leave it be for now as I'll probably end up with my
own error output functions in time.

> handleErrors' :: CGI CGIResult -> CGI CGIResult
> handleErrors' r = do setHeader "Content-Type" "text/html; charset=utf-8"
>                      handleErrors r
