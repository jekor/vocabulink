\section{Utility Functions}

> module Vocabulink.Utils (if', (?), intFromString) where

> import Control.Exception (try)

> if' :: Bool -> a -> a -> a
> if' True  x _ = x
> if' False _ y = y

> infixl 1 ?
> (?) :: Bool -> a -> a -> a
> (?) = if'

> intFromString :: String -> IO (Maybe Integer)
> intFromString s = do n <- try $ readIO s
>                      case n of
>                        Left _   -> return Nothing
>                        Right n' -> return $ Just n'