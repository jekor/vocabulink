\section{Utility Functions}

> module Vocabulink.Utils (if', (?), intFromString, split) where

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

Split a string with a predicate. This returns empty lists before, after, and
between elements matching the predicate. For example:

split (== '/') "test"   = ["test"]
split (== '/') "/test"  = ["","test"]
split (== '/') "/test/" = ["","test",""]
split (== '/') "//test" = ["","","test"]

> split :: (a -> Bool) -> [a] -> [[a]]
> split p l = case break p l of
>               ([],(_:y))  -> [] : split p y
>               (x,[])      -> [x]
>               (x,(_:y))   -> x : split p y