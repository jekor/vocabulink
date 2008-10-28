> module Vocabulink.Utils where

> import Control.Exception

> if' :: Bool -> a -> a -> a
> if' True  x _ = x
> if' False _ y = y

> infixl 1 ?
> (?) :: Bool -> a -> a -> a
> (?) = if'

> intFromString :: String -> IO (Either Exception Integer)
> intFromString s = try (readIO s >>= return)