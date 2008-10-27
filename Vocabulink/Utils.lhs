> module Vocabulink.Utils where

> if' :: Bool -> a -> a -> a
> if' True  x _ = x
> if' False _ y = y

> infixl 1 ?
> (?) :: Bool -> a -> a -> a
> (?) = if'