> module Vocabulink.Utils where

> import Control.Exception
> import Text.XHtml.Strict

> if' :: Bool -> a -> a -> a
> if' True  x _ = x
> if' False _ y = y

> infixl 1 ?
> (?) :: Bool -> a -> a -> a
> (?) = if'

A common idiom is to use concatHtml for an element's contents.

> infixl 3 <<|
> (<<|) :: (Html -> Html) -> [Html] -> Html
> h <<| l = h << concatHtml l

> intFromString :: String -> IO (Either Exception Integer)
> intFromString s = try (readIO s >>= return)