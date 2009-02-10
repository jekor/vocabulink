> module Vocabulink.Widget (Widget, renderWidget) where

> import Vocabulink.App
> import Vocabulink.Html

All widgets produce HTML fragments.

> class Widget a where
>   renderWidget :: a -> App Html