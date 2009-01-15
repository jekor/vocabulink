> module Vocabulink.Widget (Widget, renderWidget) where

> import Vocabulink.App (App)
> import Text.XHtml.Strict (Html)

All widgets produce HTML fragments.

> class Widget a where
>   renderWidget :: a -> App Html