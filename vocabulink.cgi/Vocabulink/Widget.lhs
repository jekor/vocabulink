\section{Widgets}

I had an idea to build the interface for Vocabulink out of reusable modular
``widgets'' (seems to be the prevailing term in social web application
development). Pages tend to be composed of rectangles. Each rectangle can be
responsible for building and rendering itself.

This has been attempted in CMSes and frameworks before, with varying success.
There's a chance of getting poor performance due to the inability to combine
database queries (each widget querying for just what it needs), but I expect
that the abstraction provided will be worth it.

> module Vocabulink.Widget (Widget, renderWidget) where

> import Vocabulink.App
> import Vocabulink.Html

A widget is very simple. It's a type class with the single |renderWidget|
method that produces an HTML fragment.

> class Widget a where
>   renderWidget :: a -> App Html