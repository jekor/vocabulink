\section{The MyLinks Widget}

The first need I came across when trying to use Vocabulink for myself was to
see a list of links that I'd created. This is a very simple widget that
demonstrates widgets in general.

> module Vocabulink.Widget.MyLinks (MyLinks(..)) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Widget
> import Vocabulink.Utils

A MyLinks widget consists of just the number of links to display. This allows
us to place the widget in different contexts in different sizes.

> data MyLinks = MyLinks Int

The MyLinks widget should only be rendered for logged-in members. If we attempt
to render it in some other context it will display an error.

We take advantage of the generic predicates list provided by |getPartialLinks|.

> instance Widget MyLinks where
>   renderWidget (MyLinks n) =
>     withMemberNumber (stringToHtml "error, not logged in") $ \memberNo -> do
>       partials <- getPartialLinks ["author = ?"] [toSql memberNo] 0 n
>       return $ case partials of
>         Nothing  -> paragraph << "Error retrieving your links."
>         Just ps  -> thediv ! [theclass "widget"] <<
>           [  h3 << "My Links",
>              unordList $ map displayPossiblePartial ps ]
>     where displayPossiblePartial =
>             maybe (paragraph << "Error retrieving link.") partialLinkHtml
