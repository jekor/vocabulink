> module Vocabulink.Widget.MyLinks (MyLinks(..)) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Widget (Widget, renderWidget)
> import Vocabulink.Utils

> data MyLinks = MyLinks Int

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
