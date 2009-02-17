> module Vocabulink.Widget.MyLinks (MyLinks(..)) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Widget (Widget, renderWidget)
> import Vocabulink.Utils

> data MyLinks = MyLinks Integer

> instance Widget MyLinks where
>   renderWidget (MyLinks n) =
>     withMemberNumber (stringToHtml "error, not logged in") $ \memberNo -> do
>       links <- getLatestMemberLinks memberNo n
>       return $ thediv ! [theclass "widget"] <<
>         [ h3 << "My Links",
>           case links of
>             Just l   -> unordList $ map partialLinkHtml l
>             Nothing  -> stringToHtml "Error retrieving links." ]

> getLatestMemberLinks :: Integer -> Integer -> App (Maybe [PartialLink])
> getLatestMemberLinks memberNo n = do
>   r <- quickQuery''  "SELECT link_no, link_type, origin, destination \
>                      \FROM link WHERE author = ? \
>                      \ORDER BY link_no DESC LIMIT ?"
>                      [toSql memberNo, toSql n]
>   case r of
>     Just []  -> return $ Just []
>     Just r'  -> return $ Just $ catMaybes $ map partialLinkFromValues r'
>     _        -> return Nothing