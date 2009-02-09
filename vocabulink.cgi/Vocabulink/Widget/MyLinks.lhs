> module Vocabulink.Widget.MyLinks (MyLinks(..)) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Link (partialLinkFromValues)
> import Vocabulink.Link.Types (PartialLink(..), partialLinkHtml)
> import Vocabulink.Member (withMemberNumber')
> import Vocabulink.Widget (Widget, renderWidget)

> import Control.Monad.Reader (asks)
> import Text.XHtml.Strict

> data MyLinks = MyLinks Integer

> instance Widget MyLinks where
>   renderWidget (MyLinks n) =
>     withMemberNumber' $ \memberNo -> do
>       links <- getLatestMemberLinks memberNo n
>       return $ thediv ! [theclass "widget"] <<
>         [ h3 << "My Links",
>           unordList $ map partialLinkHtml links ]

> getLatestMemberLinks :: Integer -> Integer -> App [PartialLink]
> getLatestMemberLinks memberNo n = do
>   c <- asks db
>   r <- liftIO $ quickQuery' c "SELECT link_no, origin, destination, link_type \
>                               \FROM link WHERE author = ? \
>                               \ORDER BY link_no DESC LIMIT ?"
>                               [toSql memberNo, toSql n]
>                   `catchSqlE` "No links found."
>   return $ map partialLinkFromValues r