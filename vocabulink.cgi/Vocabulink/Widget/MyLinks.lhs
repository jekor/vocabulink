> module Vocabulink.Widget.MyLinks (MyLinks(..)) where

> import Vocabulink.Link (getMemberPartialLinks)
> import Vocabulink.Link.Types (partialLinkHtml)
> import Vocabulink.Member (withMemberNumber')
> import Vocabulink.Widget (Widget, renderWidget)

> import Text.XHtml.Strict

> data MyLinks = MyLinks

> instance Widget MyLinks where
>   renderWidget _ =
>     withMemberNumber' $ \memberNo -> do
>       links <- getMemberPartialLinks memberNo
>       return $ thediv ! [theclass "widget"] <<
>         [ h3 << "My Links",
>           unordList $ map partialLinkHtml links ]