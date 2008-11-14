> module Vocabulink.Link.Types (LinkType(..), Link(..), linkName, linkEditHtml, newLinkHtml) where

> import Vocabulink.Html (formName)
> import Vocabulink.Utils ((?))

> import Text.XHtml.Strict

Each link between lexemes has a type. This type determines how the link is
displayed, edited, used in statistical analysis, etc.

> data LinkType = Association | Cognate | LinkWord String String |
>                 ForeignLinkWord String String | Relationship String String

> data Link = Link LinkType String String

Each link is represented by a name in the database.

> linkName :: Link -> String
> linkName (Link (Association) _ _) = "association"
> linkName (Link (Cognate) _ _) = "cognate"
> linkName (Link (LinkWord _ _) _ _) = "link word"
> linkName (Link (ForeignLinkWord _ _) _ _) = "foreign link word"
> linkName (Link (Relationship _ _) _ _) = "relationship"

> linkEditHtml :: Link -> [Html]
> linkEditHtml (Link (Association) _ _) = []
> linkEditHtml (Link (Cognate) _ _) = []
> linkEditHtml (Link (LinkWord _ story) _ _) =
>   [label << "link word",
>    textfield "link-word",
>    label << "story",
>    textarea ! [name "story", cols "80", rows "20"] << story]
> linkEditHtml (Link (ForeignLinkWord word story) o d) =
>   linkEditHtml $ Link (LinkWord word story) o d
> linkEditHtml (Link (Relationship _ _) o d) =
>   [textfield "left-side",
>    stringToHtml " is to ",
>    textfield "right-side", br,
>    stringToHtml $ " as " ++ o ++ " is to " ++ d]

> newLinkHtml :: String -> String -> String -> [Html]
> newLinkHtml def origin destination =
>   map editBlock
>     [Link (Association) origin destination,
>      Link (Cognate) origin destination,
>      Link (LinkWord "" "Write a story linking the two words here.")
>           origin destination,
>      Link (ForeignLinkWord "" "Write a story linking the two words here.")
>           origin destination,
>      Link (Relationship "" "") origin destination]
>         where editBlock :: Link -> Html
>               editBlock link = thediv !
>                 [identifier $ formName $ linkName link,
>                  theclass "link-editor",
>                  thestyle $ def == linkName link ? "" $ "display: none"] << linkEditHtml link

