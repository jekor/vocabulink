> module Vocabulink.Link.Types (LinkType(..), Link(..), linkTypeName, linkEditHtml,
>                               newLinkHtml, establishLinkType, linkFromForm,
>                               getLinkType, linkTypeHtml) where

> import Vocabulink.App
> import Vocabulink.CGI (getInput')
> import Vocabulink.DB (toSql', fromSql', catchSqlE)
> import Vocabulink.Html (formName)
> import Vocabulink.Utils ((?))

> import Codec.Binary.UTF8.String (encodeString)
> import Database.HDBC
> import Text.XHtml.Strict

Each link between lexemes has a type. This type determines how the link is
displayed, edited, used in statistical analysis, etc.

> data LinkType = Association | Cognate | LinkWord String String |
>                 ForeignLinkWord String String | Relationship String String
>                 deriving (Show, Eq)

> data Link = Link LinkType String String

Each link is represented by a name in the database.

> linkTypeName :: Link -> String
> linkTypeName (Link (Association) _ _) = "association"
> linkTypeName (Link (Cognate) _ _) = "cognate"
> linkTypeName (Link (LinkWord _ _) _ _) = "link word"
> linkTypeName (Link (ForeignLinkWord _ _) _ _) = "foreign link word"
> linkTypeName (Link (Relationship _ _) _ _) = "relationship"

> getLinkType :: IConnection conn => conn -> Integer -> String -> IO (Maybe LinkType)
> getLinkType _ _ "association" = return $ Just Association
> getLinkType _ _ "cognate" = return $ Just Cognate
> getLinkType c linkNo "link word" = do
>   rs <- quickQuery' c
>     "SELECT link_word, story FROM link_type_link_word \
>     \WHERE link_no = ?" [toSql linkNo]
>       `catchSqlE` "Unable to retrieve link."
>   case rs of
>     [[linkWord, story]] -> return $ Just $ LinkWord (fromSql' linkWord) (fromSql' story)
>     _ -> error "Unable to retrieve link."
> getLinkType c linkNo "foreign link word" = do
>   rs <- quickQuery' c
>     "SELECT link_word, story FROM link_type_link_word \
>     \WHERE link_no = ?" [toSql linkNo]
>       `catchSqlE` "Unable to retrieve link."
>   case rs of
>     [[linkWord, story]] -> return $ Just $ ForeignLinkWord (fromSql' linkWord) (fromSql' story)
>     _ -> error "Unable to retrieve link."
> getLinkType c linkNo "relationship" = do
>   rs <- quickQuery' c
>     "SELECT left_side, right_side FROM link_type_relationship \
>     \WHERE link_no = ?" [toSql linkNo]
>       `catchSqlE` "Unable to retrieve link."
>   case rs of
>     [[leftSide, rightSide]] -> return $ Just $ Relationship (fromSql' leftSide) (fromSql' rightSide)
>     _ -> error "Unable to retrieve link."
> getLinkType _ _ _ = error "Unknown link type"

> establishLinkType :: IConnection conn => conn -> Integer -> LinkType -> IO (Integer)
> establishLinkType _ _ Association = return 1
> establishLinkType _ _ Cognate = return 1
> establishLinkType c linkNo (LinkWord linkWord story) =
>   run c "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                  \VALUES (?, ?, ?)"
>         [toSql linkNo, toSql' linkWord, toSql' story]
> establishLinkType c linkNo (ForeignLinkWord linkWord story) =
>   run c "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                  \VALUES (?, ?, ?)"
>         [toSql linkNo, toSql' linkWord, toSql' story]
> establishLinkType c linkNo (Relationship leftSide rightSide) =
>   run c "INSERT INTO link_type_relationship (link_no, left_side, right_side) \
>                                     \VALUES (?, ?, ?)"
>         [toSql linkNo, toSql' leftSide, toSql' rightSide]

> linkEditHtml :: Link -> [Html]
> linkEditHtml (Link (Association) _ _) = []
> linkEditHtml (Link (Cognate) _ _) = []
> linkEditHtml (Link (LinkWord _ story) _ _) =
>   [label << "link word:",
>    textfield "link-word", br,
>    textarea ! [name "story", cols "80", rows "20"] << story]
> linkEditHtml (Link (ForeignLinkWord word story) o d) =
>   linkEditHtml $ Link (LinkWord word story) o d
> linkEditHtml (Link (Relationship _ _) o d) =
>   [textfield "left-side",
>    stringToHtml " is to ",
>    textfield "right-side", br,
>    stringToHtml $ " as " ++ o ++ " is to " ++ d]

> linkTypeHtml :: LinkType -> [Html]
> linkTypeHtml Association = []
> linkTypeHtml Cognate = []
> linkTypeHtml (LinkWord linkWord story) =
>   [paragraph << ("link word: " ++ (encodeString linkWord)), br,
>    paragraph << story]
> linkTypeHtml (ForeignLinkWord linkWord story) =
>   [paragraph << ("link word: " ++ (encodeString linkWord)), br,
>    paragraph << (encodeString story)]
> linkTypeHtml (Relationship leftSide rightSide) =
>   [paragraph << ((encodeString leftSide) ++ " -> " ++ (encodeString rightSide))]

> linkFromForm :: App Link
> linkFromForm = do
>   origin <- getInput' "origin"
>   destination <- getInput' "destination"
>   linkType <- (getInput' "link-type" >>= linkTypeFromForm)
>   return $ Link linkType origin destination

> linkTypeFromForm :: String -> App LinkType
> linkTypeFromForm "association" = return Association
> linkTypeFromForm "cognate" = return Cognate
> linkTypeFromForm "link word" = do
>   linkWord <- getInput' "link-word"
>   story <- getInput' "story"
>   return $ LinkWord linkWord story
> linkTypeFromForm "foreign link word" = do
>   linkWord <- getInput' "link-word"
>   story <- getInput' "story"
>   return $ ForeignLinkWord linkWord story
> linkTypeFromForm "relationship" = do
>   leftSide <- getInput' "left-side"
>   rightSide <- getInput' "right-side"
>   return $ Relationship leftSide rightSide
> linkTypeFromForm _ = error "Unknown link type"

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
>                 [identifier $ formName $ linkTypeName link,
>                  theclass "link-editor",
>                  thestyle $ def == linkTypeName link ? "" $ "display: none"] << linkEditHtml link

