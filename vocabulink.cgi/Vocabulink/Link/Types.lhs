> module Vocabulink.Link.Types (LinkType(..), Link(..), linkTypeName, linkEditHtml,
>                               newLinkHtml, establishLinkType, linkFromForm,
>                               getLinkFromPartial, linkTypeHtml, PartialLink(..),
>                               getPartialLinkType, partialLinkHtml) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils ((?))

> import Codec.Binary.UTF8.String (encodeString)
> import Control.Monad.Reader (asks)

Each link between lexemes has a type. This type determines how the link is
displayed, edited, used in statistical analysis, etc.

> data LinkType = Association | Cognate | LinkWord String String |
>                 ForeignLinkWord String String | Relationship String String
>                 deriving (Show, Eq)

> data Link = Link Integer LinkType String String

> linkType :: Link -> LinkType
> linkType (Link _ t _ _) = t

Fully loading a link from the database requires 2 database queries. But we
don't always need all of the data associated with a link. It's usually enough
to know just its type and the 2 lexemes it's linking.

We'll use a separate type to represent this. Essentially we'll be passing dummy
parameters to the LinkType constructors. We need a separate type so that we
don't accidentally pass a partial link to a function that expects a complete
link.

> data PartialLink = PartialLink Integer LinkType String String

Each link is represented by a name in the database.

> linkTypeName :: LinkType -> String
> linkTypeName Association          = "association"
> linkTypeName Cognate              = "cognate"
> linkTypeName (LinkWord _ _)        = "link word"
> linkTypeName (ForeignLinkWord _ _) = "foreign link word"
> linkTypeName (Relationship _ _)    = "relationship"

> getPartialLinkType :: String -> LinkType
> getPartialLinkType "association"       = Association
> getPartialLinkType "cognate"           = Cognate
> getPartialLinkType "link word"         = LinkWord "" ""
> getPartialLinkType "foreign link word" = ForeignLinkWord "" ""
> getPartialLinkType "relationship"      = Relationship "" ""
> getPartialLinkType _                   = error "Unknown link type."

> getLinkFromPartial :: PartialLink -> App Link
> getLinkFromPartial (PartialLink n t o d) = do
>   linkTypeDetails <- getLinkTypeDetails n t
>   return $ Link n linkTypeDetails o d

> getLinkTypeDetails :: Integer -> LinkType -> App LinkType
> getLinkTypeDetails _ Association = return Association
> getLinkTypeDetails _ Cognate = return Cognate
> getLinkTypeDetails n (LinkWord _ _) = do
>   c <- asks db
>   rs <- liftIO $ queryTuple c
>     "SELECT link_word, story FROM link_type_link_word \
>     \WHERE link_no = ?" [toSql n]
>       `catchSqlE` "Unable to retrieve link."
>   case rs of
>     [linkWord, story] -> return $ LinkWord (fromSql linkWord) (fromSql story)
>     _ -> error "Unable to retrieve link."
> getLinkTypeDetails n (ForeignLinkWord _ _) = do
>   c <- asks db
>   rs <- liftIO $ queryTuple c
>     "SELECT link_word, story FROM link_type_link_word \
>     \WHERE link_no = ?" [toSql n]
>       `catchSqlE` "Unable to retrieve link."
>   case rs of
>     [linkWord, story] -> return $ ForeignLinkWord (fromSql linkWord) (fromSql story)
>     _ -> error "Unable to retrieve link."
> getLinkTypeDetails n (Relationship _ _) = do
>   c <- asks db
>   rs <- liftIO $ queryTuple c
>     "SELECT left_side, right_side FROM link_type_relationship \
>     \WHERE link_no = ?" [toSql n]
>       `catchSqlE` "Unable to retrieve link."
>   case rs of
>     [leftSide, rightSide] -> return $ Relationship (fromSql leftSide) (fromSql rightSide)
>     _ -> error "Unable to retrieve link."

> establishLinkType :: IConnection conn => conn -> Integer -> LinkType -> IO (Integer)
> establishLinkType _ _ Association = return 1
> establishLinkType _ _ Cognate = return 1
> establishLinkType c linkNo (LinkWord linkWord story) =
>   run c "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                  \VALUES (?, ?, ?)"
>         [toSql linkNo, toSql linkWord, toSql story]
> establishLinkType c linkNo (ForeignLinkWord linkWord story) =
>   run c "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                  \VALUES (?, ?, ?)"
>         [toSql linkNo, toSql linkWord, toSql story]
> establishLinkType c linkNo (Relationship leftSide rightSide) =
>   run c "INSERT INTO link_type_relationship (link_no, left_side, right_side) \
>                                     \VALUES (?, ?, ?)"
>         [toSql linkNo, toSql leftSide, toSql rightSide]

> linkEditHtml :: Link -> [Html]
> linkEditHtml (Link _ (Association) _ _) = []
> linkEditHtml (Link _ (Cognate) _ _) = []
> linkEditHtml (Link _ (LinkWord _ story) _ _) =
>   [label << "link word:",
>    textfield "link-word", br,
>    textarea ! [name "story", cols "80", rows "20"] << story]
> linkEditHtml (Link n (ForeignLinkWord word story) o d) =
>   linkEditHtml $ Link n (LinkWord word story) o d
> linkEditHtml (Link _ (Relationship _ _) o d) =
>   [textfield "left-side",
>    stringToHtml " is to ",
>    textfield "right-side", br,
>    stringToHtml $ " as " ++ o ++ " is to " ++ d]

> partialLinkHtml :: PartialLink -> Html
> partialLinkHtml (PartialLink n _ o d) =
>   anchor ! [href ("/link/" ++ (show n))] << (o ++ (encodeString " → ") ++ d)

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
>   origin <- getRequiredInput "origin"
>   destination <- getRequiredInput "destination"
>   linkType' <- (getRequiredInput "link-type" >>= linkTypeFromForm)
>   return $ Link 0 linkType' origin destination

> linkTypeFromForm :: String -> App LinkType
> linkTypeFromForm "association" = return Association
> linkTypeFromForm "cognate" = return Cognate
> linkTypeFromForm "link word" = do
>   linkWord <- getRequiredInput "link-word"
>   story <- getRequiredInput "story"
>   return $ LinkWord linkWord story
> linkTypeFromForm "foreign link word" = do
>   linkWord <- getRequiredInput "link-word"
>   story <- getRequiredInput "story"
>   return $ ForeignLinkWord linkWord story
> linkTypeFromForm "relationship" = do
>   leftSide <- getRequiredInput "left-side"
>   rightSide <- getRequiredInput "right-side"
>   return $ Relationship leftSide rightSide
> linkTypeFromForm _ = error "Unknown link type"

> newLinkHtml :: String -> String -> String -> [Html]
> newLinkHtml def origin destination =
>   map editBlock
>     [Link 0 (Association) origin destination,
>      Link 0 (Cognate) origin destination,
>      Link 0 (LinkWord "" "Write a story linking the two words here.")
>           origin destination,
>      Link 0 (ForeignLinkWord "" "Write a story linking the two words here.")
>           origin destination,
>      Link 0 (Relationship "" "") origin destination]
>         where editBlock :: Link -> Html
>               editBlock link = thediv !
>                 [identifier $ formName $ linkTypeName $ linkType link,
>                  theclass "link-editor",
>                  thestyle $ def == linkTypeName (linkType link) ? "" $ "display: none"] << linkEditHtml link

