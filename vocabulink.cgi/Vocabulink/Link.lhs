> module Vocabulink.Link (lexemePage, linkHtml, getLink, linkPage, newLinkPage,
>                         linkLexemes, searchPage, deleteLink, linksPage,
>                         Link(..), partialLinkFromValues) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html (stdPage, Dependency(..), pager, simpleChoice)
> import Vocabulink.Member (withMemberNumber)
> import Vocabulink.Review.Html (reviewHtml)
> import Vocabulink.Link.Types (Link(..), newLinkHtml, linkFromForm,
>                               linkTypeName, establishLinkType,
>                               getLinkFromPartial, linkTypeHtml,
>                               PartialLink(..), getPartialLinkType)

> import Codec.Binary.UTF8.String (encodeString)
> import Control.Monad (liftM)
> import Control.Monad.Reader (asks)
> import Data.Maybe (fromJust)
> import Database.HDBC (SqlValue, withTransaction, rollback)
> import Network.FastCGI (CGIResult, liftIO, redirect)
> import Text.XHtml.Strict

When retrieving the page for a lexeme, we first check to see if a lemma for
this lexeme is defined. If not, we assume it to be canonical.

> lexemePage :: String -> App CGIResult
> lexemePage l = do
>   c <- asks db
>   lemma <- liftIO $ queryValue c "SELECT lemma FROM lexeme \
>                                  \WHERE lexeme = ?" [toSql l]
>   case lemma of
>     Just lm -> redirect $ "/lexeme/" ++ encodeString (fromSql lm)
>     Nothing -> stdPage (encodeString l) [CSS "link"]
>       [ form ! [action "/link", method "get"] <<
>         [ hidden "origin" (encodeString l),
>           thediv ! [identifier "baseline", theclass "link"] <<
>             linkHtml (stringToHtml l)
>                      (textfield "destination" +++
>                       submit "" "link") ] ]

origin should already be UTF8 encoded.

> linkHtml :: Html -> Html -> Html
> linkHtml origin destination = concatHtml
>   [ thespan ! [theclass "lexeme"] << origin,
>     image ! [src "http://s.vocabulink.com/black.png", width "20%", height "1"],
>     thespan ! [theclass "lexeme"] << destination ]

> partialLinkFromValues :: [SqlValue] -> PartialLink
> partialLinkFromValues (n:o:d:t:[]) =
>   let partialLinkType = getPartialLinkType (fromSql t)
>   in PartialLink (fromSql n) partialLinkType (fromSql o) (fromSql d)
> partialLinkFromValues _ = error "Invalid link returned from database."

> getPartialLink :: Integer -> App PartialLink
> getPartialLink linkNo = do
>   c <- asks db
>   t <- liftIO $ queryTuple c "SELECT link_no, origin, destination, link_type \
>                              \FROM link WHERE link_no = ?" [toSql linkNo]
>                   `catchSqlE` "Link not found."
>   return $ partialLinkFromValues t

> getLink :: Integer -> App Link
> getLink linkNo = do
>   l' <- getPartialLink linkNo
>   getLinkFromPartial l'

Return the types of links sorted by how common they should be.

Eventually we'll want to cache this.

> linkTypes :: App [String]
> linkTypes = do
>   c <- asks db
>   types <- liftIO $ queryAttribute c
>     "SELECT name FROM link_type LEFT OUTER JOIN \
>     \(SELECT link_type, COUNT(*) AS count FROM link \
>      \GROUP BY link_type) AS t ON (t.link_type = link_type.name) \
>     \ORDER BY t.count DESC NULLS LAST" []
>    `catchSqlE` "Failed to retrieve link types."
>   return $ map fromSql types

> newLinkPage :: App CGIResult
> newLinkPage = do
>   origin <- encodeString `liftM` getRequiredInput "origin"
>   destination <- encodeString `liftM` getRequiredInput "destination"
>   types <- linkTypes
>   let t = origin ++ " -> " ++ destination
>   stdPage t [CSS "link", JS "MochiKit", JS "link"]
>     [ form ! [action "", method "post"] <<
>        [ thediv ! [identifier "baseline", theclass "link"] <<
>            linkHtml (stringToHtml origin) (stringToHtml destination),
>          thediv ! [identifier "link-details"] <<
>            ([ simpleChoice "link-type" types, br ] ++
>             newLinkHtml (head types) origin destination ++
>             [ br,
>               submit "" "Associate" ]) ] ]

> establishLink :: Link -> Integer -> App (Maybe Integer)
> establishLink (Link _ linkType origin destination) memberNo = do
>   c' <- asks db
>   liftIO $ withTransaction c' $ \c -> do
>     linkNo <- insertNo c "INSERT INTO link (origin, destination, link_type, \
>                                            \language, author) \
>                          \VALUES (?, ?, ?, 'en', ?)"
>                          [toSql origin, toSql destination, toSql $ linkTypeName linkType,
>                           toSql memberNo]
>                          "link_link_no_seq"
>     case linkNo of
>       Nothing -> rollback c >> return Nothing
>       Just n  -> do n' <- establishLinkType c n linkType
>                     if n' == 1
>                        then return $ Just n
>                        else return Nothing
>    `catchSqlE` "Failed to establish link."

> linkLexemes :: App CGIResult
> linkLexemes =
>   withMemberNumber $ \memberNo -> do
>     link <- linkFromForm
>     linkNo <- establishLink link memberNo
>     case linkNo of
>       Just n  -> redirect $ "/link/" ++ (show n)
>       Nothing -> error "Failed to establish link."

> linkPage :: Integer -> App CGIResult
> linkPage linkNo = do
>   memberNo <- asks memberNumber
>   (Link _ linkType origin destination) <- getLink linkNo
>   c <- asks db
>   owner <- liftIO $ liftM fromSql $ liftM fromJust $ queryValue c
>     "SELECT author = ? FROM link WHERE link_no = ?"
>     [toSql memberNo, toSql linkNo]
>   review <- reviewHtml linkNo
>   ops <- linkOperations linkNo owner
>   let t = (encodeString origin) ++ " -> " ++ (encodeString destination)
>   stdPage t [CSS "link"]
>     [ review,
>       ops,
>       thediv ! [identifier "baseline", theclass "link"] <<
>         ([linkHtml (stringToHtml $ encodeString origin) (stringToHtml $ encodeString destination)] ++
>          linkTypeHtml linkType) ]

> linkOperations :: Integer -> Bool -> App Html
> linkOperations n True = do
>   c <- asks db
>   deleted <- liftIO $ queryValue c "SELECT deleted FROM link \
>                                    \WHERE link_no = ?" [toSql n]
>   case fromSql `liftM` deleted of
>     Just True -> return $ stringToHtml "Deleted"
>     _         -> return $ form ! [action ("/link/" ++ (show n) ++ "/delete"), method "post"] <<
>                    submit "" "Delete"
> linkOperations _ False = return noHtml

Generate a page of links for the specified member or all members (for Nothing).

> linksPage :: App CGIResult
> linksPage = do
>   pg  <- readInputDefault 1 "pg"
>   n   <- readInputDefault 10 "n"
>   links <- getLinks ((pg - 1) * n) (n + 1)
>   pagerControl <- pager n pg $ (length links) + ((pg - 1) * n)
>   stdPage "Links" [CSS "link"]
>     [ (take n $ map displayLink links) +++ pagerControl ]

> getLinks :: Int -> Int -> App [[SqlValue]]
> getLinks offset limit = do
>   c <- asks db
>   liftIO $ quickQuery' c "SELECT link_no, origin, destination FROM link \
>                          \WHERE deleted = FALSE \
>                          \ORDER BY link_no OFFSET ? LIMIT ?"
>                          [toSql offset, toSql limit]
>              `catchSqlE` "Failed to retrieve links."

> displayLink :: [SqlValue] -> Html
> displayLink [no, origin, destination] = 
>   let no' = fromSql no :: Integer
>       origin' = fromSql origin :: String
>       destination' = fromSql destination :: String in
>   thediv ! [theclass "link"] << anchor ! [href $ "/link/" ++ (show no')] <<
>     linkHtml (stringToHtml (encodeString origin')) (stringToHtml (encodeString destination'))
> displayLink _ = thediv ! [theclass "link"] << "Link is malformed."

> deleteLink :: Integer -> App CGIResult
> deleteLink linkNo = do
>   ref <- refererOrVocabulink
>   c <- asks db
>   liftIO $ quickStmt c "UPDATE link SET deleted = TRUE \
>                        \WHERE link_no = ?" [toSql linkNo]
>              `catchSqlE` "Failed to delete link."
>   redirect ref

We'll stick to just searching through 10 results per page for now.

> searchPage :: App CGIResult
> searchPage = do
>   term <- getRequiredInput "q"
>   let n = 10
>   pg  <- readInputDefault 1 "pg"
>   links <- searchLinks term ((pg - 1) * n) (n + 1)
>   pagerControl <- pager n pg $ (length links) + ((pg - 1) * n)
>   case links of
>     [] -> redirect $ "/lexeme/" ++ (encodeString term)
>     _  -> stdPage "Search Results" [CSS "link"]
>             [ h1 << "Search Results",
>              (take n $ map displayLink links) +++ pagerControl ]

This is a basic search that searches through the origin and destination names
of the links in the database.

> searchLinks :: String -> Int -> Int -> App [[SqlValue]]
> searchLinks term offset limit = do
>   c <- asks db
>   liftIO $ quickQuery' c "SELECT link_no, origin, destination \
>                          \FROM link WHERE (origin = ? OR destination = ?) \
>                          \AND deleted = FALSE \
>                          \OFFSET ? LIMIT ?"
>                          [toSql term, toSql term, toSql offset, toSql limit]
>              `catchSqlE` "Failed to search links."
