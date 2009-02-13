> module Vocabulink.Link (lexemePage, linkHtml, getLink, linkPage, newLinkPage,
>                         linkLexemes, searchPage, deleteLink, linksPage,
>                         Link(..), partialLinkFromValues) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Review.Html (reviewHtml)
> import Vocabulink.Link.Types (Link(..), newLinkHtml, linkFromForm,
>                               linkTypeName, establishLinkType,
>                               getLinkFromPartial, linkTypeHtml,
>                               PartialLink(..), getPartialLinkType)
> import Vocabulink.Utils

When retrieving the page for a lexeme, we first check to see if a lemma for
this lexeme is defined. If not, we assume it to be canonical.

> lexemePage :: String -> App CGIResult
> lexemePage l = do
>   lemma <- queryValue'  "SELECT lemma FROM lexeme \
>                         \WHERE lexeme = ?" [toSql l]
>   case lemma of
>     Nothing         -> error "Database failure when looking for the lexeme."
>     Just (Just lm)  -> redirect $ "/lexeme/" ++ encodeString (fromSql lm)
>     Just Nothing    -> stdPage (encodeString l) [CSS "link"]
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

> partialLinkFromValues :: [SqlValue] -> Maybe PartialLink
> partialLinkFromValues (n:o:d:t:[]) =
>   let partialLinkType = getPartialLinkType (fromSql t)
>   in Just $ PartialLink (fromSql n) partialLinkType (fromSql o) (fromSql d)
> partialLinkFromValues _ = Nothing

> getPartialLink :: Integer -> App (Maybe PartialLink)
> getPartialLink linkNo = do
>   t <- queryTuple' "SELECT link_no, origin, destination, link_type \
>                    \FROM link WHERE link_no = ?" [toSql linkNo]
>   return $ maybe Nothing partialLinkFromValues t

> getLink :: Integer -> App (Maybe Link)
> getLink linkNo = do
>   l' <- getPartialLink linkNo
>   maybe (return Nothing) getLinkFromPartial l'

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
>        [  thediv ! [identifier "baseline", theclass "link"] <<
>             linkHtml (stringToHtml origin) (stringToHtml destination),
>           thediv ! [identifier "link-details"] <<
>             (  [  select ! [identifier "link-type", name "link-type"] << options types,
>                   br ] ++
>                newLinkHtml (head types) origin destination ++
>                [  br,
>                   submit "" "Associate" ]) ] ]

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
>   withRequiredMemberNumber $ \memberNo -> do
>     link <- linkFromForm
>     linkNo <- establishLink link memberNo
>     case linkNo of
>       Just n  -> redirect $ "/link/" ++ (show n)
>       Nothing -> error "Failed to establish link."

> linkPage :: Integer -> App CGIResult
> linkPage linkNo = do
>   memberNo <- asks memberNumber
>   l <- getLink linkNo
>   case l of
>     Nothing -> output404 ["Link not found."]
>     Just (Link _ linkType origin destination) -> do
>       review <- reviewHtml linkNo
>       owner <- queryValue'  "SELECT author = ? FROM link WHERE link_no = ?"
>                             [toSql memberNo, toSql linkNo]
>       ops <- case owner of
>                Nothing        -> return $ stringToHtml
>                                    "Unable to determine link ownership."
>                Just Nothing   -> return $ stringToHtml
>                                    "Unable to determine link ownership."
>                Just (Just o)  -> linkOperations linkNo (isJust memberNo && fromSql o)
>       let t = (encodeString origin) ++ " -> " ++ (encodeString destination)
>       stdPage t [CSS "link"]
>         [ review,
>           ops,
>           thediv ! [identifier "baseline", theclass "link"] <<
>             ([linkHtml (stringToHtml $ encodeString origin) (stringToHtml $ encodeString destination)] ++
>              linkTypeHtml linkType) ]

> linkOperations :: Integer -> Bool -> App Html
> linkOperations n True = do
>   deleted <- queryValue'  "SELECT deleted FROM link \
>                           \WHERE link_no = ?" [toSql n]
>   case deleted of
>     Just (Just d)  -> if fromSql d
>                         then return $ stringToHtml "Deleted"
>                         else return $ form ! [action ("/link/" ++ (show n) ++ "/delete"), method "post"] <<
>                                submit "" "Delete"
>     _              -> return $ stringToHtml
>                         "Unable to determine whether or not link has been deleted."
> linkOperations _ False = return noHtml

Generate a page of links for the specified member or all members (for Nothing).

> linksPage :: App CGIResult
> linksPage = do
>   (pg, n, offset) <- currentPage
>   links <- getLinks offset (n + 1)
>   pagerControl <- pager pg n $ offset + (length links)
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

> searchPage :: App CGIResult
> searchPage = do
>   term <- getRequiredInput "q"
>   (pg, n, offset) <- currentPage
>   links <- searchLinks term offset (n + 1)
>   pagerControl <- pager pg n $ offset + (length links)
>   case links of
>     []  -> redirect $ "/lexeme/" ++ (encodeString term)
>     _   -> stdPage "Search Results" [CSS "link"]
>              [  h1 << "Search Results",
>                 (take n $ map displayLink links) +++ pagerControl ]

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
