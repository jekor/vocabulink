\section{Links}

Links are the center of interest in our program. Most activities revolve around
them.

> module Vocabulink.Link (  Link(..), PartialLink(..), getLink, getPartialLink,
>                           lexemePage, linkHtml, linkPage, newLinkPage,
>                           linkLexemes, searchPage, deleteLink, linksPage,
>                           partialLinkHtml, partialLinkFromValues) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Review.Html
> import Vocabulink.Utils

Abstractly, a link is defined by the origin and destination lexemes it links as
well as its type. Practically, we also need to carry around information such as
its link number (in the database) as well as a string representation of its
type (for partially constructed links, which you'll see later).

> data Link = Link {  linkNumber       :: Integer,
>                     linkTypeName     :: String,
>                     linkOrigin       :: String,
>                     linkDestination  :: String,
>                     linkType         :: LinkType }

We can associate 2 lexemes in many different ways. Because different linking
methods require different information, they each need different representations
in the database. This leads to some additional complexity.

Each link between lexemes has a type. This type determines how the link is
displayed, edited, used in statistical analysis, etc. See the handbook for a
more in-depth description of the types.

> data LinkType =  Association | Cognate | LinkWord String String |
>                  Relationship String String
>                  deriving (Show, Eq)

Fully loading a link from the database requires 2 database queries. But we
don't always need the type-specific data associated with a link. Sometimes it's
not even possible to have it, such as during interactive link construction with
a member.

We'll use a separate type to represent this. Essentially it's a link with an
undefined linkType. We use a separate type to avoid passing a partial link to a
function that expects a fully-instantiated link. The only danger here is
writing a function that accepts a partial link and then trying to access the
linkType information.

> newtype PartialLink = PartialLink Link

\subsection{Storing and Retrieving Links}

We'll look at retrieving links first, as it's a little more straightforward
than storing links to the database.

Retrieving a partial link is simple.

> getPartialLink :: Integer -> App (Maybe PartialLink)
> getPartialLink linkNo = do
>   t <- queryTuple'  "SELECT link_no, link_type, origin, destination \
>                     \FROM link WHERE link_no = ?" [toSql linkNo]
>   return $ partialLinkFromValues =<< t

We use a helper function to convert the raw SQL tuple to a partial link value
since we need to do so in our next function. Note that we leave the link's
linkType undefined.

> partialLinkFromValues :: [SqlValue] -> Maybe PartialLink
> partialLinkFromValues [n, t, o, d] = Just $
>   PartialLink $ Link {  linkNumber       = fromSql n,
>                         linkTypeName     = fromSql t,
>                         linkOrigin       = fromSql o,
>                         linkDestination  = fromSql d,
>                         linkType         = undefined }
> partialLinkFromValues _  = Nothing

Here we retrieve multiple links at once. This was the original motivation for
diving links into full links and partial links. Often we need to retrieve links
for display but we don't need or want extra trips to the database. Here we need
1 query instead of potentially @limit@ queries.

This assumes the ordering of links is determined by link number. It's used by
the page which displays a listing of links. We don't want to display deleted
links (which are left in the database for people still reviewing them).

> getPartialLinks :: App (Maybe [Maybe PartialLink])
> getPartialLinks = do
>   ts <- queryTuples  "SELECT link_no, link_type, origin, destination \
>                      \FROM link WHERE deleted = FALSE \
>                      \ORDER BY link_no" []
>   return $ map partialLinkFromValues `liftM` ts

Once we have a partial link, it's a simple matter to turn it into a full link.
We just need to retrieve its type-level details from the database.

> getLinkFromPartial :: PartialLink -> App (Maybe Link)
> getLinkFromPartial (PartialLink partial) = do
>   linkT <- getLinkType (PartialLink partial)
>   return $ (\t -> Just $ partial {linkType = t}) =<< linkT

Each type is expected to be potentially different enough to require its own
database schema for representation. We could attempt to use PostgreSQL's
inheritance features, but I've decided to handle the difference between types
at the Haskell layer for now.

> getLinkType :: PartialLink -> App (Maybe LinkType)
> getLinkType (PartialLink p) = case p of
>   (Link { linkTypeName  = "association" })  -> return $ Just Association
>   (Link { linkTypeName  = "cognate"})       -> return $ Just Cognate
>   (Link { linkTypeName  = "link word",
>           linkNumber    = n })              -> do
>     rs <- queryTuple'  "SELECT link_word, story FROM link_type_link_word \
>                        \WHERE link_no = ?" [toSql n]
>     case rs of
>       Just [linkWord, story]  -> return $ Just $ LinkWord (fromSql linkWord) (fromSql story)
>       _                       -> return Nothing
>   (Link { linkTypeName  = "relationship",
>           linkNumber    = n })              -> do
>     rs <- queryTuple'  "SELECT left_side, right_side FROM link_type_relationship \
>                        \WHERE link_no = ?" [toSql n]
>     case rs of
>       Just [left, right]  -> return $ Just $ Relationship (fromSql left) (fromSql right)
>       _                   -> return Nothing
>   _                                         -> error "Bad partial link."

> getLink :: Integer -> App (Maybe Link)
> getLink linkNo = do
>   l <- getPartialLink linkNo
>   maybe (return Nothing) getLinkFromPartial l

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
>       [  form ! [action "/link", method "get"] <<
>          [  hidden "origin" (encodeString l),
>             thediv ! [identifier "baseline", theclass "link"] <<
>               linkHtml (stringToHtml l)
>                        (textfield "destination" +++
>                         submit "" "link") ] ]

Here's the simplest HTML representation of a link we use.

Origin and destination should already be UTF-8 encoded.

> linkHtml :: Html -> Html -> Html
> linkHtml orig dest = concatHtml
>   [ thespan ! [theclass "lexeme"] << orig,
>     image ! [src "http://s.vocabulink.com/black.png", width "20%", height "1"],
>     thespan ! [theclass "lexeme"] << dest ]

> linkHtml' :: String -> String -> Html
> linkHtml' orig dest = linkHtml (stringToHtml $ encodeString orig)
>                                (stringToHtml $ encodeString dest)

> newLinkPage :: App CGIResult
> newLinkPage = do
>   orig  <- getRequiredInput "origin"
>   dest  <- getRequiredInput "destination"
>   types <- linkTypes
>   case types of
>     Just types'@(_:_)  -> do
>       let t = orig ++ " -> " ++ dest
>       stdPage t [CSS "link", JS "MochiKit", JS "link"]
>         [ form ! [action "", method "post"] <<
>            [  thediv ! [identifier "baseline", theclass "link"] <<
>                 linkHtml' orig dest,
>               thediv ! [identifier "link-details"] <<
>                 (  [  select ! [identifier "link-type", name "link-type"] << options types',
>                       br ] ++
>                    newLinkHtml (head types') orig dest ++
>                    [  br,
>                       submit "" "Associate" ]) ] ]
>     _                  -> error "Failed to retrieve link types."

> establishLink :: Link -> Integer -> App (Maybe Integer)
> establishLink l memberNo = do
>   c' <- asks db
>   liftIO $ withTransaction c' $ \c -> do
>     linkNo <- insertNo c "INSERT INTO link (origin, destination, link_type, \
>                                            \language, author) \
>                          \VALUES (?, ?, ?, 'en', ?)"
>                          [  toSql (linkOrigin l),
>                             toSql (linkDestination l),
>                             toSql (linkTypeName l),
>                             toSql memberNo ]
>                          "link_link_no_seq"
>                 `catchSqlE` "Failed to establish link."
>     case linkNo of
>       Nothing -> rollback c >> return Nothing
>       Just n  -> do r <- establishLinkType c (l {linkNumber = n})
>                     return $ (\_ -> Just n) =<< r

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
>     Nothing  -> output404 ["Link not found."]
>     Just l'  -> do
>       review <- reviewHtml linkNo
>       owner <- queryValue'  "SELECT author = ? FROM link WHERE link_no = ?"
>                             [toSql memberNo, toSql linkNo]
>       ops <- case owner of
>                Just (Just o)  -> linkOperations linkNo (isJust memberNo && fromSql o)
>                _              -> return $ stringToHtml
>                                    "Unable to determine link ownership."
>       let orig  = encodeString $ linkOrigin l'
>           dest  = encodeString $ linkDestination l'
>       stdPage (orig ++ " -> " ++ dest) [CSS "link"]
>         [ review,
>           ops,
>           thediv ! [identifier "baseline", theclass "link"] <<
>             (  [linkHtml' orig dest] ++
>                linkTypeHtml (linkType l')) ]

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
>                         "Can't determine whether or not link has been deleted."
> linkOperations _ False = return noHtml

> linksPage :: App CGIResult
> linksPage = do
>   partials <- getPartialLinks
>   case partials of
>     Nothing  -> error "Unable to retrieve links."
>     Just ls  -> do
>       (pg, n, offset) <- currentPage
>       let ls' = take (n + 1) $ drop offset ls
>       pagerControl <- pager pg n $ offset + (length ls')
>       simplePage "Links" [CSS "link"]
>         [ map displayPossiblePartial (take n ls') +++ pagerControl ]
>           where displayPossiblePartial =
>                   maybe (paragraph << "Error retrieving link.")
>                         displayPartialHtml

> displayPartialHtml :: PartialLink -> Html
> displayPartialHtml l = thediv ! [theclass "link"] << (partialLinkHtml l)

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
>   links <- searchLinks term
>   case links of
>     Nothing  -> error "Error while searching links."
>     Just []  -> redirect $ "/lexeme/" ++ (encodeString term)
>     Just ls  -> do
>       (pg, n, offset) <- currentPage
>       let ls' = take (n + 1) $ drop offset ls
>       pagerControl <- pager pg n $ offset + (length ls')
>       stdPage "Search Results" [CSS "link"]
>         [  h1 << "Search Results",
>            map displayPossiblePartial (take n ls') +++ pagerControl ]
>           where displayPossiblePartial =
>                   maybe (paragraph << "Error retrieving link.")
>                         displayPartialHtml

This is a basic search that searches through the origin and destination names
of the links in the database.

> searchLinks :: String -> App (Maybe [Maybe PartialLink])
> searchLinks term = do
>   ts <- queryTuples  "SELECT link_no, link_type, origin, destination \
>                      \FROM link \
>                      \WHERE (origin = ? OR destination = ?) \
>                        \AND deleted = FALSE" [toSql term, toSql term]
>   return $ map partialLinkFromValues `liftM` ts

Return the types of links sorted by how common they should be. Eventually we'll
want to cache this.

> linkTypes :: App (Maybe [String])
> linkTypes = do
>   types <- queryAttribute'
>     "SELECT name FROM link_type LEFT OUTER JOIN \
>      \(SELECT link_type, COUNT(*) AS count FROM link \
>       \GROUP BY link_type) AS t ON (t.link_type = link_type.name) \
>     \ORDER BY t.count DESC NULLS LAST" []
>   return $ map fromSql `liftM` types

This takes a connection because it will be used inside of a transaction.

A more elegant solution for the future would be to locally modify the reader to
use the transaction handle.

> establishLinkType :: IConnection conn => conn -> Link -> IO (Maybe ())
> establishLinkType c l = case linkType l of
>   Association                -> return $ Just ()
>   Cognate                    -> return $ Just ()
>   (LinkWord word story)      -> do
>     r <- quickStmt c  "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                          \VALUES (?, ?, ?)"
>                       [toSql (linkNumber l), toSql word, toSql story]
>            `catchSqlE` "Failed to establish link."
>     return $ Just r
>   (Relationship left right)  -> do
>     r <- quickStmt c  "INSERT INTO link_type_relationship (link_no, left_side, right_side) \
>                                                   \VALUES (?, ?, ?)"
>                       [toSql (linkNumber l), toSql left, toSql right]
>            `catchSqlE` "Failed to establish link."
>     return $ Just r

> linkEditHtml :: Link -> [Html]
> linkEditHtml l = case linkType l of
>   Association                -> []
>   Cognate                    -> []
>   (LinkWord word story)      ->
>     [  label << "link word:",
>        textfield "link-word" ! [value word], br,
>        textarea ! [name "story", cols "80", rows "20"] << story ]
>   (Relationship left right)  ->
>     [  textfield "left-side" ! [value left],
>        stringToHtml " is to ",
>        textfield "right-side" ! [value right], br,
>        stringToHtml $ " as " ++ orig ++ " is to " ++ dest ]
>     where orig  = encodeString $ linkOrigin l
>           dest  = encodeString $ linkDestination l

> partialLinkHtml :: PartialLink -> Html
> partialLinkHtml (PartialLink l) =
>   anchor ! [href ("/link/" ++ (show $ linkNumber l))] << (linkOrigin l ++ (encodeString " -- ") ++ linkDestination l)

> linkTypeHtml :: LinkType -> [Html]
> linkTypeHtml Association = []
> linkTypeHtml Cognate = []
> linkTypeHtml (LinkWord linkWord story) =
>   [  paragraph << ("link word: " ++ (encodeString linkWord)), br,
>      paragraph << story]
> linkTypeHtml (Relationship leftSide rightSide) =
>   [ paragraph << ((encodeString leftSide) ++ " -> " ++ (encodeString rightSide)) ]

> linkFromForm :: App Link
> linkFromForm = do
>   orig      <- getRequiredInput "origin"
>   dest      <- getRequiredInput "destination"
>   typeName  <- getRequiredInput "link-type"
>   linkT     <- linkTypeFromForm typeName
>   return $ Link {  linkNumber       = undefined,
>                    linkTypeName     = typeName,
>                    linkOrigin       = orig,
>                    linkDestination  = dest,
>                    linkType         = linkT }

> linkTypeFromForm :: String -> App LinkType
> linkTypeFromForm "association"  = return Association
> linkTypeFromForm "cognate"      = return Cognate
> linkTypeFromForm "link word"    = do
>   linkWord  <- getRequiredInput "link-word"
>   story     <- getRequiredInput "story"
>   return $ LinkWord linkWord story
> linkTypeFromForm "relationship" = do
>   leftSide   <- getRequiredInput "left-side"
>   rightSide  <- getRequiredInput "right-side"
>   return $ Relationship leftSide rightSide
> linkTypeFromForm _ = error "Unknown link type"

> newLinkHtml :: String -> String -> String -> [Html]
> newLinkHtml def orig dest =
>   let sparseLink = Link {  linkNumber       = undefined,
>                            linkTypeName     = undefined,
>                            linkOrigin       = orig,
>                            linkDestination  = dest,
>                            linkType         = undefined } in
>     map editBlock
>       [  sparseLink {  linkTypeName  = "association",
>                        linkType      = Association },
>          sparseLink {  linkTypeName  = "cognate",
>                        linkType      = Cognate },
>          sparseLink {  linkTypeName  = "link word",
>                        linkType      = LinkWord "" "Write a story linking the 2 words here." },
>          sparseLink {  linkTypeName  = "relationship",
>                        linkType      = Relationship "" "" } ]
>            where editBlock :: Link -> Html
>                  editBlock link = thediv !
>                    [  identifier $ safeID $ (linkTypeName link),
>                       theclass "link-editor",
>                       thestyle $ def == linkTypeName link ? "" $ "display: none"] << linkEditHtml link
