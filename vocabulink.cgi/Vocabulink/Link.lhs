\section{Links}

Links are the center of interest in our program. Most activities revolve around
them.

> module Vocabulink.Link (  Link(..), PartialLink(..), LinkType(..),
>                           getPartialLink, getPartialLinks,
>                           getLinkFromPartial, getLink, establishLink,
>                           linkHtml, linkPage, linkLexemes, deleteLink,
>                           linksPage, linksContainingPage, newLink,
>                           partialLinkHtml, partialLinkFromValues) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Review.Html
> import Vocabulink.Utils

> import qualified Text.XHtml.Strict.Formlets as F

\subsection{Link Data Types}

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
>                  deriving (Show)

Sometimes we need to work with a more readable name, such as interacting with a
client or the database.

> linkTypeNameFromType :: LinkType -> String
> linkTypeNameFromType Association         = "association"
> linkTypeNameFromType Cognate             = "cognate"
> linkTypeNameFromType (LinkWord _ _)      = "link word"
> linkTypeNameFromType (Relationship _ _)  = "relationship"

Fully loading a link from the database requires a join. However, the join
depends on the type of thi link. But we don't always need the type-specific
data associated with a link. Sometimes it's not even possible to have it, such
as during interactive link construction with a member.

We'll use a separate type to represent this. Essentially it's a link with an
undefined linkType. We use a separate type to avoid passing a partial link to a
function that expects a fully-instantiated link. The only danger here is
writing a function that accepts a partial link and then trying to access the
linkType information.

> newtype PartialLink = PartialLink Link

\subsection{Storing and Retrieving Links}

We refer to storing a link as ``establishing'' the link.

Each link type is expected to be potentially different enough to require its
own database schema for representation. We could attempt to use PostgreSQL's
inheritance features, but I've decided to handle the difference between types
at the Haskell layer for now. I'm actually hesitant to use separate tables for
separate types as it feels like I'm breaking the relational model. However, any
extra efficiency for study outranks implementation elegance (correctness?).

Establishing a link requires a member number since all links must be owned by a
member.

Since we need to store the link in 2 different tables, we use a transaction.
Our App-level database functions do not yet support transactions, so we'll have
to handle them manually for now. You'll also notice that some link types have
no additional information and hence no table in the database.

This returns the newly established link number.

> establishLink :: Link -> Integer -> App (Maybe Integer)
> establishLink l memberNo = do
>   c' <- asks db
>   liftIO $ withTransaction c' $ \c -> do
>     linkNo <- insertNo c
>       "INSERT INTO link (origin, destination, link_type, language, author) \
>                 \VALUES (?, ?, ?, 'en', ?)"
>       [  toSql (linkOrigin l), toSql (linkDestination l),
>          toSql (linkTypeName l), toSql memberNo ]
>       "link_link_no_seq" `catchSqlD` Nothing
>     case linkNo of
>       Nothing -> rollback c >> return Nothing
>       Just n  -> do r <- establishLinkType c (l {linkNumber = n})
>                     return $ r >>= \_ -> Just n

The table we insert additional details into depends on the type of the link and
it's easiest to use a separate function for it.

This takes a connection because it will actually accept a transaction. A more
elegant solution for the future would be to locally modify the reader to use
the transaction handle.

> establishLinkType :: IConnection conn => conn -> Link -> IO (Maybe ())
> establishLinkType c l = case linkType l of
>   Association                -> return $ Just ()
>   Cognate                    -> return $ Just ()
>   (LinkWord word story)      -> (do
>     quickStmt c
>       "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                \VALUES (?, ?, ?)"
>       [toSql (linkNumber l), toSql word, toSql story]
>     return $ Just ()) `catchSqlD` Nothing
>   (Relationship left right)  -> (do
>     quickStmt c
>       "INSERT INTO link_type_relationship (link_no, left_side, right_side) \
>                                   \VALUES (?, ?, ?)"
>       [toSql (linkNumber l), toSql left, toSql right]
>     return $ Just ()) `catchSqlD` Nothing

Now that we've seen how we store links, let's look at retrieving them (which is
slightly more complicated in order to allow for efficient retrieval of multiple
links).

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
dividing links into full links and partial links. Often we need to retrieve
links for display but we don't need or want extra trips to the database. Here
we need 1 query instead of potentially @limit@ queries.

This assumes the ordering of links is determined by link number. It's used by
the page which displays a listing of links. We don't want to display deleted
links (which are left in the database for people still reviewing them).

This takes a list of predicates (as SQL strings) to filter the results by. It's
dangerous, to do string munging like this, but perhaps equally dangerous to
have multiple similar functions go out of sync.

> getPartialLinks ::  [String] -> [SqlValue] -> Int -> Int ->
>                     App (Maybe [Maybe PartialLink])
> getPartialLinks preds replacements offset limit = do
>   ts <- queryTuples'  ("SELECT link_no, link_type, origin, destination \
>                        \FROM link WHERE NOT deleted " ++ conditions ++
>                        "OFFSET ? LIMIT ?")
>                       (replacements ++ [toSql offset, toSql limit])
>   return $ map partialLinkFromValues `liftM` ts
>     where conditions = concatMap ("AND " ++) preds

Once we have a partial link, it's a simple matter to turn it into a full link.
We just need to retrieve its type-level details from the database.

> getLinkFromPartial :: PartialLink -> App (Maybe Link)
> getLinkFromPartial (PartialLink partial) = do
>   linkT <- getLinkType (PartialLink partial)
>   return $ (\t -> Just $ partial {linkType = t}) =<< linkT

> getLinkType :: PartialLink -> App (Maybe LinkType)
> getLinkType (PartialLink p) = case p of
>   (Link {  linkTypeName  = "association" })  -> return $ Just Association
>   (Link {  linkTypeName  = "cognate"})       -> return $ Just Cognate
>   (Link {  linkTypeName  = "link word",
>            linkNumber    = n })              -> do
>     rs <- queryTuple'  "SELECT link_word, story FROM link_type_link_word \
>                        \WHERE link_no = ?" [toSql n]
>     case rs of
>       Just [linkWord, story]  -> return $ Just $
>         LinkWord (fromSql linkWord) (fromSql story)
>       _                       -> return Nothing
>   (Link {  linkTypeName  = "relationship",
>            linkNumber    = n })              -> do
>     rs <- queryTuple'  "SELECT left_side, right_side FROM link_type_relationship \
>                        \WHERE link_no = ?" [toSql n]
>     case rs of
>       Just [left, right]  -> return $ Just $
>         Relationship (fromSql left) (fromSql right)
>       _                   -> return Nothing
>   _                                         -> error "Bad partial link."

We now have everything we need to retrieve a full link in 1 step.

> getLink :: Integer -> App (Maybe Link)
> getLink linkNo = do
>   l <- getPartialLink linkNo
>   maybe (return Nothing) getLinkFromPartial l

\subsection{Link Pages}

Here's the simplest HTML representation of a link we use.

Origin and destination should already be UTF-8 encoded.

> linkHtml :: Html -> Html -> Html
> linkHtml orig dest = concatHtml
>   [ thespan ! [theclass "lexeme"] << orig,
>     image ! [src "http://s.vocabulink.com/edge.png", width "20%", height "1"],
>     thespan ! [theclass "lexeme"] << dest ]

> linkHtml' :: String -> String -> Html
> linkHtml' orig dest = linkHtml (stringToHtml $ encodeString orig)
>                                (stringToHtml $ encodeString dest)

> linkLexemes :: Link -> App CGIResult
> linkLexemes link =
>   withRequiredMemberNumber $ \memberNo -> do
>     linkNo <- establishLink link memberNo
>     case linkNo of
>       Just n   -> redirect $ "/link/" ++ (show n)
>       Nothing  -> error "Failed to establish link."

> linkPage :: Integer -> App CGIResult
> linkPage linkNo = do
>   memberNo <- asks memberNumber
>   l <- getLink linkNo
>   case l of
>     Nothing  -> output404 ["link", show linkNo]
>     Just l'  -> do
>       review <- reviewHtml linkNo
>       owner <- queryValue'  "SELECT author = ? FROM link WHERE link_no = ?"
>                             [toSql memberNo, toSql linkNo]
>       ops <- case owner of
>                Just (Just o)  -> linkOperations linkNo (isJust memberNo && fromSql o)
>                _              -> return $ stringToHtml
>                                    "Unable to determine link ownership."
>       let orig  = linkOrigin l'
>           dest  = linkDestination l'
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
>                         else return $ form ! [action ("/link/" ++ (show n) ++ "/delete"), method "POST"] <<
>                                submit "" "Delete"
>     _              -> return $ stringToHtml
>                         "Can't determine whether or not link has been deleted."
> linkOperations _ False = return noHtml

If we found no links matching the description, display the search term on its
own in the center of the screen with ``create link'' buttons on either side.

> linksPage :: App CGIResult
> linksPage  = do
>   (pg, n, offset) <- currentPage
>   partials <- getPartialLinks [] [] offset (n + 1)
>   case partials of
>     Nothing  -> error "Error while retrieving links."
>     Just ps  -> do
>       pagerControl <- pager pg n $ offset + (length ps)
>       simplePage "Links" [CSS "link"]
>         [ map displayPossiblePartial (take n ps) +++ pagerControl ]
>    where displayPossiblePartial =
>            maybe (paragraph << "Error retrieving link.") displayPartialHtml

> linksContainingPage :: String -> App CGIResult
> linksContainingPage focus = do
>   partials <- getPartialLinks preds [] 0 100
>   case partials of
>     Nothing  -> error "Error while retrieving links."
>     Just ls  -> stdPage ("Links containing " ++ focus) [CSS "link"]
>       [linkFocusBox focus ls]
>    where preds = ["(origin LIKE '" ++ focus ++ "' OR \
>                    \destination LIKE '" ++ focus ++ "')"]

> linkFocusBox :: String -> [Maybe PartialLink] -> Html
> linkFocusBox focus []  = table ! [theclass "focus-box"] <<
>   [  tbody << tr <<
>      [  td ! [theclass "origins"] << form ! [action "/link", method "GET"] <<
>           button ! [name "input1", value (encodeString focus)] << "New Link",
>         td ! [theclass "edges"] <<
>           image ! [src "http://s.vocabulink.com/edge.png",
>                    width "200", height "1"],
>         td ! [theclass "focus"] << (encodeString focus),
>         td ! [theclass "edges"] <<
>           image ! [src "http://s.vocabulink.com/edge.png",
>                    width "200", height "1"],
>         td ! [theclass "destinations"] << form ! [action "/link", method "GET"] <<
>           button ! [name "input0", value (encodeString focus)] << "New Link" ] ]
> linkFocusBox _ _   = error "unsupported"

> newLink :: App CGIResult
> newLink = do
>   ts   <- linkTypes
>   case ts of
>     Nothing   -> error "Unable to retrieve link types."
>     Just ts'  -> do
>       res  <- runForm (establish ts') "Link"
>       case res of
>         Left xhtml  -> simplePage "Create a Link" [] [xhtml]
>         Right link  -> linkLexemes link

> establish :: [String] -> AppForm Link
> establish ts = mkLink  <$> linkNodeInput "Origin" <*> linkNodeInput "Destination"
>                        <*> linkTypeInput ts

> mkLink :: String -> String -> LinkType -> Link
> mkLink orig dest t = Link {  linkNumber       = undefined,
>                              linkTypeName     = linkTypeNameFromType t,
>                              linkOrigin       = orig,
>                              linkDestination  = dest,
>                              linkType         = t }

> formLabel' :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel' text = plug (\xhtml -> label << (text ++ ": ") +++ xhtml)

TODO: Check that the input is more than just whitespace.

> linkNodeInput :: String -> AppForm String
> linkNodeInput l = l `formLabel'` F.input Nothing `check` ensures
>   [  ((/= "")           , l ++ " is required."),
>      ((<= 64) . length  , l ++ " must be 64 characters or shorter.") ]

We have a bit of a challenge with link types. We want the form to adjust
dynamically using JavaScript when a member chooses one of the link types from a
select list. But we also want form validation using formlets. Formlets would be
rather straightforward if we were using a 2-step process (choose the link type,
submit, fill in the link details, submit). But it's important enough that
members can quickly enter new links that we're making it a 1-step process.

The idea is to generate all the form fields for every possible link type in
advance, with a default hidden state. Then JavaScript will reveal the
appropriate fields when a link type is chosen. Upon submit, all link type
fields for all but the selected link type will be empty (or unnecessary). When
running the form, we will instantiate all of them, but then |linkTypeS| will
select just the appropriate one based on the @<select>@.

The main challenge here is that we can't put the validation in the link types
themselves. We have to move it into |linkTypeInput|. The problem comes from
either my lack of understanding of Applicative Functors, or the fact that by
the time the formlet combination strategy (Failure) the unused link types have
already generated failure because they have no way of knowing if they've been
selected (``idioms are ignorant'').

I'm deferring a proper implementation until it's absolutely necessary.
Hopefully by then I will know more than I do now.

> linkTypeInput :: [String] -> AppForm LinkType
> linkTypeInput ts = (linkTypeS  <$> "Link Type" `formLabel` (F.select (zip ts ts) Nothing)
>                                <*> pure Association
>                                <*> pure Cognate
>                                <*> linkTypeLinkWord
>                                <*> linkTypeRelationship) `check`
>                    ensure complete "Please fill in all the link type fields."
>   where complete Association         = True
>         complete Cognate             = True
>         complete (LinkWord w s)      = (w /= "") && (s /= "")
>         complete (Relationship l r)  = (l /= "") && (r /= "")

> linkTypeS :: String -> LinkType -> LinkType -> LinkType -> LinkType -> LinkType
> linkTypeS "association"   l _ _ _ = l
> linkTypeS "cognate"       _ l _ _ = l
> linkTypeS "link word"     _ _ l _ = l
> linkTypeS "relationship"  _ _ _ l = l
> linkTypeS _ _ _ _ _ = error "Unknown link type."

> linkTypeLinkWord :: AppForm LinkType
> linkTypeLinkWord = LinkWord  <$> "Link Word" `formLabel` F.input Nothing
>                              <*> F.textarea (Just "Write a story linking the 2 words here.")

> linkTypeRelationship :: AppForm LinkType
> linkTypeRelationship = Relationship <$>
>   plug (+++ stringToHtml " is to ") (F.input Nothing) <*> F.input Nothing

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
