\section{Links}

Links are the center of interest in our program. Most activities revolve around
them.

> module Vocabulink.Link (  Link(..), PartialLink(..), LinkType(..),
>                           getPartialLink, getLinkFromPartial, getLink,
>                           memberLinks, latestLinks, linkPage, deleteLink,
>                           linksPage, linksContainingPage, newLink,
>                           partialLinkHtml, partialLinkFromValues) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Review.Html
> import Vocabulink.Utils

> import Data.List (partition)
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

> linkColor :: Link -> String
> linkColor l = case linkTypeName l of
>                 "association"   -> "#000000"
>                 "cognate"       -> "#00AA00"
>                 "link word"     -> "#0000FF"
>                 "relationship"  -> "#AA0077"
>                 _               -> "#FF00FF"

> linkBackgroundColor :: Link -> String
> linkBackgroundColor l = case linkTypeName l of
>                 "association"   -> "#DFDFDF"
>                 "cognate"       -> "#DFF4DF"
>                 "link word"     -> "#DFDFFF"
>                 "relationship"  -> "#F4DFEE"
>                 _               -> "#FFDFFF"

Fully loading a link from the database requires a join. However, the join
depends on the type of thi link. But we don't always need the type-specific
data associated with a link. Sometimes it's not even possible to have it, such
as during interactive link construction with a member.

We'll use a separate type to represent this. Essentially it's a link with an
undefined linkType. We use a separate type to avoid passing a partial link to a
function that expects a fully-instantiated link. The only danger here is
writing a function that accepts a partial link and then trying to access the
linkType information.

> newtype PartialLink = PartialLink { pLink :: Link }

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
>   r <- withTransaction' $ do
>     c <- asks appDB
>     linkNo <- liftIO $ insertNo c
>       "INSERT INTO link (origin, destination, link_type, \
>                         \language, author) \
>                 \VALUES (?, ?, ?, 'en', ?)"
>       [  toSql (linkOrigin l), toSql (linkDestination l),
>          toSql (linkTypeName l), toSql memberNo ]
>       "link_link_no_seq"
>     case linkNo of
>       Nothing  -> liftIO $ rollback c >> return Nothing
>       Just n   -> do  establishLinkType (l {linkNumber = n})
>                       return linkNo
>   return $ fromMaybe Nothing r

The table we insert additional details into depends on the type of the link and
it's easiest to use a separate function for it.

This takes a connection because it will actually accept a transaction. A more
elegant solution for the future would be to locally modify the reader to use
the transaction handle.

> establishLinkType :: Link -> App ()
> establishLinkType l = case linkType l of
>   Association                -> return ()
>   Cognate                    -> return ()
>   (LinkWord word story)      -> do
>     run'  "INSERT INTO link_type_link_word (link_no, link_word, story) \
>                                    \VALUES (?, ?, ?)"
>           [toSql (linkNumber l), toSql word, toSql story]
>     return ()
>   (Relationship left right)  -> do
>     run'  "INSERT INTO link_type_relationship \
>                  \(link_no, left_side, right_side) \
>           \VALUES (?, ?, ?)"
>           [toSql (linkNumber l), toSql left, toSql right]
>     return ()

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
>     rs <- queryTuple'  "SELECT left_side, right_side \
>                        \FROM link_type_relationship \
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

Return the types of links sorted by how common they should be. Eventually we'll
want to cache this.

> linkTypes :: App (Maybe [String])
> linkTypes = do
>   types <- queryAttribute'
>     "SELECT name FROM link_type LEFT OUTER JOIN \
>      \(SELECT link_type, COUNT(*) AS count FROM link \
>       \WHERE NOT deleted \
>       \GROUP BY link_type) AS t ON (t.link_type = link_type.name) \
>     \ORDER BY t.count DESC NULLS LAST" []
>   return $ map fromSql `liftM` types

\subsection{Deleting Links}

Links can be deleted by their owner. They're not actually removed from the
database, as doing so would require removing the link from other members'
review stacks. Instead, we just flag it as deleted so that it doesn't appear
in most contexts.

> deleteLink :: Integer -> App CGIResult
> deleteLink linkNo = do
>   res  <- quickStmt'  "UPDATE link SET deleted = TRUE \
>                       \WHERE link_no = ?" [toSql linkNo]
>   case res of
>     Nothing  -> error "Failed to delete link."
>     Just _   -> redirect =<< referrerOrVocabulink

\subsection{Displaying Links}

Generate the JavaScript necessary to draw a link on the page using SVG. You'll
need to make sure to include both |JS "raphael"| and |JS "link-graph"| as
dependencies when using this.

> drawLinkSVG :: Link -> Html
> drawLinkSVG link = script << primHtml (
>   "connect(window, 'onload', partial(drawLink," ++
>   showLinkJSON link ++ "));") +++
>   thediv ! [identifier "graph", thestyle "height: 100px"] << noHtml

It seems that the JSON library author does not want us making new instances of
the JSON class. Oh well, I didn't want to write |readJSON| anyway.

> showLinkJSON :: Link -> String
> showLinkJSON link =  let obj = [  ("orig", linkOrigin link),
>                                   ("dest", linkDestination link),
>                                   ("color", linkColor link),
>                                   ("bgcolor", linkBackgroundColor link),
>                                   ("label", linkLabel $ linkType link)] in
>                      encode $ toJSObject obj
>                        where linkLabel (LinkWord word _)  = word
>                              linkLabel _                  = ""

> displayLink :: Link -> Html
> displayLink l = concatHtml [
>   drawLinkSVG l,
>   thediv ! [theclass "link-details"] << linkTypeHtml (linkType l)]

Displaying a partial link is similar. We need to do so in different contexts,
so we have 2 different functions.

> partialLinkHtml :: PartialLink -> Html
> partialLinkHtml (PartialLink l) =
>   anchor ! [  href ("/link/" ++ (show $ linkNumber l)),
>               thestyle $  "color: " ++ linkColor l ++
>                           "; background-color: " ++ linkBackgroundColor l ++
>                           "; border: 1px solid " ++ linkColor l ] <<
>     (linkOrigin l ++ " → " ++ linkDestination l)

More comprehensive display of a link involves displaying information on its
type, which varies based on type.

> linkTypeHtml :: LinkType -> Html
> linkTypeHtml Association = noHtml
> linkTypeHtml Cognate = noHtml
> linkTypeHtml (LinkWord _ story) =
>   markdownToHtml story
> linkTypeHtml (Relationship leftSide rightSide) =
>   paragraph ! [thestyle "text-align: center"] << [
>     stringToHtml "as", br,
>     stringToHtml $ leftSide ++ " → " ++ rightSide ]

To show a specific link, we retrieve the link from the database and then
display it. Most of the extra code in the following is for handling the display
of link operations (``review'', ``delete'', etc.), dealing with retrieval
exceptions, etc.

> linkPage :: Integer -> App CGIResult
> linkPage linkNo = do
>   memberNo <- asks appMemberNo
>   l <- getLink linkNo
>   case l of
>     Nothing  -> output404 ["link", show linkNo]
>     Just l'  -> do
>       review <- reviewIndicator linkNo
>       owner <- queryValue'  "SELECT author = ? FROM link WHERE link_no = ?"
>                             [toSql memberNo, toSql linkNo]
>       ops <- case owner of
>         Just o  -> linkOperations linkNo (isJust memberNo && fromSql o)
>         _       -> return $ stringToHtml
>                      "Unable to determine link ownership."
>       let orig  = linkOrigin l'
>           dest  = linkDestination l'
>       stdPage (orig ++ " -> " ++ dest) [
>         CSS "link", JS "MochiKit", JS "raphael", JS "link-graph"] []
>         [review, ops, displayLink l']

Each link can be ``operated on''. It can be reviewed (added to the member's
review set) and deleted (marked as deleted). In the future, I expect operations
such as ``tag'', ``rate'', etc. And perhaps we should add a way for someone to
mark a link without adding it to a review set.

> linkOperations :: Integer -> Bool -> App Html
> linkOperations n True   = do
>   deleted <- queryValue'  "SELECT deleted FROM link \
>                           \WHERE link_no = ?" [toSql n]
>   return $ case deleted of
>     Just d  -> if fromSql d
>       then stringToHtml "Deleted"
>       else form ! [action ("/link/" ++ (show n) ++ "/delete"), method "POST"] <<
>              submit "" "Delete"
>     _       -> stringToHtml
>                  "Can't determine whether or not link has been deleted."
> linkOperations _ False  = return noHtml

\subsection{Finding Links}

While Vocabulink is still small, it makes sense to have a page just for
displaying all the (non-deleted) links in the system. This will probably go
away shortly after public release.

> linksPage :: App CGIResult
> linksPage  = do
>   (pg, n, offset) <- currentPage
>   ts <- latestLinks offset (n + 1)
>   case ts of
>     Nothing  -> error "Error while retrieving links."
>     Just ps  -> do
>       pagerControl <- pager pg n $ offset + (length ps)
>       simplePage "Links" [CSS "link"] [
>         unordList (map partialLinkHtml (take n ps)) !
>           [identifier "central-column", theclass "links"],
>         pagerControl ]

But more practical for the long run is providing search. ``Containing'' search
is a search for links that ``contain'' the given ``focus'' lexeme on one side
or the other of the link. The term ``containing'' is a little misleading and
should be changed.

Link search is also the only method for creating new links. The reason is that
we don't want members creating new links without seeing what links for a term
already exist. This may be a little confusing to new members, so this decision
should be reviewed again after public release.

> linksContainingPage :: String -> App CGIResult
> linksContainingPage focus = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination \
>                       \FROM link WHERE NOT deleted \
>                       \AND (origin LIKE ? OR destination LIKE ?) \
>                       \LIMIT 20"
>                       [toSql focus, toSql focus]
>   case ts of
>     Nothing  -> error "Error while retrieving links."
>     Just ls  -> stdPage ("Links containing " ++ focus)
>                   [  CSS "link",
>                      JS "MochiKit", JS "raphael", JS "link-graph"] []
>                   (linkFocusBox focus (catMaybes $ map partialLinkFromValues ls))

When the links containing a search term have been found, we need a way to
display them. This is where HTML is inadequate. Ideally we could create some
sort of link graph view (think circular) with links of varying widths and
colors and really make use of visual signals. For now though, for
accessibility's sake (I would like basic features to be usable on browsers
without SVG or Flash) we'll use a number of pre-made link edge drawings.
Currently we support the 0th case (no links found, just display ``new link''
buttons).

Before we can display the 1st and later cases, we need to sort the links into
``links containing the focus as the origin'' and ``links containing the focus
as the destination''.

> linkFocusBox :: String -> [PartialLink] -> [Html]
> linkFocusBox focus links = [
>   script << primHtml
>     (  "connect(window, 'onload', partial(drawLinks," ++
>        encode focus ++ "," ++
>        jsonNodes ("/link?input1=" ++ focus) origs ++ "," ++
>        jsonNodes ("/link?input0=" ++ focus) dests ++ "));" ),
>   thediv ! [identifier "graph"] << noHtml ]
>  where partitioned   = partition ((== focus) . linkOrigin . pLink) links
>        origs         = snd partitioned
>        dests         = fst partitioned
>        jsonNodes url xs  = encode $ insertMid
>          (toJSObject [  ("orig",   "new link"),
>                         ("dest",   "new link"),
>                         ("color",  "#000000"),
>                         ("bgcolor", "#DFDFDF"),
>                         ("style",  "dotted"),
>                         ("url",    url) ])
>          (map (\o ->  let o' = pLink o in
>                       toJSObject [  ("orig",     linkOrigin o'),
>                                     ("dest",     linkDestination o'),
>                                     ("color",    linkColor $ pLink o),
>                                     ("bgcolor",  linkBackgroundColor $ pLink o),
>                                     ("number",   show $ linkNumber $ pLink o)]) xs)
>        insertMid :: a -> [a] -> [a]
>        insertMid x xs = let (l,r) = foldr (\a ~(x',y') -> (a:y',x')) ([],[]) xs in
>                         reverse l ++ [x] ++ r

\subsection{Creating New Links}

We want the creation of new links to be as simple as possible. For now, it's
done using a single page with a form. The form dynamically updates (via
JavaScript) based on the type of the link being created.

> newLink :: App CGIResult
> newLink = withRequiredMemberNumber $ \memberNo -> do
>   ts    <- linkTypes
>   uri   <- requestURI
>   meth  <- requestMethod
>   case ts of
>     Nothing   -> error "Unable to retrieve link types."
>     Just ts'  -> do
>       preview <- getInput "preview"
>       (status, xhtml) <- runForm' (establish ts')
>       case preview of
>         Just _  -> do
>           let preview' = case status of
>                            Failure failures  -> unordList failures
>                            Success link      -> thediv ! [theclass "preview"] <<
>                                                   displayLink link
>           simplePage "Create a Link (preview)" deps
>             [  preview',
>                form ! [  thestyle "text-align: center",
>                          action (uriPath uri), method "POST"] <<
>                  [xhtml, actionBar] ]
>         Nothing -> do
>           case status of
>             Failure failures  -> simplePage "Create a Link" deps
>               [  form ! [  thestyle "text-align: center",
>                            action (uriPath uri), method "POST"] <<
>                    [  meth == "GET" ? noHtml $ unordList failures,
>                       xhtml, actionBar ] ]
>             Success link -> do
>               linkNo <- establishLink link memberNo
>               case linkNo of
>                 Just n   -> redirect $ "/link/" ++ (show n)
>                 Nothing  -> error "Failed to establish link."
>  where deps = [  CSS "link", JS "MochiKit", JS "link",
>                  JS "raphael", JS "link-graph"]
>        actionBar = thediv ! [thestyle "margin-left: auto; margin-right: auto; \
>                                       \width: 12em"] <<
>                      [  submit "preview" "Preview" ! [thestyle "float: left; width: 5.5em"],
>                         submit "" "Link" ! [thestyle "float: right; width: 5.5em"],
>                         paragraph ! [thestyle "clear: both"] << noHtml ]

Here's a form for creating a link. It gathers all of the required details
(origin, destination, and link type details).

> establish :: [String] -> AppForm Link
> establish ts = mkLink  <$> plug (+++ stringToHtml " ") (linkNodeInput "Origin")
>                        <*> linkNodeInput "Destination"
>                        <*> linkTypeInput ts

When creating a link from a form, the link number must be undefined until the
link is established in the database. Also, because of the way formlets work (or
how I'm using them), we need to retrieve the link type name from the link type.

> mkLink :: String -> String -> LinkType -> Link
> mkLink orig dest t = Link {  linkNumber       = undefined,
>                              linkTypeName     = linkTypeNameFromType t,
>                              linkOrigin       = orig,
>                              linkDestination  = dest,
>                              linkType         = t }

> linkNodeInput :: String -> AppForm String
> linkNodeInput l = l `formLabel` F.input Nothing `check` ensures
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
> linkTypeInput ts = (linkTypeS  <$> "Link Type" `formLabel'` linkSelect Nothing
>                                <*> pure Association
>                                <*> pure Cognate
>                                <*> fieldset' "link-word" linkTypeLinkWord
>                                <*> fieldset' "relationship" linkTypeRelationship)
>                    `check` ensure complete
>                      "Please fill in all the link type fields."
>   where linkSelect = F.select $ zip ts ts
>         complete Association         = True
>         complete Cognate             = True
>         complete (LinkWord w s)      = (w /= "") && (s /= "")
>         complete (Relationship l r)  = (l /= "") && (r /= "")
>         fieldset' ident              = plug
>           (fieldset ! [identifier ident, thestyle "display: none"] <<)

> linkTypeS :: String -> LinkType -> LinkType -> LinkType -> LinkType -> LinkType
> linkTypeS "association"   l _ _ _ = l
> linkTypeS "cognate"       _ l _ _ = l
> linkTypeS "link word"     _ _ l _ = l
> linkTypeS "relationship"  _ _ _ l = l
> linkTypeS _ _ _ _ _ = error "Unknown link type."

> linkTypeLinkWord :: AppForm LinkType
> linkTypeLinkWord = LinkWord  <$> "Link Word" `formLabel'` F.input Nothing
>   <*> F.textarea (Just "Write a story linking the 2 words here.")

> linkTypeRelationship :: AppForm LinkType
> linkTypeRelationship = Relationship <$>
>   plug (+++ stringToHtml " is to ") (F.input Nothing) <*> F.input Nothing

We want to be able to display links to members (and non-members) in various
ways.

It would be really nice to have lazy result lists. However, it doesn't seem to
work too well. For now, you need to specify how many results you want, as well
as an offset.

Here we retrieve multiple links at once. This was the original motivation for
dividing links into full links and partial links. Often we need to retrieve
links for display but we don't need or want extra trips to the database. Here
we need 1 query instead of potentially @limit@ queries.

This assumes the ordering of links is determined by link number. It's used by
the page which displays a listing of links. We don't want to display deleted
links (which are left in the database for people still reviewing them).

> memberLinks :: Integer -> Int -> Int -> App (Maybe [PartialLink])
> memberLinks memberNo offset limit = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination \
>                       \FROM link WHERE NOT deleted \
>                       \AND author = ? \
>                       \ORDER BY link_no DESC \
>                       \OFFSET ? LIMIT ?"
>                       [toSql memberNo, toSql offset, toSql limit]
>   return $ (catMaybes . map partialLinkFromValues) `liftM` ts

> latestLinks :: Int -> Int -> App (Maybe [PartialLink])
> latestLinks offset limit = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination \
>                       \FROM link WHERE NOT deleted \
>                       \ORDER BY link_no DESC \
>                       \OFFSET ? LIMIT ?" [toSql offset, toSql limit]
>   return $ (catMaybes . map partialLinkFromValues) `liftM` ts
