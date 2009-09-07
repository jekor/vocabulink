% Copyright 2008, 2009 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\section{Links}

Links are the center of interest in our program. Most activities revolve around
them.

> module Vocabulink.Link (  Link(..), PartialLink(..), LinkType(..),
>                           getPartialLink, getLinkFromPartial, getLink,
>                           memberLinks, latestLinks, linkPage, deleteLink,
>                           linksPage, linksContainingPage, newLink,
>                           partialLinkHtml, partialLinkFromValues,
>                           drawLinkSVG, drawLinkSVG', languagePairsPage,
>                           languagePairLinks, languageNameFromAbbreviation,
>                           LinkPack(..),
>                           newLinkPack, linkPackPage, deleteLinkPack,
>                           addToLinkPack, linkPacksPage, getLinkPack,
>                           displayCompactLinkPack, linkPackTextLink,
>                           linkPackIconLink ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Comment
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Rating
> import Vocabulink.Review.Html
> import Vocabulink.Utils

> import Control.Exception (try)
> import Data.List (partition)
> import System.Cmd (system)
> import System.Exit (ExitCode(..))
> import qualified Text.Formlets as Fl
> import qualified Text.XHtml as H
> import qualified Text.XHtml.Strict.Formlets as F (input, selectRaw, textarea, hidden)

\subsection{Link Data Types}

Abstractly, a link is defined by the origin and destination lexemes it links,
as well as its type. Practically, we also need to carry around information such
as its link number (in the database) as well as a string representation of its
type (for partially constructed links, which you'll see later).

> data Link = Link {  linkNumber           :: Integer,
>                     linkTypeName         :: String,
>                     linkOrigin           :: String,
>                     linkOriginLang       :: String,
>                     linkDestination      :: String,
>                     linkDestinationLang  :: String,
>                     linkType             :: LinkType }

The |linkOriginLang| and |linkDestinationLang| are the language abbrevations.
To get the full name we need to look it up.

> languageNameFromAbbreviation :: String -> App (Maybe String)
> languageNameFromAbbreviation a = return . lookup a =<< languages

Here are a couple shortcuts.

> linkOriginLanguage, linkDestinationLanguage :: Link -> App String
> linkOriginLanguage l = fromJust <$> languageNameFromAbbreviation (linkOriginLang l)
> linkDestinationLanguage l = fromJust <$> languageNameFromAbbreviation (linkDestinationLang l)

> languages :: App [(String, String)]
> languages = languagesFromDB

> languagesFromDB :: App [(String, String)]
> languagesFromDB = map langToPair . fromJust <$>
>   queryTuples'  "SELECT abbr, name FROM language ORDER BY name" []
>     where  langToPair [a, n]  = (fromSql a, fromSql n)
>            langToPair _       = error "Failed to retrieve languages from the database."

We can associate 2 lexemes in many different ways. Because different linking
methods require different information, they each need different representations
in the database. This leads to some additional complexity.

Each link between lexemes has a type. This type determines how the link is
displayed, edited, used in statistical analysis, etc. See the Vocabulink
handbook for a more in-depth description of the types.

> data LinkType =  Association | Cognate | LinkWord String String |
>                  Relationship String String
>                  deriving (Show)

Sometimes we need to work with a human-readable name, such as when interacting
with a client or the database.

> linkTypeNameFromType :: LinkType -> String
> linkTypeNameFromType Association         = "association"
> linkTypeNameFromType Cognate             = "cognate"
> linkTypeNameFromType (LinkWord _ _)      = "link word"
> linkTypeNameFromType (Relationship _ _)  = "relationship"

Each link type also has an associated color. This makes the type of links stand
out clearly in lists and graphs.

> linkColor :: Link -> String
> linkColor l = case linkTypeName l of
>                 "association"   -> "#000000"
>                 "cognate"       -> "#00AA00"
>                 "link word"     -> "#0000FF"
>                 "relationship"  -> "#AA0077"
>                 _               -> "#FF00FF"

The link's background color is used for shading and highlighting.

> linkBackgroundColor :: Link -> String
> linkBackgroundColor l = case linkTypeName l of
>                 "association"   -> "#DFDFDF"
>                 "cognate"       -> "#DFF4DF"
>                 "link word"     -> "#DFDFFF"
>                 "relationship"  -> "#F4DFEE"
>                 _               -> "#FFDFFF"

Links are created by members. Vocabulink does not own them. It merely has a
license to use them (as part of the Terms of Use). So when displaying a link in
full, we display a copyright notice with the member's username.

Of course, simple link types without nothing other than the origin and
destination of the link do not contain copyrightable material.

> linkCopyright :: Link -> App Html
> linkCopyright l =
>   case linkType l of
>     LinkWord _ _ -> do
>       t <- queryTuple'  "SELECT username, \
>                                \extract(year from created), \
>                                \extract(year from updated) \
>                         \FROM link, member \
>                         \WHERE member_no = author AND link_no = ?"
>                         [toSql $ linkNumber l]
>       return $ paragraph ! [theclass "copyright"] << stringToHtml (
>         "Copyright " ++ case t of
>                           Just [a,c,u]  ->  let c'  = show (fromSql c :: Integer)
>                                                 u'  = show (fromSql u :: Integer)
>                                                 r   = c' == u' ? c' $ c' ++ "–" ++ u' in
>                                             r ++ " " ++ fromSql a
>                           _             ->  "unknown")
>     _            -> return noHtml

Fully loading a link from the database requires joining 2 relations. The join
depends on the type of the link. But we don't always need the type-specific
data associated with a link. Sometimes it's not even possible to have it, such
as during interactive link construction.

We'll use a separate type to represent this. Essentially it's a link with an
undefined linkType. We use a separate type to avoid passing a partial link to a
function that expects a fully-instantiated link. The only danger here is
writing a function that accepts a partial link and then trys to access the
linkType information.

> newtype PartialLink = PartialLink { pLink :: Link }

\subsection{Storing Links}

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
Our App-level database functions are not yet great with transactions, so we'll
have to handle the transaction manually here. You'll also notice that some link
types (such as cognates) have no additional information and hence no relation
in the database.

This returns the newly established link number.

> establishLink :: Link -> Integer -> App (Maybe Integer)
> establishLink l memberNo = do
>   r <- withTransaction' $ do
>     c <- asks appDB
>     linkNo <- liftIO $ insertNo c
>       "INSERT INTO link (origin, destination, \
>                         \origin_language, destination_language, \
>                         \link_type, author) \
>                 \VALUES (?, ?, ?, ?, ?, ?)"
>       [  toSql (linkOrigin l), toSql (linkDestination l),
>          toSql (linkOriginLang l), toSql (linkDestinationLang l),
>          toSql (linkTypeName l), toSql memberNo ]
>       "link_link_no_seq"
>     case linkNo of
>       Nothing  -> liftIO $ rollback c >> return Nothing
>       Just n   -> do  establishLinkType (l {linkNumber = n})
>                       return linkNo
>   return $ fromMaybe Nothing r

The relation we insert additional details into depends on the type of the link
and it's easiest to use a separate function for it.

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

\subsection{Retrieving Links}

Now that we've seen how we store links, let's look at retrieving them (which is
slightly more complicated in order to allow for efficient retrieval of multiple
links).

Retrieving a partial link is simple.

> getPartialLink :: Integer -> App (Maybe PartialLink)
> getPartialLink linkNo = do
>   t <- queryTuple'  "SELECT link_no, link_type, origin, destination, \
>                            \origin_language, destination_language \
>                     \FROM link \
>                     \WHERE link_no = ?" [toSql linkNo]
>   return $ partialLinkFromValues =<< t

We use a helper function to convert the raw SQL tuple to a partial link value.
Note that we leave the link's |linkType| undefined.

> partialLinkFromValues :: [SqlValue] -> Maybe PartialLink
> partialLinkFromValues [n, t, o, d, ol, dl] = Just $
>   PartialLink Link {  linkNumber           = fromSql n,
>                       linkTypeName         = fromSql t,
>                       linkOrigin           = fromSql o,
>                       linkDestination      = fromSql d,
>                       linkOriginLang       = fromSql ol,
>                       linkDestinationLang  = fromSql dl,
>                       linkType             = undefined }
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
> getLink linkNo = getPartialLink linkNo >>= maybe (return Nothing) getLinkFromPartial

We already know what types of links exist, but we want only the active link
types (some, like Relationship, are experimental) sorted by how common they
are.

> activeLinkTypes :: [String]
> activeLinkTypes = ["link word", "association", "cognate"]

\subsection{Deleting Links}

Links can be deleted by their owner. They're not actually removed from the
database, as doing so would require removing the link from other members'
review sets. Instead, we just flag the link as deleted so that it doesn't
appear in most contexts.

> deleteLink :: Integer -> App CGIResult
> deleteLink linkNo = do
>   res  <- quickStmt'  "UPDATE link SET deleted = TRUE \
>                       \WHERE link_no = ?" [toSql linkNo]
>   case res of
>     Nothing  -> error "Failed to delete link."
>     Just _   -> redirect =<< referrerOrVocabulink

\subsection{Displaying Links}

Drawing links is a rather complicated process due to the limitations of HTML.
Fortunately there is Raphaël (http://raphaeljs.com/reference.html) which makes
some pretty fancy link drawing possible via JavaScript. You'll need to make
sure to include |JS "lib.link"| as a dependency when using this.

> drawLinkSVG :: Link -> Html
> drawLinkSVG = drawLinkSVG' "drawLink"

> drawLinkSVG' :: String -> Link -> Html
> drawLinkSVG' f link = thediv ! [identifier "graph", thestyle "height: 100px"] << noHtml +++
>   script ! [thetype "text/javascript"] << primHtml (
>     "$(document).ready(" ++ f ++ ".curry(" ++ showLinkJSON link ++ "));")

It seems that the JSON library author does not want us making new instances of
the |JSON| class. Oh well, I didn't want to write |readJSON| anyway.

> showLinkJSON :: Link -> String
> showLinkJSON link =  let obj = [  ("orig", linkOrigin link),
>                                   ("dest", linkDestination link),
>                                   ("color", linkColor link),
>                                   ("bgcolor", linkBackgroundColor link),
>                                   ("label", linkLabel $ linkType link)] in
>                      encode $ toJSObject obj
>                        where linkLabel (LinkWord word _)  = word
>                              linkLabel _                  = ""

Displaying an entire link involves not just drawing a graphical representation
of the link but displaying its type-level details as well.

> displayLink :: Link -> Html
> displayLink l = concatHtml [
>   drawLinkSVG l,
>   thediv ! [theclass "link-details"] << linkTypeHtml (linkType l) ]

> linkTypeHtml :: LinkType -> Html
> linkTypeHtml Association = noHtml
> linkTypeHtml Cognate = noHtml
> linkTypeHtml (LinkWord _ story) =
>   markdownToHtml story
> linkTypeHtml (Relationship leftSide rightSide) =
>   paragraph ! [thestyle "text-align: center"] << [
>     stringToHtml "as", br,
>     stringToHtml $ leftSide ++ " → " ++ rightSide ]

Sometimes we don't need to display all of a links details. This displays a
partial link more compactly, such as for use in lists, etc.

> partialLinkHtml :: PartialLink -> App Html
> partialLinkHtml (PartialLink l) = do
>   originLanguage <- linkOriginLanguage l
>   destinationLanguage <- linkDestinationLanguage l
>   return $ anchor ! [  href ("/link/" ++ show (linkNumber l)),
>                        H.title (originLanguage ++ " → " ++ destinationLanguage),
>                        thestyle $  "color: " ++ linkColor l ++
>                                    "; background-color: " ++ linkBackgroundColor l ++
>                                    "; border: 1px solid " ++ linkColor l ] <<
>              (linkOrigin l ++ " → " ++ linkDestination l)

Each link gets its own URI and page. Most of the extra code in the following is
for handling the display of link operations (``review'', ``delete'', etc.),
dealing with retrieval exceptions, etc.

For the link's owner, we'll send along the source of the link in a hidden
textarea for in-page editing.

> linkPage :: Integer -> App CGIResult
> linkPage linkNo = do
>   memberNo <- asks appMemberNo
>   l <- getLink linkNo
>   case l of
>     Nothing  -> output404 ["link", show linkNo]
>     Just l'  -> do
>       owner <- queryValue'  "SELECT author = ? FROM link WHERE link_no = ?"
>                             [toSql memberNo, toSql linkNo]
>       let owner' = maybe False fromSql owner
>       ops <- linkOperations linkNo owner'
>       rating <- queryTuple'  "SELECT COUNT(rating), SUM(rating) / COUNT(rating) \
>                              \FROM link_rating WHERE link_no = ?" [toSql linkNo]
>       ratingEnabled <- if isJust memberNo
>                           then  isNothing <$> queryValue'  "SELECT rating FROM link_rating \
>                                                            \WHERE link_no = ? AND member_no = ?"
>                                                            [toSql linkNo, toSql memberNo]
>                           else  return False
>       copyright <- linkCopyright l'
>       let orig  = linkOrigin l'
>           dest  = linkDestination l'
>       originLanguage <- linkOriginLanguage l'
>       destinationLanguage <- linkDestinationLanguage l'
>       r <- queryValue'  "SELECT root_comment \
>                         \FROM link_comments \
>                         \WHERE link_no = ?" [toSql linkNo]
>       comments <- case r of
>                     Just root  -> renderComments $ fromSql root
>                     Nothing    -> return noHtml
>       stdPage (orig ++ " -> " ++ dest) [CSS "link", JS "lib.link"] []
>         [  drawLinkSVG l',
>            thediv ! [theclass "link-ops"] << [
>              anchor ! [href (  "/links?ol=" ++ linkOriginLang l' ++
>                                "&dl=" ++ linkDestinationLang l' ) ] <<
>                (originLanguage ++ " → " ++ destinationLanguage),
>              let (c', r') = case rating of
>                             Just [c'', r'']  -> (fromSql c'', fromSql r'')
>                             _                -> (0, Nothing) in
>              ratingBar ("/link" </> show linkNo </> "rating") c' r' ratingEnabled,
>              isNothing memberNo ? anchor ! [href "/member/login"] << "Login to Rate" $ noHtml,
>              ops ],
>            thediv ! [theclass "link-details"] << [
>              case (owner', linkType l') of
>                (True, LinkWord _ story)  -> textarea ! [thestyle "display: none"] << story
>                _                         -> noHtml,
>              thediv ! [identifier "link-details"] << linkTypeHtml (linkType l') ],
>            copyright, clear, hr ! [thestyle "margin-top: 1.3em"],
>            h3 << "Comments", comments ]

Each link can be ``operated on''. It can be reviewed (added to the member's
review set) and deleted (marked as deleted). In the future, I expect operations
such as ``tag'', ``rate'', etc.

The |Bool| parameter indicates whether or not the currently logged-in member
(if the client is logged in) is the owner of the link.

> linkOperations :: Integer -> Bool -> App Html
> linkOperations n owner = do
>   review <- reviewIndicator n
>   deleted <- queryValue'  "SELECT deleted FROM link \
>                           \WHERE link_no = ?" [toSql n]
>   packForm <- addToLinkPackForm n
>   return $ concatHtml [
>     review,
>     packForm,
>     if owner
>       then button ! [identifier "link-edit"] << "Edit"
>       else noHtml,
>     case (owner, deleted) of
>       (True, Just d)  -> if fromSql d
>                            then paragraph << "Deleted"
>                            else form ! [action ("/link/" ++ show n ++ "/delete"), method "POST"] <<
>                                   submit "" "Delete"
>       (True, _)       -> stringToHtml
>                            "Can't determine whether or not link has been deleted."
>       (False, _)      -> noHtml ]

> addToLinkPackForm :: Integer -> App Html
> addToLinkPackForm n = do
>   memberNo <- asks appMemberNo
>   case memberNo of
>     Nothing  -> return noHtml
>     Just mn  -> do
>       -- Find all of the packs created by this member that don't already contain
>       -- this link.
>       packs <- queryTuples'
>         "SELECT pack_no, name FROM link_pack \
>         \WHERE creator = ? \
>           \AND pack_no NOT IN (SELECT pack_no FROM link_pack_link \
>                               \WHERE link_no = ?) \
>         \ORDER BY pack_no DESC" [toSql mn, toSql n]
>       case packs of
>         Nothing  -> return noHtml
>         Just ps  -> do
>           let ps' = map (\x -> (fromSql $ head x, fromSql $ head $ tail x)) ps
>           return $ concatHtml [
>             button ! [theclass "reveal add-to-pack"] << "→ Pack",
>             form ! [  action "/pack/link/new", method "POST",
>                       identifier "add-to-pack", thestyle "display: none" ] << [
>               hidden "link" $ show n,
>               menu "pack" (ps' ++ [("new", "New Pack")]) !
>                 [identifier "pack-select"], br,
>               submit "" "→ Pack" ] ]

\subsection{Finding Links}

While Vocabulink is still small, it makes sense to have a page just for
displaying all the (non-deleted) links in the system. This will probably go
away eventually.

> linksPage :: String -> (Int -> Int -> App (Maybe [PartialLink])) -> App CGIResult
> linksPage title f = do
>   (pg, n, offset) <- currentPage
>   ts <- f offset (n + 1)
>   case ts of
>     Nothing  -> error "Error while retrieving links."
>     Just ps  -> do
>       pagerControl <- pager pg n $ offset + length ps
>       partialLinks <- mapM partialLinkHtml (take n ps)
>       simplePage title [CSS "link"] [
>         unordList partialLinks ! [identifier "central-column", theclass "links"],
>         pagerControl ]

A more practical option for the long run is providing search. ``Containing''
search is a search for links that ``contain'' the given ``focus'' lexeme on one
side or the other of the link. The term ``containing'' is a little misleading
and should be changed at some point.

For now we use exact matching only as that can use an index. Fuzzy matching is
going to require configuring full text search or a separate search daemon.

> linksContainingPage :: String -> App CGIResult
> linksContainingPage focus = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination, \
>                              \origin_language, destination_language \
>                       \FROM link \
>                       \WHERE NOT deleted \
>                         \AND (origin LIKE ? OR destination LIKE ?) \
>                       \LIMIT 20"
>                       [toSql focus, toSql focus]
>   case ts of
>     Nothing  -> error "Error while retrieving links."
>     Just ls  -> simplePage (  "Found " ++ show (length ls) ++
>                               " link" ++ (length ls == 1 ? "" $ "s") ++ 
>                               " containing \"" ++ focus ++ "\"")
>                   [CSS "link", JS "lib.link"]
>                   (linkFocusBox focus (mapMaybe partialLinkFromValues ls))

When the links containing a search term have been found, we need a way to
display them. We do so by drawing a ``link graph'': a circular array of links.

Before we can display the graph, we need to sort the links into ``links
containing the focus as the origin'' and ``links containing the focus as the
destination''.

If you're trying to understand this function, it helps to read the JavaScript
it outputs and digest each local function separately.

> linkFocusBox :: String -> [PartialLink] -> [Html]
> linkFocusBox focus links = [
>   thediv ! [identifier "graph"] << noHtml,
>   script << primHtml
>     (  "$(document).ready(drawLinks.curry(" ++
>        encode focus ++ "," ++
>        jsonNodes ("/link/new?fval2=" ++ focus) origs ++ "," ++
>        jsonNodes ("/link/new?fval0=" ++ focus) dests ++ "));" ) ]
>  where partitioned   = partition ((== focus) . linkOrigin . pLink) links
>        origs         = snd partitioned
>        dests         = fst partitioned
>        jsonNodes url = encode . insertMid
>          (toJSObject [  ("orig",   "new link"),
>                         ("dest",   "new link"),
>                         ("color",  "#000000"),
>                         ("bgcolor", "#DFDFDF"),
>                         ("style",  "dotted"),
>                         ("url",    url) ]) .
>          map (\o ->  let o' = pLink o in
>                      toJSObject [  ("orig",     linkOrigin o'),
>                                    ("dest",     linkDestination o'),
>                                    ("color",    linkColor $ pLink o),
>                                    ("bgcolor",  linkBackgroundColor $ pLink o),
>                                    ("number",   show $ linkNumber $ pLink o)])
>        insertMid :: a -> [a] -> [a]
>        insertMid x xs = let (l,r) = every2nd xs in
>                         reverse l ++ [x] ++ r

\subsection{Creating New Links}

We want the creation of new links to be as simple as possible. For now, it's
done on a single page. The form on the page dynamically updates (via
JavaScript, but not AJAX) based on the type of the link being created.

This is very large because it handles generating the form, previewing the
result, and dispatching the creation of the link on successful form validation.

> newLink :: App CGIResult
> newLink = withRequiredMemberNumber $ \memberNo -> do
>   uri   <- requestURI
>   meth  <- requestMethod
>   preview <- getInput "preview"
>   establishF <- establish activeLinkTypes
>   (status, xhtml) <- runForm' establishF
>   case preview of
>     Just _  -> do
>       let preview' = case status of
>                        Failure failures  -> unordList failures
>                        Success link      -> thediv ! [theclass "preview"] <<
>                                               displayLink link
>       simplePage "Create a Link (preview)" deps
>         [  preview',
>            form ! [  thestyle "text-align: center",
>                      action (uriPath uri), method "POST"] <<
>              [xhtml, actionBar] ]
>     Nothing ->
>       case status of
>         Failure failures  -> simplePage "Create a Link" deps
>           [  form ! [  thestyle "text-align: center",
>                        action (uriPath uri), method "POST"] <<
>                [  meth == "GET" ? noHtml $ unordList failures,
>                   xhtml, actionBar ] ]
>         Success link -> do
>           linkNo <- establishLink link memberNo
>           case linkNo of
>             Just n   -> redirect $ "/link/" ++ show n
>             Nothing  -> error "Failed to establish link."
>  where deps = [CSS "link", JS "lib.link"]
>        actionBar = thediv ! [thestyle "margin-left: auto; margin-right: auto; \
>                                       \width: 12em"] <<
>                      [  submit "preview" "Preview" !
>                           [thestyle "float: left; width: 5.5em"],
>                         submit "" "Link" ! [thestyle "float: right; width: 5.5em"],
>                         paragraph ! [thestyle "clear: both"] << noHtml ]

Here's a form for creating a link. It gathers all of the required details
(origin, destination, and link type details).

> establish :: [String] -> App (AppForm Link)
> establish ts = do
>   originPicker       <- languagePicker $ Left ()
>   destinationPicker  <- languagePicker $ Right ()
>   return (mkLink  <$> lexemeInput "Foreign"
>                   <*> plug (+++ stringToHtml " ") originPicker
>                   <*> lexemeInput "Native"
>                   <*> destinationPicker
>                   <*> linkTypeInput ts)

When creating a link from a form, the link number must be undefined until the
link is established in the database. Also, because of the way formlets work (or
how I'm using them), we need to retrieve the link type name from the link type.

> mkLink :: String -> String -> String -> String -> LinkType -> Link
> mkLink o ol d dl t = Link {  linkNumber           = undefined,
>                              linkTypeName         = linkTypeNameFromType t,
>                              linkOrigin           = o,
>                              linkOriginLang       = ol,
>                              linkDestination      = d,
>                              linkDestinationLang  = dl,
>                              linkType             = t }

The lexeme is the origin or destination of the link.

> lexemeInput :: String -> AppForm String
> lexemeInput l = l `formLabel` F.input Nothing `check` ensures
>   [  ((/= "")           , l ++ " is required."),
>      ((<= 64) . length  , l ++ " must be 64 characters or shorter.") ]

Each lexeme needs to be annotated with its language (to aid with
disambiguation, searching, and sorting). Most members are going to be studying
a single language, and it would be cruel to make them scroll through a huge
list of languages each time they wanted to create a new link. So what we do is
sort languages that the member has already used to the top of the list (based
on frequency).

This takes an either parameter to signify whether you want origin language
(Left) or destination language (Right). They are sorted separately.

> languagePicker :: Either () () -> App (AppForm String)
> languagePicker side = do
>   let side' = case side of
>                 Left _   -> "origin"
>                 Right _  -> "destination"
>   memberNo <- asks appMemberNo
>   langs <- map langPair `liftM` fromJust <$> queryTuples'
>              ("SELECT abbr, name \
>               \FROM link, language \
>               \WHERE language.abbr = link." ++ side' ++ "_language \
>                 \AND link.author = ? \
>               \GROUP BY " ++ side' ++ "_language, abbr, name \
>               \ORDER BY COUNT(" ++ side' ++ "_language) DESC")
>              [toSql memberNo]
>   allLangs <- languages
>   let choices = langs ++ [("","")] ++ allLangs
>   return $ F.selectRaw [] choices (Just $ fst . head $ choices) `check` ensures
>              [  ((/= "")  ,  side' ++ " language is required") ]
>     where langPair [a, b]  = (fromSql a, fromSql b)
>           langPair _       = error "Invalid language pair."

We have a bit of a challenge with link types. We want the form to adjust
dynamically using JavaScript when a member chooses one of the link types from a
select list. But we also want form validation using formlets. Formlets would be
rather straightforward if we were using a 2-step process (choose the link type,
submit, fill in the link details, submit). But it's important to keep the link
creation process simple (and hence 1-step).

The idea is to generate all the form fields for every possible link type in
advance, with a default hidden state. Then JavaScript will reveal the
appropriate fields when a link type is chosen. Upon submit, all link type
fields for all but the selected link type will be empty (or unnecessary). When
running the form, we will instantiate all of them, but then |linkTypeS| will
select just the appropriate one based on the @<select>@.

The main challenge here is that we can't put the validation in the link types
themselves. We have to move it into |linkTypeInput|. The problem comes from
either my lack of understanding of Applicative Functors, or the fact that by
the time the formlet combination strategy (Failure) runs, the unused link types
have already generated failure because they have no way of knowing if they've
been selected (``idioms are ignorant'').

I'm deferring a proper implementation until it's absolutely necessary.
Hopefully by then I will know more than I do now.

> linkTypeInput :: [String] -> AppForm LinkType
> linkTypeInput ts = (linkTypeS  <$> plug (\xhtml ->
>                                            paragraph << [  xhtml,
>                         helpButton "/article/understanding-link-types" Nothing])
>                                      ("Link Type" `formLabel` linkSelect Nothing)
>                                <*> pure Association
>                                <*> pure Cognate
>                                <*> fieldset' "link-word" linkTypeLinkWord
>                                <*> fieldset' "relationship" linkTypeRelationship)
>                    `check` ensure complete
>                      "Please fill in all the link type fields."
>   where linkSelect = F.selectRaw [] $ zip ts ts
>         complete Association         = True
>         complete Cognate             = True
>         complete (LinkWord w s)      = (w /= "") && (s /= "")
>         complete (Relationship l r)  = (l /= "") && (r /= "")
>         fieldset' ident              = plug
>           (fieldset ! [identifier ident, thestyle "display: none"] <<)

> linkTypeS :: String -> LinkType -> LinkType -> LinkType -> LinkType -> LinkType
> linkTypeS "association"   l _ _ _  = l
> linkTypeS "cognate"       _ l _ _  = l
> linkTypeS "link word"     _ _ l _  = l
> linkTypeS "relationship"  _ _ _ l  = l
> linkTypeS _               _ _ _ _  = error "Unknown link type."

> linkTypeLinkWord :: AppForm LinkType
> linkTypeLinkWord = LinkWord  <$> "Link Word" `formLabel'` F.input Nothing
>   <*> linkTypeLinkWordStory "Write a story linking the 2 words here."

> linkTypeLinkWordStory :: String -> AppForm String
> linkTypeLinkWordStory s = F.textarea Nothing Nothing (Just s)

> linkTypeRelationship :: AppForm LinkType
> linkTypeRelationship = Relationship <$>
>   plug (+++ stringToHtml " is to ") (F.input Nothing) <*> F.input Nothing

We want to be able to display links in various ways. It would be really nice to
get lazy lists from the database. However, lazy HDBC results don't seem to work
too well in my experience (at least not with PostgreSQL). For now, you need to
specify how many results you want, as well as an offset.

Here we retrieve multiple links at once. This was the original motivation for
dividing link types into full and partial. Often we need to retrieve links for
simple display but we don't need or want extra trips to the database. Here we
need only 1 query instead of potentially @limit@ queries.

We don't want to display deleted links (which are left in the database for
people still reviewing them). There is some duplication of SQL here, but I have
yet found a nice way to generalize these functions.

The first way to retrieve links is to just grab all of them, starting at the
most recent. This assumes the ordering of links is determined by link number.

> latestLinks :: Int -> Int -> App (Maybe [PartialLink])
> latestLinks offset limit = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination, \
>                              \origin_language, destination_language \
>                       \FROM link \
>                       \WHERE NOT deleted \
>                       \ORDER BY link_no DESC \
>                       \OFFSET ? LIMIT ?" [toSql offset, toSql limit]
>   return $ mapMaybe partialLinkFromValues `liftM` ts

Another way we retrieve links is by author (member). These just happen to be
sorted by link number as well.

> memberLinks :: Integer -> Int -> Int -> App (Maybe [PartialLink])
> memberLinks memberNo offset limit = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination, \
>                              \origin_language, destination_language \
>                       \FROM link \
>                       \WHERE NOT deleted AND author = ? \
>                       \ORDER BY link_no DESC \
>                       \OFFSET ? LIMIT ?"
>                       [toSql memberNo, toSql offset, toSql limit]
>   return $ mapMaybe partialLinkFromValues `liftM` ts

> languagePairLinks :: String -> String -> Int -> Int -> App (Maybe [PartialLink])
> languagePairLinks oa da offset limit = do
>   ts <- queryTuples'  "SELECT link_no, link_type, origin, destination, \
>                              \origin_language, destination_language \
>                       \FROM link \
>                       \WHERE NOT deleted \
>                         \AND origin_language = ? AND destination_language = ? \
>                       \ORDER BY link_no DESC \
>                       \OFFSET ? LIMIT ?"
>                       [toSql oa, toSql da, toSql offset, toSql limit]
>   return $ mapMaybe partialLinkFromValues `liftM` ts

Once we have a significant number of links, browsing through latest becomes
unreasonable for finding links for just the language we're interested in. To
aid in this, it helps to know which language pairs are in use and to know how
many links exist for each so that we can arrange by popularity.

Since we need both the language abbreviation and name (we use the abbreviation
in URLs and the name for display to the client), we return these as triples:
(language, language, count).

> linkLanguages :: App [((String, String), (String, String), Integer)]
> linkLanguages = do
>   ts <- queryTuples'  "SELECT origin_language, orig.name, \
>                              \destination_language, dest.name, \
>                              \COUNT(*) FROM link \
>                       \INNER JOIN language orig ON (orig.abbr = origin_language) \
>                       \INNER JOIN language dest ON (dest.abbr = destination_language) \
>                       \WHERE NOT deleted \
>                       \GROUP BY origin_language, orig.name, \
>                                \destination_language, dest.name \
>                       \ORDER BY COUNT(*) DESC" []
>   return $ case ts of
>     Nothing   -> []
>     Just ts'  -> map linkLanguages' ts'
>       where linkLanguages' [oa, on, da, dn, c]  =
>               ((fromSql oa, fromSql on), (fromSql da, fromSql dn), fromSql c)
>             linkLanguages' _                    = error "Malformed tuple."

Now we can use |linkLanguages| to create a page via which clients can browse
the site.

> languagePairsPage :: App CGIResult
> languagePairsPage = do
>   languages' <- linkLanguages
>   let (col1, col2, col3) = every3rd $ map languagePairLink languages'
>   simplePage "Links by Language Pair" [CSS "link"] [
>     thediv ! [theclass "three-column"] << [
>       thediv ! [theclass "column"] << unordList col1,
>       thediv ! [theclass "column"] << unordList col2,
>       thediv ! [theclass "column"] << unordList col3 ] ]

Display a hyperlink for a language pair.

> languagePairLink :: ((String, String), (String, String), Integer) -> Html
> languagePairLink ((oa, on), (da, dn), c) =
>   anchor ! [  theclass "language-pair",
>               href ("/links?ol=" ++ oa ++ "&dl=" ++ da) ] <<
>     (on ++ " → " ++ dn ++ " (" ++ show c ++ ")")

> data LinkPack = LinkPack {  linkPackNumber       :: Integer,
>                             linkPackName         :: String,
>                             linkPackDescription  :: String,
>                             linkPackImage        :: Maybe String,
>                             linkPackCreator      :: Integer }

> linkPackForm :: Integer -> AppForm (LinkPack, Integer)
> linkPackForm fl = plug (\xhtml -> table << xhtml) $ mkLinkPack
>   <$>  F.hidden (Just $ show fl) `check` ensure ((> 0) . length) "Missing first link number."
>   <*>  plug (tabularInput "Pack Name") (F.input Nothing) `check`
>          ensures (nonEmptyAndLessThan 50 "Pack Name")
>   <*>  plug (tabularInput "Description") (F.textarea Nothing Nothing Nothing) `check`
>          ensures (nonEmptyAndLessThan 5000 "Pack Name")
>   <*>  plug (tabularInput "Image") (nothingIfNull $ fileUpload "/pack/image" "Upload Image")
>  where mkLinkPack ::  String -> String -> String -> Maybe String ->
>                       (LinkPack, Integer)
>        mkLinkPack l n d f = (  LinkPack {  linkPackNumber       = 0,
>                                            linkPackName         = n,
>                                            linkPackDescription  = d,
>                                            linkPackImage        = f,
>                                            linkPackCreator      = undefined },
>                                read l )

> newLinkPack :: App CGIResult
> newLinkPack = withRequiredMemberNumber $ \memberNo -> do
>   uri   <- requestURI
>   meth  <- requestMethod
>   preview <- getInput "preview"
>   firstLink <- readRequiredInput "link"
>   (result, xhtml) <- runForm' $ linkPackForm firstLink
>   case preview of
>     Just _  -> do
>       let preview' = case result of
>                        Failure failures  -> unordList failures
>                        Success (linkPack, _)  -> thediv ! [theclass "preview"] << [
>                                                    h2 << linkPackName linkPack,
>                                                    displayLinkPack linkPack ]
>       simplePage "Create a Link Pack (preview)" deps
>         [  preview',
>            form ! [  thestyle "text-align: center",
>                      action (uriPath uri), method "POST" ] <<
>              [xhtml, actionBar] ]
>     Nothing ->
>       case result of
>         Success (linkPack, fl) -> do
>           linkNo <- createLinkPack linkPack memberNo fl
>           case linkNo of
>             Just n   -> redirect $ "/pack/" ++ show n
>             Nothing  -> error "Failed to establish link."
>         Failure failures -> simplePage "Create a Link Pack" deps
>           [  form ! [  thestyle "text-align: center",
>                        action (uriPath uri), method "POST" ] <<
>                [  meth == "GET" ? noHtml $ unordList failures,
>                   xhtml, actionBar ] ]
>  where deps = [CSS "link", JS "lib.link"]
>        actionBar = thediv ! [thestyle "margin-left: auto; margin-right: auto; \
>                                       \margin-top: 1.3em; width: 12em"] <<
>                      [  submit "preview" "Preview" !
>                           [thestyle "float: left; width: 5.5em"],
>                         submit "" "Create" ! [thestyle "float: right; width: 5.5em"],
>                         paragraph ! [thestyle "clear: both"] << noHtml ]

> displayLinkPack :: LinkPack -> Html
> displayLinkPack lp =
>   thediv ! [theclass "link-pack"] << [
>     linkPackIcon lp,
>     markdownToHtml $ linkPackDescription lp, clear ]

> displayCompactLinkPack :: LinkPack -> Bool -> Html
> displayCompactLinkPack lp displayTitle =
>   thediv ! [theclass "link-pack compact"] << [
>     displayTitle ? h3 << linkPackTextLink lp $ noHtml,
>     linkPackIconLink lp, clear ]

> linkPackIcon :: LinkPack -> Html
> linkPackIcon lp =
>   image ! [  src (  "http://s.vocabulink.com/pack/image/" ++
>                     case linkPackImage lp of
>                       Just i   -> i
>                       Nothing  -> "default.png" ),
>              alt (linkPackName lp) ]

> linkPackHyperlink :: LinkPack -> String
> linkPackHyperlink lp = "/pack/" ++ show (linkPackNumber lp)

> linkPackTextLink :: LinkPack -> Html
> linkPackTextLink lp =
>   anchor ! [href (linkPackHyperlink lp)] << linkPackName lp

> linkPackIconLink :: LinkPack -> Html
> linkPackIconLink lp =
>   anchor ! [href (linkPackHyperlink lp)] << linkPackIcon lp

> createLinkPack :: LinkPack -> Integer -> Integer -> App (Maybe Integer)
> createLinkPack lp memberNo firstLink = do
>   r <- withTransaction' $ do
>     c <- asks appDB
>     packNo <- liftIO $ insertNo c
>       "INSERT INTO link_pack (name, description, image_ext, creator) \
>                      \VALUES (?, ?, ?, ?)"
>       [  toSql (linkPackName lp), toSql (linkPackDescription lp),
>          toSql (takeExtension <$> linkPackImage lp), toSql memberNo ]
>       "link_pack_pack_no_seq"
>     case packNo of
>       Nothing  -> liftIO $ rollback c >> return Nothing
>       Just n   -> do
>         liftIO $ quickStmt c  "INSERT INTO link_pack_link (pack_no, link_no) \
>                                                  \VALUES (?, ?)"
>                               [toSql n, toSql firstLink]
>         return packNo
>   case r of
>     Just (Just r')  -> case linkPackImage lp of
>                          Nothing  -> return $ Just r'
>                          Just i   -> moveImage i >> return (Just r')
>       where  moveImage :: String -> App (Either SomeException ExitCode)
>              moveImage i' = do
>                dir  <- (++ "/pack/image/") . fromJust <$> getOption "staticdir"
>                let old  =  dir ++ i'
>                    new  =  dir ++ addExtension (show r') (takeExtension i')
>                    cmd  =  "mv " ++ old ++ " " ++ new
>                liftIO $ try $ system cmd
>     _               -> return Nothing

> getLinkPack :: Integer -> App (Maybe LinkPack)
> getLinkPack n = do
>   t <- queryTuple'  "SELECT pack_no, name, description, image_ext, creator \
>                     \FROM link_pack WHERE pack_no = ?" [toSql n]
>   case t of
>     Nothing  -> return Nothing
>     Just t'  -> return $ linkPackFromValues t'

> linkPackFromValues :: [SqlValue] -> Maybe LinkPack
> linkPackFromValues [pn, n, d, i, c] =
>   Just LinkPack {  linkPackNumber       = fromSql pn,
>                    linkPackName         = fromSql n,
>                    linkPackDescription  = fromSql d,
>                    linkPackImage        = (fromSql pn ++) <$> fromSql i,
>                    linkPackCreator      = fromSql c }
> linkPackFromValues _ = Nothing

> linkPackLinks :: Integer -> App (Maybe [PartialLink])
> linkPackLinks n = do
>   ts <- queryTuples'  "SELECT l.link_no, l.link_type, l.origin, l.destination, \
>                              \l.origin_language, l.destination_language \
>                       \FROM link_pack_link \
>                       \INNER JOIN link l USING (link_no) \
>                       \WHERE pack_no = ? AND NOT deleted \
>                       \ORDER BY added DESC"
>                       [toSql n]
>   return $ mapMaybe partialLinkFromValues `liftM` ts

> linkPackPage :: Integer -> App CGIResult
> linkPackPage n = do
>   linkPack <- getLinkPack n
>   links <- linkPackLinks n
>   case (linkPack, links) of
>     (Just lp, Just ls)  -> do
>       ls' <- mapM partialLinkHtml ls
>       r <- queryValue'  "SELECT root_comment \
>                         \FROM link_pack_comments \
>                         \WHERE pack_no = ?" [toSql $ linkPackNumber lp]
>       comments <- case r of
>                     Just root  -> renderComments $ fromSql root
>                     Nothing    -> return noHtml
>       simplePage ("Link Pack: " ++ linkPackName lp) [CSS "link"] [
>         thediv ! [theclass "two-column"] << [
>           thediv ! [theclass "column"] << displayLinkPack lp,
>           thediv ! [theclass "column"] << (unordList ls' ! [theclass "links pack-links"]) ],
>         hr ! [theclass "clear"], h3 << "Comments", comments ]
>     _                   -> output404 ["pack",show n]

> deleteLinkPack :: Integer -> App CGIResult
> deleteLinkPack _ = error "Unimplemented."

> addToLinkPack :: App CGIResult
> addToLinkPack = do
>   pack  <- getRequiredInput "pack"
>   link  <- getRequiredInput "link"
>   case pack of
>     "new"  ->  redirect $ "/pack/new?link=" ++ link
>     _      ->  do
>       let  packNo = read pack :: Integer
>            linkNo = read link :: Integer
>       quickStmt' "INSERT INTO link_pack_link (pack_no, link_no) \
>                                      \VALUES (?, ?)" [toSql packNo, toSql linkNo]
>       redirect $ "/pack/" ++ pack

> latestLinkPacks :: App [Maybe LinkPack]
> latestLinkPacks = maybe [] (map linkPackFromValues) <$>
>   queryTuples'  "SELECT pack_no, name, description, image_ext, creator \
>                 \FROM link_pack WHERE NOT deleted \
>                 \ORDER BY pack_no DESC" []

> linkPacksPage :: App CGIResult
> linkPacksPage = do
>   lps <- map (\x -> displayCompactLinkPack x True ! [thestyle "float: left"]) <$>
>            catMaybes <$> latestLinkPacks
>   simplePage "Latest Link Packs" [CSS "link"] lps
