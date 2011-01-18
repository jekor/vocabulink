% Copyright 2008, 2009, 2010, 2011 Chris Forno

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
>                           linksPage, newLink, partialLinkFromTuple,
>                           renderLink, renderPartialLink, languagePairsPage,
>                           languagePairLinks, languageNameFromAbbreviation,
>                           updateLinkStory, LinkPack(..),
>                           newLinkPack, linkPackPage, deleteLinkPack,
>                           addToLinkPack, linkPacksPage, getLinkPack,
>                           displayCompactLinkPack, linkPackTextLink,
>                           linkPackIconLink, addPronunciation, deletePronunciation,
>                           linkOriginLanguage, linkDestinationLanguage,
>                           wordCloud ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Comment
> import Vocabulink.Form
> import Vocabulink.Html
> import Vocabulink.Member
> import Vocabulink.Page
> import Vocabulink.Rating
> import Vocabulink.Utils

> import Control.Monad.State
> import Data.List (find, genericLength)
> import System.Cmd (system)
> import System.IO (Handle)
> import System.Random
> import Text.Blaze.Html5 (audio, source)
> import Text.Blaze.Html5.Attributes (preload)
> import qualified Text.Blaze.Html5.Formlets as HF (input, textarea, hidden, selectRaw)

> import Prelude hiding (div, span, id, words, writeFile)

\subsection{Link Data Types}

Abstractly, a link is defined by the origin and destination lexemes it links,
as well as its type. Practically, we also need to carry around information such
as its link number (in the database) as well as a string representation of its
type (for partially constructed links, which you'll see later).

> data Link = Link {  linkNumber           :: Integer,
>                     linkTypeName         :: String,
>                     linkAuthor           :: Integer,
>                     linkOrigin           :: String,
>                     linkOriginLang       :: String,
>                     linkDestination      :: String,
>                     linkDestinationLang  :: String,
>                     linkType             :: LinkType }

We'll eventually want to support private/unpublished links.

> instance UserContent Link where
>   canView link    = do
>     memberNo <- asks appMemberNo
>     deleted <- fromJust <$> $(queryTuple'
>                  "SELECT deleted FROM link \
>                  \WHERE link_no = {linkNumber link}")
>     return $ if not deleted
>       then True
>       else case memberNo of
>         Nothing -> False
>         Just n  -> n == linkAuthor link || n == 1
>   canEdit link    = do
>     memberNo <- asks appMemberNo
>     return $ case memberNo of
>       Just n   -> n == linkAuthor link || n == 1
>       Nothing  -> False
>   canDelete link  = do
>     edit <- canEdit link
>     if edit
>       then do
>         deleted <- $(queryTuple'
>                      "SELECT deleted FROM link \
>                      \WHERE link_no = {linkNumber link}")
>         return $ case deleted of
>           Just d  -> not d
>           Nothing -> False
>       else return False

The |linkOriginLang| and |linkDestinationLang| are the language abbrevations.
To get the full name we need to look it up.

> languageNameFromAbbreviation :: String -> App (Maybe String)
> languageNameFromAbbreviation abbr = lookup abbr <$> asks appLanguages

> linkOriginLanguage, linkDestinationLanguage :: Link -> App String
> linkOriginLanguage       l = fromJust <$> languageNameFromAbbreviation (linkOriginLang l)
> linkDestinationLanguage  l = fromJust <$> languageNameFromAbbreviation (linkDestinationLang l)

We can associate 2 lexemes in many different ways. Because different linking
methods require different information, they each need different representations
in the database. This leads to some additional complexity.

Each link between lexemes has a type. This type determines how the link is
displayed, edited, used in statistical analysis, etc. See the Vocabulink
handbook for a more in-depth description of the types.

> data LinkType =  Association | SoundAlike | LinkWord String String |
>                  Relationship String String
>                  deriving (Show)

Sometimes we need to work with a human-readable name, such as when interacting
with a client or the database.

I used to call cognates "cognates" on the site, but it's a confusing term for
most people. Now, I call it a "sound-alike".

> linkTypeNameFromType :: LinkType -> String
> linkTypeNameFromType Association         = "association"
> linkTypeNameFromType SoundAlike          = "sound-alike"
> linkTypeNameFromType (LinkWord _ _)      = "linkword"
> linkTypeNameFromType (Relationship _ _)  = "relationship"

Links are created by members. Vocabulink does not own them. It merely has a
license to use them (as part of the Terms of Use). So when displaying a link in
full, we display a copyright notice with the member's username.

Of course, simple link types without nothing other than the origin and
destination of the link do not contain copyrightable material.

> linkCopyright :: Link -> App Html
> linkCopyright l =
>   case linkType l of
>     LinkWord _ _ -> do
>       (u,c,up) <- fromJust <$> $(queryTuple'
>         "SELECT username, created, updated \
>         \FROM link, member \
>         \WHERE member_no = author AND link_no = {linkNumber l}")
>       c'  <- liftIO $ serverYear c
>       up' <- liftIO $ serverYear up
>       let range = c' == up' ? show c' $ show c' ++ "–" ++ show up'
>       return $ p ! class_ "copyright" $ string $ "Copyright " ++ range ++ " " ++ u
>     _            -> return mempty

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
types (such as sound-alikes) have no additional information and hence no
relation in the database.

This returns the newly established link number.

> establishLink :: Link -> Integer -> App (Integer)
> establishLink l memberNo = do
>   h <- asks appDB
>   liftIO $ withTransaction h $ do
>     linkNo <- fromJust <$> $(queryTuple
>       "INSERT INTO link (origin, destination, \
>                         \origin_language, destination_language, \
>                         \link_type, author) \
>                 \VALUES ({linkOrigin l}, {linkDestination l}, \
>                         \{linkOriginLang l}, {linkDestinationLang l}, \
>                         \{linkTypeName l}, {memberNo}) \
>       \RETURNING link_no") h
>     establishLinkType h (l {linkNumber = linkNo})
>     return linkNo

The relation we insert additional details into depends on the type of the link
and it's easiest to use a separate function for it.

> establishLinkType :: Handle -> Link -> IO ()
> establishLinkType h l = case linkType l of
>   Association                -> return ()
>   SoundAlike                 -> return ()
>   (LinkWord word story)      -> do
>     $(execute "INSERT INTO link_type_link_word \
>                      \(link_no, link_word, story) \
>               \VALUES ({linkNumber l}, {word}, {story})") h
>   (Relationship left right)  -> do
>     $(execute "INSERT INTO link_type_relationship \
>                      \(link_no, left_side, right_side) \
>               \VALUES ({linkNumber l}, {left}, {right})") h

\subsection{Retrieving Links}

Now that we've seen how we store links, let's look at retrieving them (which is
slightly more complicated in order to allow for efficient retrieval of multiple
links).

Retrieving a partial link is simple.

> getPartialLink :: Integer -> App (Maybe PartialLink)
> getPartialLink linkNo = partialLinkFromTuple <$$> $(queryTuple'
>   "SELECT link_no, link_type, author, \
>          \origin, destination, \
>          \origin_language, destination_language \
>   \FROM link \
>   \WHERE link_no = {linkNo}")

We use a helper function to convert the raw SQL tuple to a partial link value.
Note that we leave the link's |linkType| undefined.

> partialLinkFromTuple :: (Integer, String, Integer, String, String, String, String) -> PartialLink
> partialLinkFromTuple (n, t, u, o, d, ol, dl) =
>   PartialLink Link {  linkNumber           = n,
>                       linkTypeName         = t,
>                       linkAuthor           = u,
>                       linkOrigin           = o,
>                       linkDestination      = d,
>                       linkOriginLang       = ol,
>                       linkDestinationLang  = dl,
>                       linkType             = undefined }

Once we have a partial link, it's a simple matter to turn it into a full link.
We just need to retrieve its type-level details from the database.

> getLinkFromPartial :: PartialLink -> App (Maybe Link)
> getLinkFromPartial (PartialLink partial) = do
>   linkT <- getLinkType (PartialLink partial)
>   return $ (\t -> Just $ partial {linkType = t}) =<< linkT

> getLinkType :: PartialLink -> App (Maybe LinkType)
> getLinkType (PartialLink pl) = case pl of
>   (Link {  linkTypeName  = "association" })  -> return $ Just Association
>   (Link {  linkTypeName  = "sound-alike"})   -> return $ Just SoundAlike
>   (Link {  linkTypeName  = "linkword",
>            linkNumber    = n })              -> do
>     row <- $(queryTuple' "SELECT link_word, story FROM link_type_link_word \
>                          \WHERE link_no = {n}")
>     return $ (\ (w, s) -> LinkWord w s) <$> row
>   (Link {  linkTypeName  = "relationship",
>            linkNumber    = n })              -> do
>     row <- $(queryTuple' "SELECT left_side, right_side \
>                          \FROM link_type_relationship \
>                          \WHERE link_no = {n}")
>     return $ (\ (l, r) -> LinkWord l r) <$> row
>   _                                          -> error "Bad partial link."

We now have everything we need to retrieve a full link in 1 step.

> getLink :: Integer -> App (Maybe Link)
> getLink linkNo = getPartialLink linkNo >>= maybe (return Nothing) getLinkFromPartial

We already know what types of links exist, but we want only the active link
types (some, like Relationship, are experimental) sorted by how common they
are.

> activeLinkTypes :: [String]
> activeLinkTypes = ["linkword", "sound-alike", "association"]

\subsection{Deleting Links}

Links can be deleted by their owner. They're not actually removed from the
database, as doing so would require removing the link from other members'
review sets. Instead, we just flag the link as deleted so that it doesn't
appear in most contexts.

> deleteLink :: Integer -> App CGIResult
> deleteLink linkNo = do
>   $(execute' "UPDATE link SET deleted = TRUE \
>              \WHERE link_no = {linkNo}")
>   outputJSON [(""::String, ""::String)]

\subsection{Displaying Links}

<h1 class="link linkword">
    <span class="orig" title="Esperanto">nur</span>
    <span class="link" title="linkword">newer</span>
    <span class="dest" title="English">only</span>
</h1>

<h2 class="link sound-alike">
    <span class="orig" title="Esperanto">lingvo</span>
    <span class="link" title="sound-alike"></span>
    <span class="dest" title="English">language</span>
</h2>

We really shouldn't need to allow for passing class names. However, the !
function has a problem in that it will add another class attribute instead of
extending the existing one, which at least jquery doesn't like.

TODO: These queries might be better as functions.

An adjacent link is the nearest (by link number) in the given language pair.

> adjacentLinkNumbers :: Link -> App (Maybe Integer, Maybe Integer)
> adjacentLinkNumbers link = do
>   let oLang = linkOriginLang link
>       dLang = linkDestinationLang link
>   prevLink <- $(queryTuple'
>     "(SELECT link_no FROM link \
>      \WHERE origin_language = {oLang} \
>        \AND destination_language = {dLang} \
>        \AND link_no < {linkNumber link} \
>        \AND NOT deleted \
>      \ORDER BY link_no DESC LIMIT 1) \
>     \UNION \
>     \(SELECT link_no FROM link \
>      \WHERE origin_language = {oLang} \
>      \AND destination_language = {dLang} \
>      \AND NOT link_no = {linkNumber link} \
>      \AND NOT deleted \
>      \ORDER BY link_no DESC LIMIT 1) \
>      \ORDER BY link_no ASC")
>   nextLink <- $(queryTuple'
>     "(SELECT link_no FROM link \
>      \WHERE origin_language = {oLang} \
>        \AND destination_language = {dLang} \
>        \AND link_no > {linkNumber link} \
>        \AND NOT deleted \
>      \ORDER BY link_no ASC LIMIT 1) \
>     \UNION \
>     \(SELECT link_no FROM link \
>      \WHERE origin_language = {oLang} \
>        \AND destination_language = {dLang} \
>        \AND NOT link_no = {linkNumber link} \
>        \AND NOT deleted \
>      \ORDER BY link_no ASC LIMIT 1) \
>     \ORDER BY link_no DESC")
>   return (fromJust <$> prevLink, fromJust <$> nextLink)

> renderLink :: Link -> Bool -> Bool -> App Html
> renderLink link pronounceable' paging = do
>   oLanguage <- linkOriginLanguage link
>   dLanguage <- linkDestinationLanguage link
>   (prevLink, nextLink) <- if paging
>                             then adjacentLinkNumbers link
>                             else return (Nothing, Nothing)
>   return $ h1 ! class_ (stringValue $ "link " ++ linkTypeName link) $ do
>     maybe mempty (\n -> a ! href (stringValue $ show n) ! class_ "prev"
>                           ! title (stringValue $ "Previous " ++ oLanguage ++ "→" ++ dLanguage ++ " Link") $ mempty) prevLink
>     span ! class_ "orig" ! title (stringValue oLanguage) $ do
>       string $ linkOrigin link
>       pronunciation
>     span ! class_ "link" ! title (stringValue $ linkTypeName link) $
>       (renderLinkType $ linkType link)
>     span ! class_ "dest" ! title (stringValue dLanguage) $ string $ linkDestination link
>     maybe mempty (\n -> a ! href (stringValue $ show n) ! class_ "next"
>                           ! title (stringValue $ "Next " ++ oLanguage ++ "→" ++ dLanguage ++ " Link") $ mempty) nextLink
>  where renderLinkType :: LinkType -> Html
>        renderLinkType (LinkWord word _) = string word
>        renderLinkType _                 = mempty
>        pronunciation = if pronounceable'
>                          then do
>                            a ! id "pronounce" ! class_ "button" $ do
>                              audio ! preload "auto" $ do
>                                source ! src (stringValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (linkNumber link) ++ ".ogg") $ mempty
>                                source ! src (stringValue $ "http://s.vocabulink.com/audio/pronunciation/" ++ show (linkNumber link) ++ ".mp3") $ mempty
>                              img ! src "http://s.vocabulink.com/img/icon/audio.png"
>                          else mempty

> renderPartialLink :: PartialLink -> App Html
> renderPartialLink (PartialLink l) = do
>   originLanguage      <- linkOriginLanguage l
>   destinationLanguage <- linkDestinationLanguage l
>   return $ do
>     a ! class_ (stringValue $ "partial-link " ++ linkTypeName l)
>       ! href (stringValue $ "/link/" ++ show (linkNumber l))
>       ! title (stringValue $ originLanguage ++ " → " ++ destinationLanguage) $ do
>       span ! class_ "orig" $ string $ linkOrigin l
>       string " → "
>       span ! class_ "dest" $ string $ linkDestination l

Displaying an entire link involves not just drawing a graphical representation
of the link but displaying its type-level details as well.

> displayLink :: Link -> App Html
> displayLink l = do
>   renderedLink <- renderLink l False True
>   return $ do
>     renderedLink
>     div ! class_ "link-details htmlfrag" $ linkTypeHtml (linkType l)

Like displayLink, but don't try to do anything with the link number (since it's
undefined).

> previewLink :: Link -> App Html
> previewLink link = do
>   oLanguage <- linkOriginLanguage link
>   dLanguage <- linkDestinationLanguage link
>   return $ do
>     h1 ! class_ (stringValue $ "link " ++ linkTypeName link) $ do
>       span ! class_ "orig" ! title (stringValue oLanguage) $ string $ linkOrigin link
>       span ! class_ "link" ! title (stringValue $ linkTypeName link) $
>         (renderLinkType $ linkType link)
>       span ! class_ "dest" ! title (stringValue dLanguage) $ string $ linkDestination link
>     div ! class_ "link-details htmlfrag" $ linkTypeHtml (linkType link)
>  where renderLinkType :: LinkType -> Html
>        renderLinkType (LinkWord word _) = string word
>        renderLinkType _                 = mempty

> linkTypeHtml :: LinkType -> Html
> linkTypeHtml Association = mempty
> linkTypeHtml SoundAlike = mempty
> linkTypeHtml (LinkWord _ story) =
>   markdownToHtml story
> linkTypeHtml (Relationship leftSide rightSide) =
>   p ! style "text-align: center" $ do
>     string "as"
>     br
>     string $ leftSide ++ " → " ++ rightSide

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
>     Nothing -> outputNotFound
>     Just l' -> do
>       viewable <- canView l'
>       if not viewable
>         then outputNotFound
>         else do
>           let owner' = maybe False (linkAuthor l' ==) memberNo
>           ops <- linkOperations l'
>           (c, r) <- ((first fromJust) . fromJust) <$> $(queryTuple'
>             "SELECT COUNT(rating), SUM(rating) / COUNT(rating) \
>             \FROM link_rating WHERE link_no = {linkNo}")
>           ratingEnabled <- case memberNo of
>                              Just n  -> isNothing <$> $(queryTuple'
>                                "SELECT rating FROM link_rating \
>                                \WHERE link_no = {linkNo} AND member_no = {n}")
>                              Nothing -> return False
>           hasPronunciation <- pronounceable l'
>           copyright <- linkCopyright l'
>           oLanguage <- linkOriginLanguage l'
>           dLanguage <- linkDestinationLanguage l'
>           let orig  = linkOrigin l'
>               dest  = linkDestination l'
>           row <- $(queryTuple' "SELECT root_comment \
>                                \FROM link_comments \
>                                \WHERE link_no = {linkNo}")
>           comments <- case row of
>                         Just root  -> renderComments root
>                         Nothing    -> return mempty
>           renderedLink <- renderLink l' hasPronunciation True
>           stdPage (orig ++ " → " ++ dest) [CSS "link", JS "lib.link"] mempty $ do
>             div ! id "link-head-bar" $ do
>               h2 $ a ! href (stringValue $ "/links?ol=" ++ linkOriginLang l' ++ "&dl=" ++ linkDestinationLang l') $
>                 string (oLanguage ++ " to " ++ dLanguage ++ ":")
>               div ! id "link-ops" $ do
>                 ops
>                 ratingBar ("/link" </> show linkNo </> "rating") c r ratingEnabled
>             renderedLink
>             div ! class_ "link-details" $ do
>               case (owner', linkType l') of
>                 (True, LinkWord _ story)  -> textarea ! style "display: none" $ string $ story
>                 _                         -> mempty
>               div ! id "link-details" ! class_ "htmlfrag" $ linkTypeHtml (linkType l')
>             copyright
>             clear
>             hr ! style "margin-top: 1.3em"
>             h3 $ "Comments"
>             comments

Each link can be ``operated on''. It can be reviewed (added to the member's
review set) and deleted (marked as deleted). In the future, I expect operations
such as ``tag'', ``rate'', etc.

The |Bool| parameter indicates whether or not the currently logged-in member
(if the client is logged in) is the owner of the link.

> linkAction :: String -> String -> Bool -> Html
> linkAction label' icon' enabled =
>   let icon = "http://s.vocabulink.com/img/icon/" ++
>              icon' ++
>              (enabled ? "" $ "-disabled") ++
>              ".png" in
>   a ! class_ (stringValue $ "operation " ++ (enabled ? "enabled" $ "disabled")) $ do
>     img ! src (stringValue icon) ! class_ "icon"
>     string label'

> linkOperations :: Link -> App Html
> linkOperations link = do
>   memberNo     <- asks appMemberNo
> --  memberEmail  <- asks appMemberEmail
>   editable    <- canEdit link
>   deletable   <- canDelete link
>   reviewing'  <- reviewing link
>   hasPron     <- pronounceable link
>   let review  = linkAction "add to review" "add"
> --      pack    = linkAction "add to link pack" "add-to-link-pack"
>   return $ do
>     case (memberNo, reviewing') of
>       (_,        True) -> (review False) ! title "already reviewing this link"
>       (Just _,   _)    -> (review True)  ! id "link-op-review"
>                                          ! title "add this link to be quizzed on it later"
>       (Nothing,  _)    -> (review False) ! href "/member/login" ! title "login to review"
>     case (editable, hasPron) of
>       (True, False) -> (linkAction "add pronunciation" "audio-add" True)
>                          ! id "link-op-add-pronunciation"
>                          ! title "add an audio file showing pronunciation"
>       (True, True)  -> (linkAction "delete pronunciation" "audio-delete" True)
>                          ! id "link-op-delete-pronunciation"
>                          ! title "delete the audio file showing pronunciation"
>       _             -> mempty
>     case (editable, linkType link) of
>       (True, LinkWord _ _) -> (linkAction "edit link" "edit" True)
>                                 ! id "link-op-edit"
>                                 ! title "edit the linkword story"
>       _                    -> mempty
> --    case (memberNo, memberEmail) of
> --      (_, Just _)        -> (pack True)  ! [identifier "link-op-pack", title "add this link to a collection of other links"]
> --      (Just _, Nothing)  -> (pack False) ! [href "/member/confirmation", title "confirm your email to add this link to a link pack"]
> --      (Nothing, _)       -> (pack False) ! [href "/member/login", title "login to add this link to a link pack"],
>     when deletable ((linkAction "delete link" "delete" True)
>            ! id "link-op-delete"
>            ! title "delete this link (it will still be visibles to others who are reviewing it)")
>  where reviewing :: Link -> App Bool
>        reviewing l = do
>          memberNo <- asks appMemberNo
>          case memberNo of
>            Nothing -> return False
>            Just n  -> (/= []) <$> $(queryTuples'
>              "SELECT link_no FROM link_to_review \
>              \WHERE member_no = {n} AND link_no = {linkNumber l} \
>              \LIMIT 1")

> addToLinkPackForm :: Integer -> App Html
> addToLinkPackForm n = do
>   memberNo <- asks appMemberNo
>   case memberNo of
>     Nothing -> return mempty
>     Just mn -> do
>       -- Find all of the packs created by this member that don't already contain
>       -- this link.
>       packs <- $(queryTuples'
>         "SELECT pack_no, name FROM link_pack \
>         \WHERE creator = {mn} \
>           \AND pack_no NOT IN (SELECT pack_no FROM link_pack_link \
>                               \WHERE link_no = {n}) \
>         \ORDER BY pack_no DESC")
> --      let ps = map (\ (num, name) -> (show num, name)) packs
>       let ps = map (first show) packs
>       return $ do
>         button ! class_ "reveal add-to-pack" $ string "→ Pack"
>         form ! action "/pack/link/new" ! method "post" ! id "add-to-pack" ! style "display: none" $ do
>           input ! type_ "hidden" ! name "link" ! value (stringValue $ show n)
>           menu "pack" (ps ++ [("new", "New Pack")]) ! id "pack-select"
>           br
>           input ! type_ "submit" ! value "→ Pack"

\subsection{Finding Links}

While Vocabulink is still small, it makes sense to have a page just for
displaying all the (non-deleted) links in the system. This will probably go
away eventually.

> linksPage :: String -> (Int -> Int -> App [PartialLink]) -> App CGIResult
> linksPage title' f = do
>   (pg, n, offset) <- currentPage
>   ts <- f offset (n + 1)
>   pagerControl <- pager pg n $ offset + length ts
>   partialLinks <- mapM renderPartialLink (take n ts)
>   simplePage title' [CSS "link"] $ do
>     unordList partialLinks ! id "central-column" ! class_ "links"
>     pagerControl

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
>   (status, html) <- runForm' establishF
>   case preview of
>     Just _  -> do
>       preview' <- case status of
>                     Failure failures  -> return $ unordList $ map string failures
>                     Success link      -> do
>                       displayedLink <- previewLink link
>                       return $ div ! class_ "preview" $ displayedLink
>       simplePage "Create a Link (preview)" deps $ do
>         preview'
>         form ! style "text-align: center" ! action (stringValue $ uriPath uri) ! method "post" $ do
>           html
>           actionBar
>     Nothing ->
>       case status of
>         Failure failures  -> simplePage "Create a Link" deps $ do
>           form ! style "text-align: center" ! action (stringValue $ uriPath uri) ! method "post" $ do
>             when (meth /= "GET") (unordList $ map string failures)
>             html
>             actionBar
>         Success link -> do
>           linkNo <- establishLink link memberNo
>           redirect $ "/link/" ++ show linkNo
>  where deps = [CSS "link", JS "lib.link"]
>        actionBar = div ! style "margin-left: auto; margin-right: auto; width: 12em" $ do
>                      input ! type_ "submit" ! name "preview" ! value "Preview"
>                            ! style "float: left; width: 5.5em"
>                      input ! type_ "submit" ! name "" ! value "Link"
>                            ! style "float: right; width: 5.5em"
>                      p ! style "clear: both" $ mempty

Here's a form for creating a link. It gathers all of the required details
(origin, destination, and link type details).

> establish :: [String] -> App (AppForm Link)
> establish ts = do
>   originPicker      <- languagePicker $ Left ()
>   destinationPicker <- languagePicker $ Right ()
>   return (mkLink  <$> lexemeInput "Foreign"
>                   <*> plug (`mappend` (string " ")) originPicker
>                   <*> lexemeInput "Native"
>                   <*> destinationPicker
>                   <*> linkTypeInput ts)

When creating a link from a form, the link number must be undefined until the
link is established in the database. Also, because of the way formlets work (or
how I'm using them), we need to retrieve the link type name from the link type.

> mkLink :: String -> String -> String -> String -> LinkType -> Link
> mkLink o ol d dl t = Link {  linkNumber           = undefined,
>                              linkAuthor           = undefined,
>                              linkTypeName         = linkTypeNameFromType t,
>                              linkOrigin           = o,
>                              linkOriginLang       = ol,
>                              linkDestination      = d,
>                              linkDestinationLang  = dl,
>                              linkType             = t }

The lexeme is the origin or destination of the link.

> lexemeInput :: String -> AppForm String
> lexemeInput l = l `formLabel` HF.input Nothing `check` ensures
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
>   memberNo <- asks appMemberNo
>   let side' = case side of
>                 Left  _ -> "origin"
>                 Right _ -> "destination"
>   langs <- case memberNo of
>     Nothing -> return []
>     Just n  -> case side of
>                  Left  _ -> $(queryTuples'
>                    "SELECT abbr, name \
>                    \FROM link, language \
>                    \WHERE language.abbr = link.origin_language \
>                      \AND link.author = {n} \
>                    \GROUP BY origin_language, abbr, name \
>                    \ORDER BY COUNT(origin_language) DESC")
>                  Right _ -> $(queryTuples'
>                    "SELECT abbr, name \
>                    \FROM link, language \
>                    \WHERE language.abbr = link.destination_language \
>                      \AND link.author = {n} \
>                    \GROUP BY destination_language, abbr, name \
>                    \ORDER BY COUNT(destination_language) DESC")
>   allLangs <- asks appLanguages
>   let choices = map (second string) langs ++ [("","")] ++ map (second string) allLangs
>   return $ HF.selectRaw choices (Just $ fst . head $ choices) `check` ensures
>              [((/= ""), side' ++ " language is required") ]

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
> linkTypeInput ts = (linkTypeS  <$> plug (\html -> p $ do
>                                                     html
>                                                     helpButton "/article/understanding-link-types" Nothing)
>                                      ("Link Type" `formLabel` linkSelect Nothing)
>                                <*> pure Association
>                                <*> pure SoundAlike
>                                <*> fieldset' "linkword" linkTypeLinkWord
>                                <*> fieldset' "relationship" linkTypeRelationship)
>                    `check` ensure complete
>                      "Please fill in all the link type fields."
>   where linkSelect                   = HF.selectRaw $ zip ts (map string ts)
>         complete Association         = True
>         complete SoundAlike          = True
>         complete (LinkWord w s)      = (w /= "") && (s /= "")
>         complete (Relationship l r)  = (l /= "") && (r /= "")
>         fieldset' ident              = plug (fieldset ! id ident ! style "display: none")

> linkTypeS :: String -> LinkType -> LinkType -> LinkType -> LinkType -> LinkType
> linkTypeS "association"   l _ _ _  = l
> linkTypeS "sound-alike"   _ l _ _  = l
> linkTypeS "linkword"      _ _ l _  = l
> linkTypeS "relationship"  _ _ _ l  = l
> linkTypeS _               _ _ _ _  = error "Unknown link type."

> linkTypeLinkWord :: AppForm LinkType
> linkTypeLinkWord = LinkWord  <$> "Link Word" `formLabel'` HF.input Nothing
>   <*> linkTypeLinkWordStory "Write a story linking the 2 words here."

> linkTypeLinkWordStory :: String -> AppForm String
> linkTypeLinkWordStory = HF.textarea Nothing Nothing . Just

> linkTypeRelationship :: AppForm LinkType
> linkTypeRelationship = Relationship <$>
>   plug (`mappend` (string " is to ")) (HF.input Nothing) <*> HF.input Nothing

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

> latestLinks :: Int -> Int -> App [PartialLink]
> latestLinks offset limit =
>   map partialLinkFromTuple <$> $(queryTuples'
>     "SELECT link_no, link_type, author, \
>            \origin, destination, \
>            \origin_language, destination_language \
>     \FROM link \
>     \WHERE NOT deleted \
>     \ORDER BY link_no DESC \
>     \OFFSET {offset} LIMIT {limit}")

Another way we retrieve links is by author (member). These just happen to be
sorted by link number as well.

> memberLinks :: Integer -> Int -> Int -> App [PartialLink]
> memberLinks memberNo offset limit =
>   map partialLinkFromTuple <$> $(queryTuples'
>     "SELECT link_no, link_type, author, \
>            \origin, destination, \
>            \origin_language, destination_language \
>     \FROM link \
>     \WHERE NOT deleted AND author = {memberNo} \
>     \ORDER BY link_no DESC \
>     \OFFSET {offset} LIMIT {limit}")

> languagePairLinks :: String -> String -> Int -> Int -> App [PartialLink]
> languagePairLinks ol dl offset limit = do
>   map partialLinkFromTuple <$> $(queryTuples'
>     "SELECT link_no, link_type, author, \
>            \origin, destination, \
>            \origin_language, destination_language \
>     \FROM link \
>     \WHERE NOT deleted \
>       \AND origin_language = {ol} AND destination_language = {dl} \
>     \ORDER BY link_no DESC \
>     \OFFSET {offset} LIMIT {limit}")

Once we have a significant number of links, browsing through latest becomes
unreasonable for finding links for just the language we're interested in. To
aid in this, it helps to know which language pairs are in use and to know how
many links exist for each so that we can arrange by popularity.

Since we need both the language abbreviation and name (we use the abbreviation
in URLs and the name for display to the client), we return these as triples:
(language, language, count).

> linkLanguages :: App [((String, String), (String, String), Integer)]
> linkLanguages =
>   map linkLanguages' <$> $(queryTuples'
>     "SELECT origin_language, orig.name, \
>            \destination_language, dest.name, \
>            \COUNT(*) \
>     \FROM link \
>     \INNER JOIN language orig ON (orig.abbr = origin_language) \
>     \INNER JOIN language dest ON (dest.abbr = destination_language) \
>     \WHERE NOT deleted \
>     \GROUP BY origin_language, orig.name, \
>              \destination_language, dest.name \
>     \ORDER BY COUNT(*) DESC")
>  where linkLanguages' (oa, on, da, dn, c) = ((oa, on), (da, dn), fromJust c)

Now we can use |linkLanguages| to create a page via which clients can browse
the site.

> languagePairsPage :: App CGIResult
> languagePairsPage = do
>   languages' <- linkLanguages
>   simplePage "Links by Language Pair" [CSS "link"] $ do
>     multiColumnList 3 $ map languagePairLink languages'

Display a hyperlink for a language pair.

> languagePairLink :: ((String, String), (String, String), Integer) -> Html
> languagePairLink ((oa, on), (da, dn), c) =
>   a ! class_ "language-pair" ! href (stringValue $"/links?ol=" ++ oa ++ "&dl=" ++ da) $ do
>     string $ on ++ " → " ++ dn ++ " (" ++ show c ++ ")"

> data LinkPack = LinkPack {  linkPackNumber       :: Integer,
>                             linkPackName         :: String,
>                             linkPackDescription  :: String,
>                             linkPackImage        :: Maybe FilePath,
>                             linkPackCreator      :: Integer }

We all make mistakes, and sometimes a member will need to clean up a link
story. This allows them to make the edit. Originally I left this unimplemented
since I didn't want major edits to throw off someone's reviewing, so I was
going to implement versioning. While that's still a possibility, it seems like
more work than it's worth for now.

This only works for linkword links.

TODO: Check that the new trimmed body is not empty.

> updateLinkStory :: Integer -> App CGIResult
> updateLinkStory linkNo = withRequiredMemberNumber $ \memberNo -> do
>   story <- getBody
>   link <- getPartialLink linkNo
>   case link of
>     Nothing  -> outputNotFound
>     Just l'  ->
>       if linkTypeName (pLink l') == "linkword"
>         then if linkAuthor (pLink l') == memberNo
>                then do
>                  $(execute' "UPDATE link_type_link_word SET story = {story} \
>                             \WHERE link_no = {linkNo}")
>                  outputHtml $ markdownToHtml story
>                else error "Unauthorized."
>         else error "Unsupported link type."

> linkPackForm :: Integer -> AppForm (LinkPack, Integer)
> linkPackForm fl = plug table $ mkLinkPack
>   <$>  HF.hidden (Just $ show fl) `check` ensure ((> 0) . length) "Missing first link number."
>   <*>  plug (tabularInput "Pack Name") (HF.input Nothing) `check`
>          ensures (nonEmptyAndLessThan 50 "Pack Name")
>   <*>  plug (tabularInput "Description") (HF.textarea Nothing Nothing Nothing) `check`
>          ensures (nonEmptyAndLessThan 5000 "Pack Name")
>   <*>  plug (tabularInput "Image") (nothingIfNull $ fileUpload "Upload Image")
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
>   (result, html) <- runForm' $ linkPackForm firstLink
>   case preview of
>     Just _  -> do
>       let preview' = case result of
>                        Failure failures      -> unordList $ map string failures
>                        Success (linkPack, _) -> div ! class_ "preview" $ do
>                                                   h2 $ string $ linkPackName linkPack
>                                                   displayLinkPack linkPack
>       simplePage "Create a Link Pack (preview)" deps $ do
>         preview'
>         form ! style "text-align: center" ! action (stringValue $ uriPath uri) ! method "post" $ do
>           html
>           actionBar
>     Nothing ->
>       case result of
>         Success (linkPack, fl) -> do
>           linkNo <- createLinkPack linkPack memberNo fl
>           redirect $ "/pack/" ++ show linkNo
>         Failure failures -> simplePage "Create a Link Pack" deps $ do
>           form ! style "text-align: center" ! action (stringValue $ uriPath uri) ! method "post" $ do
>             when (meth /= "GET") (unordList $ map string failures)
>             html
>             actionBar
>  where deps = [CSS "link", JS "lib.link", JS "lib.upload"]
>        actionBar = div ! style "margin-left: auto; margin-right: auto; margin-top: 1.3em; width: 12em" $ do
>                      input ! type_ "submit" ! name "preview" ! value "Preview"
>                            ! style "float: left; width: 5.5em"
>                      input ! type_ "submit" ! value "Create"
>                            ! style "float: right; width: 5.5em"
>                      p ! style "clear: both" $ mempty

> displayLinkPack :: LinkPack -> Html
> displayLinkPack lp =
>   div ! class_ "link-pack" $ do
>     linkPackIcon lp
>     markdownToHtml $ linkPackDescription lp
>     clear

> displayCompactLinkPack :: LinkPack -> Bool -> Html
> displayCompactLinkPack lp displayTitle =
>   div ! class_ "link-pack compact" $ do
>     when displayTitle (h3 $ linkPackTextLink lp)
>     linkPackIconLink lp
>     clear

> linkPackIcon :: LinkPack -> Html
> linkPackIcon lp =
>   img ! src (stringValue $ "http://s.vocabulink.com/img/pack/" ++ icon)
>       ! alt (stringValue $ linkPackName lp)
>  where icon = fromMaybe "default.png" (linkPackImage lp)

> linkPackHyperlink :: LinkPack -> String
> linkPackHyperlink lp = "/pack/" ++ show (linkPackNumber lp)

> linkPackTextLink :: LinkPack -> Html
> linkPackTextLink lp = a ! href (stringValue $ linkPackHyperlink lp) $ string $ linkPackName lp

> linkPackIconLink :: LinkPack -> Html
> linkPackIconLink lp = a ! href (stringValue $ linkPackHyperlink lp) $ linkPackIcon lp

> createLinkPack :: LinkPack -> Integer -> Integer -> App Integer
> createLinkPack lp memberNo firstLink = do
>   h <- asks appDB
>   n <- liftIO $ withTransaction h $ do
>     packNo <- liftIO $ fromJust <$> case linkPackImage lp of
>       Nothing -> $(queryTuple
>         "INSERT INTO link_pack (name, description, \
>                                \creator) \
>                        \VALUES ({linkPackName lp}, {linkPackDescription lp}, \
>                                \{memberNo}) \
>         \RETURNING pack_no") h
>       Just i  -> $(queryTuple
>         "INSERT INTO link_pack (name, description, \
>                                \image_ext, creator) \
>                        \VALUES ({linkPackName lp}, {linkPackDescription lp}, \
>                                \{takeExtension i}, {memberNo}) \
>         \RETURNING pack_no") h
>     $(execute "INSERT INTO link_pack_link (pack_no, link_no) \
>                                   \VALUES ({packNo}, {firstLink})") h
>     return packNo
>   when (isJust $ linkPackImage lp) (moveImage (fromJust $ linkPackImage lp) n)
>   return n
>  where moveImage :: FilePath -> Integer -> App ()
>        moveImage i' r = do
>          dir <- (</> "img" </> "pack") <$> asks appDir
>          let old = dir </> i'
>              new = dir </> show r <.> takeExtension i'
>              cmd = "mv " ++ old ++ " " ++ new
>          _ <- liftIO $ system cmd
>          return ()

> getLinkPack :: Integer -> App (Maybe LinkPack)
> getLinkPack n = linkPackFromTuple <$$> $(queryTuple'
>   "SELECT pack_no, name, description, image_ext, creator \
>   \FROM link_pack WHERE pack_no = {n}")

> linkPackFromTuple :: (Integer, String, String, Maybe String, Integer) -> LinkPack
> linkPackFromTuple (pn, n, d, i, c) =
>   LinkPack {  linkPackNumber       = pn,
>               linkPackName         = n,
>               linkPackDescription  = d,
>               linkPackImage        = liftM (\ ext -> show pn ++ ext) i,
>               linkPackCreator      = c }

> linkPackLinks :: Integer -> App [PartialLink]
> linkPackLinks n =
>   map partialLinkFromTuple <$> $(queryTuples'
>     "SELECT l.link_no, l.link_type, l.author, \
>            \l.origin, l.destination, \
>            \l.origin_language, l.destination_language \
>     \FROM link_pack_link \
>     \INNER JOIN link l USING (link_no) \
>     \WHERE pack_no = {n} AND NOT deleted \
>     \ORDER BY added DESC")

> linkPackPage :: Integer -> App CGIResult
> linkPackPage n = do
>   linkPack <- getLinkPack n
>   links <- linkPackLinks n
>   case (linkPack, links) of
>     (Just lp, ls)  -> do
>       ls' <- mapM renderPartialLink ls
>       row <- $(queryTuple' "SELECT root_comment \
>                            \FROM link_pack_comments \
>                            \WHERE pack_no = {linkPackNumber lp}")
>       comments <- case row of
>                     Just root  -> renderComments root
>                     Nothing    -> return mempty
>       simplePage ("Link Pack: " ++ linkPackName lp) [CSS "link"] $ do
>         div ! class_ "two-column" $ do
>           div ! class_ "column" $  displayLinkPack lp
>           div ! class_ "column" $ (unordList ls' ! class_ "links pack-links")
>         hr ! class_ "clear"
>         h3 $ "Comments"
>         comments
>     _              -> outputNotFound

> deleteLinkPack :: Integer -> App CGIResult
> deleteLinkPack _ = error "Unimplemented."

> addToLinkPack :: App CGIResult
> addToLinkPack = do
>   pack <- getRequiredInput "pack"
>   link <- getRequiredInput "link"
>   case pack of
>     "new" -> redirect $ "/pack/new?link=" ++ link
>     _     -> do
>       let  packNo = read pack :: Integer
>            linkNo = read link :: Integer
>       $(execute' "INSERT INTO link_pack_link (pack_no, link_no) \
>                                      \VALUES ({packNo}, {linkNo})")
>       redirect $ "/pack/" ++ pack

> latestLinkPacks :: App [LinkPack]
> latestLinkPacks =
>   map linkPackFromTuple <$> $(queryTuples'
>     "SELECT pack_no, name, description, image_ext, creator \
>     \FROM link_pack WHERE NOT deleted \
>     \ORDER BY pack_no DESC")

> linkPacksPage :: App CGIResult
> linkPacksPage = do
>   lps <- map (\x -> displayCompactLinkPack x True ! style "float: left") <$> latestLinkPacks
>   simplePage "Latest Link Packs" [CSS "link"] $ mconcat lps

> pronounceable :: Link -> App Bool
> pronounceable link = isJust <$> $(queryTuple'
>   "SELECT format FROM link_pronunciation \
>   \WHERE link_no = {linkNumber link}")

TODO: Would this be safe from race conditions with a transaction?

> addPronunciation :: Integer -> App CGIResult
> addPronunciation linkNo = do
>   link <- getLink linkNo
>   case link of
>     Nothing -> outputNotFound
>     Just l  -> do
>       editable <- canEdit l
>       if not editable
>         then outputUnauthorized
>         else do
>           -- Make sure that a pronunciation doesn't already exist.
>           exists <- pronounceable l
>           if exists
>             then error "Pronunciation already exists."
>             else do
>               formFile <- getInputFilename "qqfile"
>               (filename, content) <- case formFile of
>                                        -- Old browsers will send as a file input.
>                                        Just f   -> do
>                                          content' <- fromJust <$> getInputFPS "qqfile"
>                                          return (f, content')
>                                        -- New browsers will send as the POST body.
>                                        Nothing  -> do
>                                          f <- getRequiredInput "qqfile"
>                                          content' <- getBodyFPS
>                                          return (f, content')
>               let format = map toLower $ safeTail $ takeExtension filename
>               if format `notElem` allowedExtensions
>                 then error $ "Unsupported file format: " ++ format
>                 else do
>                   dir <- (</> "upload" </> "audio" </> "pronunciation") <$> asks appDir
>                   let filepath = dir </> (show linkNo) <.> format
>                   liftIO $ writeFile filepath content
>                   liftIO $ prepAudio format filepath
>                   $(execute' "INSERT INTO link_pronunciation (link_no, format) \
>                                                      \VALUES ({linkNo}, {format})")
>                   outputJSON [("success", True)]
>  where allowedExtensions = ["flac", "wav", "ogg", "mp3"]

> deletePronunciation :: Integer -> App CGIResult
> deletePronunciation linkNo = do
>   $(execute' "DELETE FROM link_pronunciation WHERE link_no = {linkNo}")
>   dir <- (</> "upload" </> "audio" </> "pronunciation") <$> asks appDir
>   liftIO $ unsafeSystem $ "rm " ++ dir ++ "/" ++ show linkNo ++ ".*"
>   outputNothing

TODO: Move this to another module.

> prepAudio :: String -> FilePath -> IO ()
> prepAudio "flac" f = do
>   unsafeSystem $ "flac -d " ++ f ++ " &> /dev/null"
>   prepAudio "wav" $ replaceExtension f ".wav"
>   unsafeSystem $ "rm " ++ replaceExtension f ".wav"
> prepAudio "wav"  f = do
>   unsafeSystem $ "oggenc " ++ f ++ " &> /dev/null"
>   unsafeSystem $ "lame " ++ f ++ " " ++ (replaceExtension f ".mp3") ++ " &> /dev/null"
> prepAudio "ogg"  f = do
>   unsafeSystem $ "oggdec " ++ f ++ " &> /dev/null"
>   unsafeSystem $ "lame " ++ replaceExtension f ".wav" ++ " " ++ replaceExtension f ".mp3" ++ " &> /dev/null"
>   unsafeSystem $ "rm " ++ replaceExtension f ".wav"
> prepAudio "mp3"  f = do
>   unsafeSystem $ "lame --decode " ++ f ++ " " ++ replaceExtension f ".wav" ++ " &> /dev/null"
>   unsafeSystem $ "oggenc " ++ replaceExtension f ".wav" ++ " &> /dev/null"
>   unsafeSystem $ "rm " ++ replaceExtension f ".wav"
> prepAudio _      _ = error "Unsupported file format"

It's nice to have little help buttons and such where necessary. Making them
easier to create means that we're more likely to do so, which leads to a more
helpful user interface.

Currently this uses an icon from the
\href{http://www.famfamfam.com/lab/icons/mini/}{FamFamFam ``Mini''} set.

> helpButton :: String -> Maybe String -> Html
> helpButton url label' = a ! href (stringValue url) ! class_ "help-button" $ do
>   img ! src "http://s.vocabulink.com/img/icon/info.png"
>   maybe mempty (string . (' ' :)) label'

Generate a cloud of words from links in the database.

> data WordStyle = WordStyle (Float, Float) (Float, Float) Int Int
>   deriving (Show, Eq)

> wordCloud :: Int -> Int -> Int -> Int -> Int -> Int -> App Html
> wordCloud n width' height' fontMin fontMax numClasses = do
>   words <- $(queryTuples'
>     "SELECT origin, link_no FROM link \
>     \WHERE NOT deleted \
>     \ORDER BY random() LIMIT {n}")
>   gen <- liftIO getStdGen
>   let (styles, (newGen, _)) = runState (mapM (wordStyle . fst) words) (gen, [])
>   liftIO $ setStdGen newGen
>   return $ mconcat $ catMaybes $ zipWith (\ w s -> liftM (wordTag w) s) words styles
>  where wordTag :: (String, Integer) -> WordStyle -> Html
>        wordTag (word, linkNo) (WordStyle (x, y) _ classNum fontSize) =
>          let style' = "font-size: " ++ (show fontSize) ++ "px; "
>                    ++ "left: " ++ (show x) ++ "%; " ++ "top: " ++ (show y) ++ "%;" in
>          a ! href (stringValue $ "/link/" ++ show linkNo)
>            ! class_ (stringValue $ "class-" ++ show classNum)
>            ! style (stringValue style')
>            $ string word
>        wordStyle :: String -> State (StdGen, [WordStyle]) (Maybe WordStyle)
>        wordStyle word = do
>          let fontRange = fontMax - fontMin
>          fontSize <- (\ s -> fontMax - round (logBase 1.15 ((s * (1.15 ^ fontRange)::Float) + 1))) <$> getRandomR 0.0 1.0
>          -- fontSize <- getRandomR fontMin fontMax
>          let widthP  = (100.0 / (fromIntegral width')::Float)  * genericLength word * fromIntegral fontSize
>              heightP = (100.0 / (fromIntegral height')::Float) * fromIntegral fontSize
>          x        <- getRandomR 0 (max (100 - widthP) 1)
>          y        <- getRandomR 0 (max (100 - heightP) 1)
>          class'   <- getRandomR 1 numClasses
>          (gen, prev) <- get
>          let spiral' = spiral 30.0 (x, y)
>              styles  = filter inBounds $ map (\ pos -> WordStyle pos (widthP, heightP) class' fontSize) spiral'
>              style'  = find (\ s -> not $ any (flip overlap $ s) prev) styles
>          case style' of
>            Nothing -> return Nothing
>            Just style'' -> do
>              put (gen, style'':prev)
>              return $ Just style''
>        getRandomR :: Random a => a -> a -> State (StdGen, [WordStyle]) a
>        getRandomR min' max' = do
>          (gen, styles) <- get
>          let (n', newGen) = randomR (min', max') gen
>          put (newGen, styles)
>          return n'
>        inBounds :: WordStyle -> Bool
>        inBounds (WordStyle (x, y) (w, h) _ _) = x >= 0 && y >= 0 && x + w <= 100 && y + h <= 100
>        overlap :: WordStyle -> WordStyle -> Bool
>        -- We can't really be certain of when a word is overlapping,
>        -- since the words will be rendered by the user's browser.
>        -- However, we can make a guess.
>        overlap (WordStyle (x1, y1) (w1', h1') _ _) (WordStyle (x2, y2) (w2', h2') _ _) =
>          let hInter = (x2 > x1 && x2 < x1 + w1') || (x2 + w2' > x1 && x2 + w2' < x1 + w1') || (x2 < x1 && x2 + w2' > x1 + w1')
>              vInter = (y2 > y1 && y2 < y1 + h1') || (y2 + h2' > y1 && y2 + h2' < y1 + h1') || (y2 < y1 && y2 + h2' > y1 + h1') in
>          hInter && vInter
>        spiral :: Float -> (Float, Float) -> [(Float, Float)]
>        spiral maxTheta = spiral' 0.0
>         where spiral' theta (x, y) =
>                 if theta > maxTheta
>                   then []
>                   else let r  = theta * 3
>                            x' = (r * cos theta) + x
>                            y' = (r * sin theta) + y in
>                        (x', y'):(spiral' (theta + 0.1) (x, y))
