-- Copyright 2008, 2009, 2010, 2011 Chris Forno

-- This file is part of Vocabulink.

-- Vocabulink is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- Vocabulink is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

-- Links are the center of interest in our program. Most activities revolve
-- around them.

module Vocabulink.Link ( Link(..), PartialLink(..), LinkType(..), isLinkword
                       , linkOriginLanguage, linkDestinationLanguage, languageNameFromAbbreviation
                       , activeLinkTypes, linkTypeNameFromType, linkWord
                       , getLink, getPartialLink
                       , linkLanguages, adjacentLinkNumbers
                       , createLink, deleteLink
                       , latestLinks, memberLinks, languagePairLinks, linksContaining
                       ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Link.Pronunciation
import Vocabulink.Member
import Vocabulink.Utils

-- Link Data Types

-- Abstractly, a link is defined by the origin and destination lexemes it links,
-- as well as its type. Practically, we also need to carry around information such
-- as its link number (in the database) as well as a string representation of its
-- type (for partially constructed links, which you'll see later).

data Link = Link { linkNumber          :: Integer
                 , linkTypeName        :: String
                 , linkAuthor          :: Integer
                 , linkOrigin          :: String
                 , linkOriginLang      :: String
                 , linkDestination     :: String
                 , linkDestinationLang :: String
                 , linkType            :: LinkType
                 }

-- We can associate 2 lexemes in many different ways. Because different linking
-- methods require different information, they each need different
-- representations in the database. This leads to some additional complexity.

-- Each link between lexemes has a type. This type determines how the link is
-- displayed, edited, used in statistical analysis, etc. See the Vocabulink
-- handbook for a more in-depth description of the types.

data LinkType = Association | SoundAlike | LinkWord String
                deriving (Show)

isLinkword :: Link -> Bool
isLinkword l = case linkType l of
                 (LinkWord _) -> True
                 _            -> False

linkWord :: Link -> Maybe String
linkWord l = case linkType l of
               (LinkWord w) -> Just w
               _            -> Nothing

-- We'll eventually want to support private/unpublished links.

instance UserContent Link where
  canView link    = do
    member <- asks appMember
    deleted <- fromJust <$> $(queryTuple'
                 "SELECT deleted FROM link \
                 \WHERE link_no = {linkNumber link}")
    return $ if not deleted
      then True
      else case member of
        Nothing -> False
        Just m  -> (memberNumber m) == linkAuthor link || (memberNumber m) == 1 || (memberNumber m) == 2
  canEdit link    = do
    member <- asks appMember
    return $ case member of
      Just m  -> (memberNumber m) == linkAuthor link || (memberNumber m) == 1 || (memberNumber m) == 2
      Nothing -> False
  canDelete link  = do
    edit <- canEdit link
    if edit
      then do
        deleted <- $(queryTuple'
                     "SELECT deleted FROM link \
                     \WHERE link_no = {linkNumber link}")
        return $ case deleted of
          Just d  -> not d
          Nothing -> False
      else return False

-- The |linkOriginLang| and |linkDestinationLang| are the language
-- abbrevations. To get the full name we need to look it up.

languageNameFromAbbreviation :: String -> App (Maybe String)
languageNameFromAbbreviation abbr = lookup abbr <$> asks appLanguages

linkOriginLanguage, linkDestinationLanguage :: Link -> App String
linkOriginLanguage      l = fromJust <$> languageNameFromAbbreviation (linkOriginLang l)
linkDestinationLanguage l = fromJust <$> languageNameFromAbbreviation (linkDestinationLang l)

-- We already know what types of links exist, but we want only the active link
-- types sorted by usefulness.

activeLinkTypes :: [String]
activeLinkTypes = ["linkword", "sound-alike", "association"]

-- Sometimes we need to work with a human-readable name, such as when
-- interacting with a client or the database.

-- I used to call cognates "cognates" on the site, but it's a confusing term
-- for most people. Now, I call it a "sound-alike".

linkTypeNameFromType :: LinkType -> String
linkTypeNameFromType Association  = "association"
linkTypeNameFromType SoundAlike   = "sound-alike"
linkTypeNameFromType (LinkWord _) = "linkword"

-- Fully loading a link from the database requires joining 2 relations. The
-- join depends on the type of the link. But we don't always need the
-- type-specific data associated with a link. Sometimes it's not even possible
-- to have it, such as during interactive link construction.

-- We'll use a separate type to represent this. Essentially it's a link with an
-- undefined linkType. We use a separate type to avoid passing a partial link
-- to a function that expects a fully-instantiated link. The only danger here
-- is writing a function that accepts a partial link and then trys to access
-- the linkType information.

newtype PartialLink = PartialLink { pLink :: Link }

-- Storing Links

-- We refer to storing a link as ``establishing'' the link.

-- Each link type is expected to be potentially different enough to require its
-- own database schema for representation. We could attempt to use PostgreSQL's
-- inheritance features, but I've decided to handle the difference between
-- types at the Haskell layer for now. I'm actually hesitant to use separate
-- tables for separate types as it feels like I'm breaking the relational
-- model. However, any extra efficiency for study outranks implementation
-- elegance (correctness?).

-- Establishing a link requires a member number since all links must be owned
-- by a member.

-- Since we need to store the link in 2 different tables, we use a transaction.
-- Our App-level database functions are not yet great with transactions, so
-- we'll have to handle the transaction manually here. You'll also notice that
-- some link types (such as sound-alikes) have no additional information and
-- hence no relation in the database.

-- This returns the newly established link number.

establishLink :: Link -> Integer -> App (Integer)
establishLink l memberNo = do
  h <- asks appDB
  liftIO $ withTransaction h $ do
    exists <- linkExists h l
    case exists of
      True -> error "Link already exists."
      False -> do
        linkNo <- fromJust <$> $(queryTuple
          "INSERT INTO link (origin, destination, \
                            \origin_language, destination_language, \
                            \link_type, author) \
                    \VALUES ({linkOrigin l}, {linkDestination l}, \
                            \{linkOriginLang l}, {linkDestinationLang l}, \
                            \{linkTypeName l}, {memberNo}) \
          \RETURNING link_no") h
        establishLinkType h (l {linkNumber = linkNo})
        return linkNo

-- The relation we insert additional details into depends on the type of the
-- link and it's easiest to use a separate function for it.

establishLinkType :: Handle -> Link -> IO ()
establishLinkType h l = case linkType l of
  (LinkWord word) -> do
    $(execute "INSERT INTO link_linkword \
                     \(link_no, linkword) \
              \VALUES ({linkNumber l}, {word})") h
  _ -> return ()

-- | Check to see if a link already exists in the database. This is here to
-- ensure uniqueness, as storing links of multiple types in the same table
-- means that we can't use PostgreSQL constaints.
linkExists :: Handle -> Link -> IO Bool
linkExists h l = case linkType l of
  -- Associations cannot be created if any better link type exists for the words.
  Association     -> isJust <$> $(queryTuple
    "SELECT link_no FROM link \
    \WHERE origin = {linkOrigin l} AND destination = {linkDestination l} \
      \AND origin_language = {linkOriginLang l} AND destination_language = {linkDestinationLang l}") h
  -- Sound-alikes cannot be created if a sound-alike already exists for the words.
  SoundAlike      -> isJust <$> $(queryTuple
    "SELECT link_no FROM link \
    \WHERE origin = {linkOrigin l} AND destination = {linkDestination l} \
      \AND origin_language = {linkOriginLang l} AND destination_language = {linkDestinationLang l} \
      \AND link_type = {linkTypeNameFromType SoundAlike}") h
  -- Linkwords can duplicate everything, as long as they are different linkwords.
  (LinkWord word) -> isJust <$> $(queryTuple
    "SELECT link_no FROM link INNER JOIN link_linkword USING (link_no) \
    \WHERE origin = {linkOrigin l} AND destination = {linkDestination l} \
      \AND origin_language = {linkOriginLang l} AND destination_language = {linkDestinationLang l} \
      \AND linkword = {word}") h

-- Retrieving Links

-- Now that we've seen how we store links, let's look at retrieving them (which is
-- slightly more complicated in order to allow for efficient retrieval of multiple
-- links).

-- Retrieving a partial link is simple.

getPartialLink :: Integer -> App (Maybe PartialLink)
getPartialLink linkNo = partialLinkFromTuple <$$> $(queryTuple'
  "SELECT link_no, link_type, author, \
         \origin, destination, \
         \origin_language, destination_language \
  \FROM link \
  \WHERE link_no = {linkNo}")

-- We use a helper function to convert the raw SQL tuple to a partial link
-- value. Note that we leave the link's |linkType| undefined.

partialLinkFromTuple :: (Integer, String, Integer, String, String, String, String) -> PartialLink
partialLinkFromTuple (n, t, u, o, d, ol, dl) =
  PartialLink Link { linkNumber          = n
                   , linkTypeName        = t
                   , linkAuthor          = u
                   , linkOrigin          = o
                   , linkDestination     = d
                   , linkOriginLang      = ol
                   , linkDestinationLang = dl
                   , linkType            = undefined }

-- Once we have a partial link, it's a simple matter to turn it into a full
-- link. We just need to retrieve its type-level details from the database.

getLinkFromPartial :: PartialLink -> App (Maybe Link)
getLinkFromPartial (PartialLink partial) = do
  linkT <- getLinkType (PartialLink partial)
  return $ (\t -> Just $ partial {linkType = t}) =<< linkT

getLinkType :: PartialLink -> App (Maybe LinkType)
getLinkType (PartialLink pl) = case pl of
  (Link { linkTypeName  = "association" }) -> return $ Just Association
  (Link { linkTypeName  = "sound-alike"})  -> return $ Just SoundAlike
  (Link { linkTypeName  = "linkword"
        , linkNumber    = n })             -> do
    LinkWord <$$> $(queryTuple'
      "SELECT linkword FROM link_linkword \
      \WHERE link_no = {n}")
  _                                        -> error "Bad partial link."

-- We now have everything we need to retrieve a full link in 1 step.

getLink :: Integer -> App (Maybe Link)
getLink linkNo = maybe (return Nothing) getLinkFromPartial =<< getPartialLink linkNo

-- Once we have a significant number of links, browsing through latest becomes
-- unreasonable for finding links for just the language we're interested in. To
-- aid in this, it helps to know which language pairs are in use and to know
-- how many links exist for each so that we can arrange by popularity.

-- Since we need both the language abbreviation and name (we use the
-- abbreviation in URLs and the name for display to the client), we return
-- these as triples: (language, language, count).

linkLanguages :: App [((String, String), (String, String), Integer)]
linkLanguages =
  map linkLanguages' <$> $(queryTuples'
    "SELECT origin_language, orig.name, \
           \destination_language, dest.name, \
           \COUNT(*) \
    \FROM link \
    \INNER JOIN language orig ON (orig.abbr = origin_language) \
    \INNER JOIN language dest ON (dest.abbr = destination_language) \
    \WHERE NOT deleted \
    \GROUP BY origin_language, orig.name, \
             \destination_language, dest.name")
 where linkLanguages' (oa, on, da, dn, c) = ((oa, on), (da, dn), fromJust c)

-- An adjacent link is the nearest (by link number) in the given language pair.

adjacentLinkNumbers :: Link -> App (Maybe Integer, Maybe Integer)
adjacentLinkNumbers link = do
  let oLang = linkOriginLang link
      dLang = linkDestinationLang link
  prevLink <- $(queryTuple'
    "(SELECT link_no FROM link \
     \WHERE origin_language = {oLang} \
       \AND destination_language = {dLang} \
       \AND link_no < {linkNumber link} \
       \AND NOT deleted \
     \ORDER BY link_no DESC LIMIT 1) \
    \UNION \
    \(SELECT link_no FROM link \
     \WHERE origin_language = {oLang} \
     \AND destination_language = {dLang} \
     \AND NOT link_no = {linkNumber link} \
     \AND NOT deleted \
     \ORDER BY link_no DESC LIMIT 1) \
     \ORDER BY link_no ASC")
  nextLink <- $(queryTuple'
    "(SELECT link_no FROM link \
     \WHERE origin_language = {oLang} \
       \AND destination_language = {dLang} \
       \AND link_no > {linkNumber link} \
       \AND NOT deleted \
     \ORDER BY link_no ASC LIMIT 1) \
    \UNION \
    \(SELECT link_no FROM link \
     \WHERE origin_language = {oLang} \
       \AND destination_language = {dLang} \
       \AND NOT link_no = {linkNumber link} \
       \AND NOT deleted \
     \ORDER BY link_no ASC LIMIT 1) \
    \ORDER BY link_no DESC")
  return (fromJust <$> prevLink, fromJust <$> nextLink)

-- Creating New Links

createLink :: App CGIResult
createLink = withRequiredMember $ \m -> do
  foreign'     <- getRequiredInput "foreign"
  foreignLang  <- getRequiredInput "foreign-lang"
  familiar     <- getRequiredInput "familiar"
  familiarLang <- getRequiredInput "familiar-lang"
  linkType''   <- getRequiredInput "link-type"
  linkType' <- case linkType'' of
                 "linkword"    -> do
                   linkword <- getRequiredInput "linkword"
                   return $ LinkWord linkword
                 "sound-alike" -> return SoundAlike
                 "association" -> return Association
                 _             -> error "Invalid link type"
  ogg <- getInput "ogg"
  mp3 <- getInput "mp3"
  let link = mkLink foreign' foreignLang familiar familiarLang linkType'
  n <- establishLink link (memberNumber m)
  case (ogg, mp3) of
    (Just ogg', Just mp3') -> do
      _ <- addPronunciation n ogg' "ogg"
      _ <- addPronunciation n mp3' "mp3"
      return ()
    _                      -> return ()
  redirect $ "/link/" ++ show n

-- When creating a link from a form, the link number must be undefined until
-- the link is established in the database.

mkLink :: String -> String -> String -> String -> LinkType -> Link
mkLink o ol d dl t = Link { linkNumber          = undefined
                          , linkAuthor          = undefined
                          , linkTypeName        = linkTypeNameFromType t
                          , linkOrigin          = o
                          , linkOriginLang      = ol
                          , linkDestination     = d
                          , linkDestinationLang = dl
                          , linkType            = t }

-- Deleting Links

-- Links can be deleted by their owner. They're not actually removed from the
-- database, as doing so would require removing the link from other members'
-- review sets. Instead, we just flag the link as deleted so that it doesn't
-- appear in most contexts.

deleteLink :: Integer -> App CGIResult
deleteLink linkNo = do
  $(execute' "UPDATE link SET deleted = TRUE \
             \WHERE link_no = {linkNo}")
  outputJSON [(""::String, ""::String)]

-- We want to be able to display links in various ways. It would be really nice
-- to get lazy lists from the database. For now, you need to specify how many
-- results you want, as well as an offset.

-- Here we retrieve multiple links at once. This was the original motivation
-- for dividing link types into full and partial. Often we need to retrieve
-- links for simple display but we don't need or want extra trips to the
-- database. Here we need only 1 query instead of potentially @limit@ queries.

-- We don't want to display deleted links (which are left in the database for
-- people still reviewing them). There is some duplication of SQL here, but I
-- have yet found a nice way to generalize these functions.

-- The first way to retrieve links is to just grab all of them, starting at the
-- most recent. This assumes the ordering of links is determined by link
-- number.

latestLinks :: Int -> Int -> App [PartialLink]
latestLinks offset limit =
  map partialLinkFromTuple <$> $(queryTuples'
    "SELECT link_no, link_type, author, \
           \origin, destination, \
           \origin_language, destination_language \
    \FROM link \
    \WHERE NOT deleted \
    \ORDER BY link_no DESC \
    \OFFSET {offset} LIMIT {limit}")

-- Another way we retrieve links is by author (member). These just happen to be
-- sorted by link number as well.

memberLinks :: Integer -> Int -> Int -> App [PartialLink]
memberLinks memberNo offset limit =
  map partialLinkFromTuple <$> $(queryTuples'
    "SELECT link_no, link_type, author, \
           \origin, destination, \
           \origin_language, destination_language \
    \FROM link \
    \WHERE NOT deleted AND author = {memberNo} \
    \ORDER BY link_no DESC \
    \OFFSET {offset} LIMIT {limit}")

languagePairLinks :: String -> String -> Int -> Int -> App [PartialLink]
languagePairLinks ol dl offset limit = do
  map partialLinkFromTuple <$> $(queryTuples'
    "SELECT link_no, link_type, author, \
           \origin, destination, \
           \origin_language, destination_language \
    \FROM link \
    \WHERE NOT deleted \
      \AND origin_language = {ol} AND destination_language = {dl} \
    \ORDER BY link_no DESC \
    \OFFSET {offset} LIMIT {limit}")

-- This is a close parallel to the original search for Vocabulink. We use it so
-- that we can display a link to a specialized link search only when results are
-- available.

linksContaining :: String -> App [PartialLink]
linksContaining q =
  map partialLinkFromTuple <$> $(queryTuples'
    "SELECT link_no, link_type, author, \
           \origin, destination, \
           \origin_language, destination_language \
    \FROM link \
    \WHERE NOT deleted \
      \AND (origin ILIKE {q} OR destination ILIKE {q}) \
    \LIMIT 20")
