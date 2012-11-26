-- Copyright 2008, 2009, 2010, 2011, 2012 Chris Forno

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

module Vocabulink.Link ( Link(..), linkDetails, linkTypeName
                       , languagePairLinks, linksContaining
                       ) where

import Vocabulink.Utils

data Link = Link { link_no        :: Integer
                 , learn          :: String
                 , known          :: String
                 , learn_lang     :: String
                 , known_lang     :: String
                 , soundalike     :: Bool
                 , linkword       :: Maybe String
                 }
  deriving Eq

linkTypeName :: Link -> String
linkTypeName link
  | isJust (linkword link) = "linkword"
  | soundalike link        = "soundalike"
  | otherwise              = "association"

linkDetails :: Integer -> Handle -> IO (Maybe Link)
linkDetails linkNo db =
  uncurryN Link <$$> $(queryTuple
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE link_no = {linkNo} AND NOT deleted") db

-- We want to be able to display links in various ways. It would be really nice
-- to get lazy lists from the database. For now, you need to specify how many
-- results you want, as well as an offset.

languagePairLinks :: Maybe Integer -> String -> String -> Handle -> IO [Link]
languagePairLinks memberNumber learnLang knownLang db =
  map (uncurryN Link) <$> $(queryTuples
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE learn_lang = {learnLang} AND known_lang = {knownLang} \
      \AND NOT deleted") db

linksContaining :: String -> Handle -> IO [Link]
linksContaining q db = do
  let fuzzy = "%" ++ q ++ "%"
  exacts <- map (uncurryN Link) <$> $(queryTuples
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE NOT deleted \
      \AND (learn ILIKE {q} OR known ILIKE {q} OR linkword ILIKE {q} \
        \OR unaccent(learn) ILIKE {q} OR unaccent(known) ILIKE {q} OR unaccent(linkword) ILIKE {q})") db
  closes <- map (uncurryN Link) <$> $(queryTuples
    "SELECT link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
    \FROM link \
    \WHERE NOT deleted \
      \AND (learn ILIKE {fuzzy} OR known ILIKE {fuzzy} OR linkword ILIKE {fuzzy} \
        \OR unaccent(learn) ILIKE {fuzzy} OR unaccent(known) ILIKE {fuzzy} OR unaccent(linkword) ILIKE {fuzzy}) \
    \ORDER BY char_length(learn)") db
  return $ nub $ exacts ++ closes
