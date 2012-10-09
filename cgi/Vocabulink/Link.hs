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

module Vocabulink.Link ( Link(..), linkDetails, linkFromTuple
                       , languagePairLinks, linksContaining
                       , linkLanguages, linkTypeName
                       ) where

import Vocabulink.Utils

data Link = Link { link_no        :: Integer
                 , learn          :: String
                 , known          :: String
                 , learn_lang     :: String
                 , known_lang     :: String
                 , learn_language :: String
                 , known_language :: String
                 , soundalike     :: Bool
                 , linkword       :: Maybe String
                 }

linkDetails :: Integer -> Handle -> IO (Maybe Link)
linkDetails linkNo db = do
  row <- $(queryTuple
    "SELECT l.link_no, learn, known, \
           \learn_lang, known_lang, ll.name, kl.name, \
           \s.link_no IS NOT NULL, COALESCE(linkword) \
    \FROM link l \
    \INNER JOIN language ll ON (ll.abbr = learn_lang) \
    \INNER JOIN language kl ON (kl.abbr = known_lang) \
    \LEFT JOIN link_soundalike s USING (link_no) \
    \LEFT JOIN link_linkword w USING (link_no) \
    \WHERE link_no = {linkNo} AND NOT deleted") db
  case row of
    Nothing -> return Nothing
    Just t -> return $ Just $ linkFromTuple t

-- This is hacky.
linkFromTuple :: (Integer, String, String, String, String, String, String, Maybe Bool, Maybe String) -> Link
linkFromTuple (ln, l, k, ll, kl, lll, kll, s, w) = Link ln l k ll kl lll kll (fromJust s) w

-- We want to be able to display links in various ways. It would be really nice
-- to get lazy lists from the database. For now, you need to specify how many
-- results you want, as well as an offset.

languagePairLinks :: Maybe Integer -> String -> String -> Handle -> IO [Link]
languagePairLinks memberNumber learnLang knownLang db = do
  rows <- $(queryTuples
    "SELECT l.link_no, learn, known, \
           \learn_lang, known_lang, ll.name, kl.name, \
           \s.link_no IS NOT NULL, COALESCE(linkword) \
    \FROM link l \
    \INNER JOIN language ll ON (ll.abbr = learn_lang) \
    \INNER JOIN language kl ON (kl.abbr = known_lang) \
    \LEFT JOIN link_soundalike s USING (link_no) \
    \LEFT JOIN link_linkword w USING (link_no) \
    \LEFT JOIN link_frequency f USING (link_no) \
    \WHERE learn_lang = {learnLang} AND known_lang = {knownLang} \
      \AND NOT deleted \
    \GROUP BY l.link_no, learn, known, learn_lang, known_lang, ll.name, kl.name, s.link_no, linkword \
    \ORDER BY MIN(rank) ASC") db
  return $ map linkFromTuple rows

linksContaining :: String -> Handle -> IO [Link]
linksContaining q db = do
  let fuzzy = "%" ++ q ++ "%"
  exacts <- map linkFromTuple <$> $(queryTuples
    "SELECT l.link_no, learn, known, \
           \learn_lang, known_lang, ll.name, kl.name, \
           \s.link_no IS NOT NULL, COALESCE(linkword) \
    \FROM link l \
    \INNER JOIN language ll ON (ll.abbr = learn_lang) \
    \INNER JOIN language kl ON (kl.abbr = known_lang) \
    \LEFT JOIN link_soundalike s USING (link_no) \
    \LEFT JOIN link_linkword w USING (link_no) \
    \WHERE NOT deleted \
      \AND (learn ILIKE {q} OR known ILIKE {q} OR linkword ILIKE {q} \
        \OR unaccent(learn) ILIKE {q} OR unaccent(known) ILIKE {q} OR unaccent(linkword) ILIKE {q})") db
  closes <- map linkFromTuple <$> $(queryTuples
    "SELECT l.link_no, learn, known, \
           \learn_lang, known_lang, ll.name, kl.name, \
           \s.link_no IS NOT NULL, COALESCE(linkword) \
    \FROM link l \
    \INNER JOIN language ll ON (ll.abbr = learn_lang) \
    \INNER JOIN language kl ON (kl.abbr = known_lang) \
    \LEFT JOIN link_soundalike s USING (link_no) \
    \LEFT JOIN link_linkword w USING (link_no) \
    \WHERE NOT deleted \
      \AND (learn NOT ILIKE {q} AND known NOT ILIKE {q} AND linkword NOT ILIKE {q} \
        \AND unaccent(learn) NOT ILIKE {q} AND unaccent(known) NOT ILIKE {q} AND unaccent(linkword) NOT ILIKE {q}) \
      \AND (learn ILIKE {fuzzy} OR known ILIKE {fuzzy} OR linkword ILIKE {fuzzy} \
        \OR unaccent(learn) ILIKE {fuzzy} OR unaccent(known) ILIKE {fuzzy} OR unaccent(linkword) ILIKE {fuzzy}) \
    \GROUP BY l.link_no, learn, known, learn_lang, known_lang, ll.name, kl.name, s.link_no, linkword \
    \ORDER BY char_length(learn)") db
  return $ exacts ++ closes

-- Once we have a significant number of links, browsing through latest becomes
-- unreasonable for finding links for just the language we're interested in. To
-- aid in this, it helps to know which language pairs are in use and to know
-- how many links exist for each so that we can arrange by popularity.

-- Since we need both the language abbreviation and name (we use the
-- abbreviation in URLs and the name for display to the client), we return
-- these as triples: (language, language, count).

linkLanguages :: Handle -> IO [((String, String), (String, String), Integer)]
linkLanguages db =
  map linkLanguages' <$> $(queryTuples
    "SELECT learn_lang, lr.name, \
           \known_lang, kn.name, \
           \COUNT(*) \
    \FROM link \
    \INNER JOIN language lr ON (lr.abbr = learn_lang) \
    \INNER JOIN language kn ON (kn.abbr = known_lang) \
    \WHERE NOT deleted \
    \GROUP BY learn_lang, lr.name, known_lang, kn.name") db
 where linkLanguages' (oa, on, da, dn, c) = ((oa, on), (da, dn), fromJust c)

linkTypeName :: Link -> String
linkTypeName link
  | isJust (linkword link) = "linkword"
  | soundalike link        = "soundalike"
  | otherwise              = "association"
