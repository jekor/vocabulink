-- Copyright 2009, 2010, 2011 Chris Forno

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

module Vocabulink.Config (getConfig, staticDeps, languagesFromDB) where

import Vocabulink.App
import Vocabulink.Utils

import Control.Monad.Error (runErrorT)
import Data.ConfigFile (readfile, emptyCP, ConfigParser, get, CPError, options)
import Data.List (intersect, (\\))
import System.Directory (getDirectoryContents)
import System.Posix.Files (getFileStatus, modificationTime)

-- The path to the configuration file is the one bit of configuration that's
-- the same in all environments.

configFile :: String
configFile = "/etc/vocabulink.conf"

-- These config vars are required by the program in order to do anything
-- useful. They are guaranteed to exist later and can safely be read with
-- |forceEither $ get|.

requiredConfigVars :: [String]
requiredConfigVars = [ "dbpassword", "authtokenkey", "forvokey"
                     , "threads", "maindir", "supportaddress" ]

-- This retrieves the config file and makes sure that it contains all of the
-- required configuration variables. We check the variables now because we want
-- to find out about missing ones at program start time rather than in the logs
-- later.

getConfig :: IO (Either CPError ConfigParser)
getConfig = runErrorT $ do
  cp <- join $ liftIO $ readfile emptyCP configFile
  opts <- options cp "DEFAULT"
  if requiredConfigVars `intersect` opts == requiredConfigVars
     then return cp
     else do
       let missing = show $ requiredConfigVars \\ opts
       error $ "Missing configuration options: " ++ missing

-- Vocabulink makes use of a number of CSS and JavaScript files. We want the
-- client browser to cache these for as long as possible, but we need some
-- method of busting the cache. To do this, we'll append a timestamp to the end
-- of these files when serving their URL to the client. This will make them
-- look like a new file, but the webserver will strip the version number and
-- serve the file without the timestamp.

-- There are at least 3 ways to achieve this:

-- 1. Keep a list of files and versions up-to-date in the source code. This is
--    how Vocabulink originally did things, but it's error-prone.

-- 2. Check the timestamp of a file before adding it to the page. This would be
--    the safest method, but it requires hitting the file system multiple times
--    each time we serve a page.

-- 3. Check the timestamp of all JS/CSS files at program start and pack them
--    into the App's Reader monad. This has means we only need to access the
--    filesystem when the CGI program is restarted, each request can be
--    serviced without additional disk I/O. It also allows us to forget about
--    manually updating the source code. One disadvantage is that we must
--    remember to restart the program any time we've made JavaScript or CSS
--    changes. This is the approach we'll use.

-- We do not use this technique for versioning images. Images also have high
-- cache expiry rates set by the webserver, but images are also referenced in
-- CSS files which we don't have a easy way of updating. Instead, we currently
-- rename images in the rare event that they change.

-- With |staticDeps|, we'll check the static directories for the modification
-- times of all JS and CSS files. We don't need to walk any directories
-- recursively; JS and CSS are in 2 flat directories.

staticDeps :: ConfigParser -> IO [(Dependency, EpochTime)]
staticDeps cp = do
  let dir = forceEither $ get cp "DEFAULT" "maindir"
  jsDeps  <- map (first (JS  . takeBaseName)) `liftM` modificationTimes (dir </> "s" </> "js")  ".js"
  cssDeps <- map (first (CSS . takeBaseName)) `liftM` modificationTimes (dir </> "s" </> "css") ".css"
  return $ jsDeps ++ cssDeps

modificationTimes :: FilePath -> String -> IO [(FilePath, EpochTime)]
modificationTimes dir ext = do
  files <- filter ((== ext) . takeExtension) `liftM` getDirectoryContents dir
  modTimes <- mapM (liftM modificationTime . getFileStatus . (dir </>)) files
  return $ zip files modTimes

languagesFromDB :: Handle -> IO [(String, String)]
languagesFromDB = $(queryTuples "SELECT abbr, name FROM language ORDER BY name")

