-- Copyright 2008, 2009, 2010, 2011, 2012, 2013 Chris Forno

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

-- When I wrote the first version of Vocabulink, many functions passed around a
-- database connection. Next, I understood how to use monad transformers and
-- rolled in a Reader. But everything ended up in a monad, so now I'm taking a
-- simpler (if potentially unsafe) approach.

module Vocabulink.Env ( E
                      , compileYear, languages, languageName, staticManifest
                      , withVerifiedMember, withLoggedInMember
                      ) where

import Vocabulink.Member
import Vocabulink.Utils

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Database.TemplatePG.Protocol (executeSimpleQuery)
import Database.TemplatePG.SQL (thConnection)
import Language.Haskell.TH.Syntax (runIO, Exp(..), Lit(..))
import System.Environment (getEnv)

type E a = (?db::Handle, ?static::FilePath, ?tokenKey::String, ?member::Maybe Member, ?sendmail::FilePath) => a

compileYear :: Int
compileYear = $((LitE . IntegerL) `liftM` runIO currentYear)

languages :: [(String, String)]
languages = $(runIO (do h <- thConnection
                        res <- executeSimpleQuery "SELECT abbr, name FROM language" h
                        return $ ListE $ map (\[Just abbr, Just name] ->
                                                 TupE [ LitE $ StringL $ BLU.toString abbr
                                                      , LitE $ StringL $ BLU.toString name]) res))

languageName :: String -> String
languageName languageCode = fromMaybe "Unknown Language" (lookup languageCode languages)

staticManifest :: [(FilePath, String)]
staticManifest = $(runIO (do man <- manifest =<< getEnv "MANIFEST"
                             return $ ListE $ map (\ [checksum, path] ->
                                                    TupE [ LitE $ StringL path
                                                         , LitE $ StringL checksum ]) man))

-- | Only perform the given action if the user is authenticated and has
-- verified their email address. This provides a ``logged out default'' of
-- redirecting the client to the login page.
withVerifiedMember :: E ((Member -> a) -> a)
withVerifiedMember f =
  case ?member of
    Nothing -> error "Please log in."
    Just m  -> case memberEmail m of
                 Nothing -> error "Please verify your email address."
                 Just _  -> f m

withLoggedInMember :: E ((Member -> a) -> a)
withLoggedInMember f =
  case ?member of
    Nothing -> error "Please log in."
    Just m  -> f m
