-- When I wrote the first version of Vocabulink, many functions passed around a
-- database connection. Next, I understood how to use monad transformers and
-- rolled in a Reader. But everything ended up in a monad, so now I'm taking a
-- simpler (if potentially unsafe) approach.

module Vocabulink.Env ( E
                      , compileYear, staticManifest
                      , withVerifiedMember, withLoggedInMember
                      ) where

import Vocabulink.Member
import Vocabulink.Utils

import Database.PostgreSQL.Typed (PGConnection)
import Language.Haskell.TH.Syntax (runIO, Exp(..), Lit(..))
import System.Environment (getEnv)

type E a = (?db::PGConnection, ?static::FilePath, ?tokenKey::String, ?member::Maybe Member, ?sendmail::FilePath) => a

compileYear :: Int
compileYear = $((LitE . IntegerL) `liftM` runIO currentYear)

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
