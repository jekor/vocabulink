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

-- | The App Monad

-- When I wrote the first version of Vocabulink, many functions passed around a
-- database connection. I now understand monads a little bit more, and it's
-- easier to store some information within an ``App'' monad. This reduces our
-- function signatures a little bit.

-- The App monad is now also used for passing around member information and a
-- few other conveniences.

module Vocabulink.App ( App, AppEnv(..), AppT, runApp
                      , Dependency(..), dependencyVersion
                      , queryTuple', queryTuples', execute'
                      , getOption
                      {- Control.Monad.Reader -}
                      , asks
                      ) where

import Vocabulink.CGI
import Vocabulink.Member.Auth
import Vocabulink.Utils

import Control.Applicative (Applicative)
import Control.Monad (ap)
import Control.Monad.Error (runErrorT)
import Control.Monad.Reader (ReaderT(..), MonadReader, asks)
import Control.Monad.Trans (lift)
import Data.ConfigFile (ConfigParser, get)
import Language.Haskell.TH (Q, Exp)
import Network.CGI.Monad (MonadCGI(..))
import Network.CGI (CGI, CGIT)

data AppEnv = AppEnv { appDB         :: Handle
                     , appCP         :: ConfigParser
                     , appDir        :: FilePath
                     , appStaticDeps :: [(Dependency, EpochTime)]
                     , appLanguages  :: [(String, String)]
                     , appMember     :: Maybe Member
                     , appForvoKey   :: String
                     }

-- The App monad is a combination of the CGI and Reader monads.

newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
  deriving (Monad, MonadIO, MonadReader AppEnv)

-- ...whose CGI monad uses the IO monad.

type App = AppT IO

-- We need to make the App monad an Applicative Functor so that it will work
-- with formlets.
-- TODO: Do we need this anymore?

instance Applicative App where
  pure = return
  (<*>) = ap

instance Functor App where
  fmap = liftM

-- To make the App monad an instance of MonadCGI, we need to define basic CGI
-- functions. CGI is relatively simple and its functionality can be defined on
-- top of just an environment getter and a function for adding headers. We
-- reuse the existing methods.

instance MonadCGI App where
  cgiAddHeader n = AppT . lift . cgiAddHeader n
  cgiGet = AppT . lift . cgiGet

-- |runApp| does the job of creating the Reader environment and returning the
-- CGIResult from within the App monad to the CGI monad. The environment
-- includes a database handle, a configuration file, and some member
-- information (if the request came from a logged in member).

-- We can't use the convenience of |getOption| here as we're not in the App monad
-- yet.

runApp :: Handle -- ^ this thread's database connection
       -> ConfigParser -- ^ configuration file
       -> [(Dependency, EpochTime)] -- ^ a list of external dependencies with last modified timestamps
       -> [(String, String)] -- ^ a list of language (abbrevations, names)
       -> App CGIResult -- ^ this thread's action
       -> CGI CGIResult -- ^ the resulting CGI action
runApp h cp sd ls (AppT a) = do
  let dir   = forceEither $ get cp "DEFAULT" "maindir"
      key   = forceEither $ get cp "DEFAULT" "authtokenkey"
      forvo = forceEither $ get cp "DEFAULT" "forvokey"
  token <- verifiedAuthToken key
  member <- case token of
              Nothing -> return Nothing
              Just t  -> do email <- liftIO $ join <$> $(queryTuple
                                       "SELECT email FROM member \
                                       \WHERE member_no = {authMemberNo t}") h
                            return $ Just
                              Member { memberNumber = authMemberNo t
                                     , memberName   = authUsername t
                                     , memberEmail  = email
                                     }
  runReaderT a AppEnv { appDB          = h
                      , appCP          = cp
                      , appDir         = dir
                      , appStaticDeps  = sd
                      , appLanguages   = ls
                      , appMember      = member
                      , appForvoKey    = forvo
                      }

-- Here are some functions that abstract away even having to ask for values
-- from the Reader environment in the App monad.

-- Most pages depend on some external CSS and/or JavaScript files.

-- We want to allow the client browser to cache CSS and JavaScript for as long
-- as possible, but we want to bust the cache when we update them. We can get
-- the best of both worlds by setting large expiration times and by using
-- version numbers.

-- To do this, we'll add a version number to each static file as a query
-- string. The web server will ignore this and serve the same file, but the
-- client browser should see it as a new file.

data Dependency = CSS String | JS String
                  deriving (Eq, Show)

dependencyVersion :: Dependency -> App (Maybe String)
dependencyVersion d = (fmap show . lookup d) <$> asks appStaticDeps

-- Here are some convenience functions for working with the database in the App
-- monad.

withConnection :: (Handle -> IO a) -> App a
withConnection action = do h <- asks appDB
                           liftIO $ action h

queryTuple' :: String -> Q Exp
queryTuple' sql = [| withConnection $(queryTuple sql) |]

queryTuples' :: String -> Q Exp
queryTuples' sql = [| withConnection $(queryTuples sql) |]

execute' :: String -> Q Exp
execute' sql = [| withConnection $(execute sql) |]

-- Return a configuration option or log an error.

-- This always pulls from the @DEFAULT@ section. It also only supports strings.

getOption :: String -> App (Maybe String)
getOption option = do
  cp <- asks appCP
  opt <- runErrorT $ get cp "DEFAULT" option
  case opt of
    Left e  -> liftIO $ logError "config" (show e) >> return Nothing
    Right o -> return $ Just o
