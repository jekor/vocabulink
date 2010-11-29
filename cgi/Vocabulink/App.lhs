% Copyright 2008, 2009, 2010 Chris Forno

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

\section{The App Monad}
\label{App}

When I wrote the first version of Vocabulink, many functions passed around a
database connection. I now understand monads a little bit more, and it's easier
to store some information within an ``App'' monad. This reduces our function
signatures a little bit.

The App monad is now also used for passing around member information and a few
other conveniences.

> module Vocabulink.App (      App, AppEnv(..), AppT, runApp, getOption,
>                              Dependency(..), dependencyVersion,
>                              withRequiredMemberNumber, loggedInVerified,
>                              output404, reversibleRedirect,
>                              queryTuple', queryTuples', execute',
>  {- Network.CGI -}           outputNothing,
>  {- Control.Monad.Reader -}  asks,
>  {- Database.TemplatePG -}   withTransaction, execute, queryTuple, queryTuples ) where

> import Vocabulink.CGI

We have to import the authorization token code using GHC's @SOURCE@ directive
because of cyclic dependencies.

> import Vocabulink.Member.AuthToken
> import Vocabulink.Utils

> import Control.Applicative (Applicative)
> import Control.Monad (ap)
> import Control.Monad.Error (runErrorT)
> import Control.Monad.Reader (ReaderT(..), MonadReader, asks)
> import Control.Monad.Trans (lift)

> import Data.ConfigFile (ConfigParser, get)
> import Database.TemplatePG
> import Language.Haskell.TH (Q, Exp)
> import Network.CGI.Monad (MonadCGI(..))
> import Network.CGI (CGI, CGIT, outputNotFound, outputNothing)
> import Network.URI (escapeURIString, isUnescapedInURI)
> import System.IO (Handle)

> data AppEnv = AppEnv {  appDB          :: Handle,
>                         appCP          :: ConfigParser,
>                         appDir         :: FilePath,
>                         appStaticDeps  :: [(Dependency, EpochTime)],
>                         appLanguages   :: [(String, String)],
>                         appMemberNo    :: Maybe Integer,
>                         appMemberName  :: Maybe String,
>                         appMemberEmail :: Maybe String }

The App monad is a combination of the CGI and Reader monads.

> newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
>   deriving (Monad, MonadIO, MonadReader AppEnv)

...whose CGI monad uses the IO monad.

> type App = AppT IO

We need to make the App monad an Applicative Functor so that it will work with
formlets.

> instance Applicative App where
>   pure = return
>   (<*>) = ap

> instance Functor App where
>   fmap = liftM

To make the App monad an instance of MonadCGI, we need to define basic CGI
functions. CGI is relatively simple and its functionality can be defined on top
of just an environment getter and a function for adding headers. We reuse the
existing methods.

> instance MonadCGI App where
>   cgiAddHeader n = AppT . lift . cgiAddHeader n
>   cgiGet = AppT . lift . cgiGet

|runApp| does the job of creating the Reader environment and returning the
CGIResult from within the App monad to the CGI monad. The environment includes
a database handle, a configuration file, and some member information (if the
request came from a logged in member).

We can't use the convenience of |getOption| here as we're not in the App monad
yet.

> runApp :: Handle -> ConfigParser -> FilePath -> [(Dependency, EpochTime)] -> [(String, String)] -> App CGIResult -> CGI CGIResult
> runApp h cp dir sd ls (AppT a) = do
>   let key = forceEither $ get cp "DEFAULT" "authtokenkey"
>   token <- verifiedAuthToken key
>   email <- case token of
>              Nothing -> return Nothing
>              Just t  -> liftIO $ join <$> $(queryTuple "SELECT email FROM member \
>                                                        \WHERE member_no = {authMemberNo t}") h
>   runReaderT a AppEnv {  appDB          = h,
>                          appCP          = cp,
>                          appDir         = dir,
>                          appStaticDeps  = sd,
>                          appLanguages   = ls,
>                          appMemberNo    = authMemberNo <$> token,
>                          appMemberName  = authUsername <$> token,
>                          appMemberEmail = email }

\subsection{Convenience Functions}

Here are some functions that abstract away even having to ask for values from
the Reader environment in the App monad.

\subsubsection{Static File Dependencies}

Most pages depend on some external CSS and/or JavaScript files.

We want to allow the client browser to cache CSS and JavaScript for as long as
possible, but we want to bust the cache when we update them. We can get the
best of both worlds by setting large expiration times and by using version
numbers.

To do this, we'll add a version number to each static file as a query string.
The web server will ignore this and serve the same file, but the client browser
should see it as a new file.

> data Dependency = CSS String | JS String
>                   deriving (Eq, Show)

> dependencyVersion :: Dependency -> App (Maybe String)
> dependencyVersion d = (fmap show . lookup d) <$> asks appStaticDeps

\subsubsection{Identity}

|withRequiredMemberNumber| checks to see if the member has confirmed their
email address and provides a ``logged out default'' of redirecting the client
to the login page.

Use this any time a member number is generally required.

> withRequiredMemberNumber :: (Integer -> App CGIResult) -> App CGIResult
> withRequiredMemberNumber f = do
>   memberNo <- asks appMemberNo
>   email <- asks appMemberEmail
>   case (memberNo, email) of
>     (Just mn, Just _)  -> f mn
>     (Just _, Nothing)  -> redirect =<< reversibleRedirect "/member/confirmation"
>     _                  -> redirect =<< reversibleRedirect "/member/login"

This is a helper to quickly return a value based on the client's status. If the
client is not authenticated, return nothing. If they are authenticated but have
not verified their email address, return loggedIn. If they have verified their
email address, return verified.

> loggedInVerified :: a -> a -> a -> App a
> loggedInVerified verified loggedIn nothing = do
>   memberNo <- asks appMemberNo
>   memberEmail <- asks appMemberEmail
>   return $ case (memberNo, memberEmail) of
>     (Just _,  Just _)  -> verified
>     (Just _,  _)       -> loggedIn
>     (_     ,  _)       -> nothing

When we direct a user to some page, we might want to make sure that they can
find their way back to where they were. To do so, we get the current URI and
append it to the target page in the query string. The receiving page might know
what to do with it.

> reversibleRedirect :: String -> App String
> reversibleRedirect path = do
>   request <- fromMaybe "/" <$> getVar "REQUEST_URI"
>   return $ path ++ "?redirect=" ++ escapeURIString isUnescapedInURI request

I used to log all 404s, but the logs were overrun by favicon requests and the
like.

> output404 :: [String] -> App CGIResult
> output404 = outputNotFound . intercalate "/"

\subsubsection{Database}

Here are some convenience functions for working with the database in the App
monad.

> withConnection :: (Handle -> IO a) -> App a
> withConnection a = do h <- asks appDB
>                       liftIO $ a h

> queryTuple' :: String -> Q Exp
> queryTuple' sql = [| withConnection $(queryTuple sql) |]

> queryTuples' :: String -> Q Exp
> queryTuples' sql = [| withConnection $(queryTuples sql) |]

> execute' :: String -> Q Exp
> execute' sql = [| withConnection $(execute sql) |]

(ReaderT AppEnv (CGIT m) a)

\subsubsection{Exceptions}

|tryApp| is like |tryCGI|. It allows us to catch exceptions within the App
monad. To do so, we unwrap the Reader monad and use |tryCGI| (which unwraps
another Reader and Writer).

> tryApp :: App a -> App (Either SomeException a)
> tryApp (AppT c) = AppT (ReaderT (tryCGI' . runReaderT c))

\subsubsection{Configuration}

Return a configuration option or log an error.

This always pulls from the @DEFAULT@ section. It also only supports strings.

> getOption :: String -> App (Maybe String)
> getOption option = do
>   cp <- asks appCP
>   opt <- runErrorT $ get cp "DEFAULT" option
>   case opt of
>     Left e   -> liftIO $ logError "config" (show e) >> return Nothing
>     Right o  -> return $ Just o
