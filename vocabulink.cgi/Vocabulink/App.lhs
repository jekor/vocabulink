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

\section{The App Monad}
\label{App}

When I wrote the first version of Vocabulink, many functions passed around a
database connection. I now understand monads a little bit more, and it's easier
to store some information within an ``App'' monad. This reduces our function
signatures a little bit.

The App monad is now also used for passing around member information and a few
other conveniences.

> module Vocabulink.App (      App, AppEnv(..), AppT, runApp, logApp, getOption,
>                              withMemberNumber, withRequiredMemberNumber,
>                              output404, reversibleRedirect,
>                              queryTuple', queryValue', queryAttribute',
>                              queryTuples', quickInsertNo', runStmt', quickStmt',
>                              withTransaction', run',
>  {- Control.Monad.Reader -}  asks) where

> import Vocabulink.CGI
> import Vocabulink.DB

We have to import the authorization token code using GHC's @SOURCE@ directive
because of cyclic dependencies.

> import {-# SOURCE #-} Vocabulink.Member.AuthToken
> import Vocabulink.Utils

> import Control.Applicative
> import Control.Exception (Exception, try)
> import Control.Monad (ap)
> import Control.Monad.Error (runErrorT)
> import Control.Monad.Reader (ReaderT(..), MonadReader, asks)
> import Control.Monad.Trans (lift)

> import Data.ConfigFile (ConfigParser, get)
> import Data.Either.Utils (forceEither)
> import Data.List (intercalate)
> import Database.HaskellDB
> import Database.HaskellDB.HDBC
> import Database.HaskellDB.Sql.PostgreSQL (generator)
> import Network.CGI.Monad (MonadCGI(..), tryCGI)
> import Network.FastCGI (CGI, CGIT, outputNotFound)
> import Network.URI (escapeURIString, isUnescapedInURI)

> data AppEnv = AppEnv {  appDB          :: Connection,
>                         appHDB         :: Database,
>                         appCP          :: ConfigParser,
>                         appMemberNo    :: Maybe Integer,
>                         appMemberName  :: Maybe String,
>                         appMemberEmail :: Maybe String }

The App monad is a combination of the CGI and Reader monads.

> newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
>   deriving (Monad, MonadIO, MonadReader AppEnv)

...whose CGI monad uses the IO monad.

> type App a = AppT IO a

We need to make the App monad an Applicative Functor so that it will work with
formlets.

> instance Applicative (AppT IO) where
>   pure = return
>   (<*>) = ap

> instance Functor (AppT IO) where
>   fmap = liftM

To make the App monad an instance of MonadCGI, we need to define basic CGI
functions. CGI is relatively simple and its functionality can be defined on top
of just an environment getter and a function for adding headers. We reuse the
existing methods.

> instance MonadCGI (AppT IO) where
>   cgiAddHeader n v = AppT $ lift $ cgiAddHeader n v
>   cgiGet x = AppT $ lift $ cgiGet x

|runApp| does the job of creating the Reader environment and returning the
CGIResult from within the App monad to the CGI monad. The environment includes
a database handle, a configuration file, and some member information (if the
request came from a logged in member).

We can't use the convenience of |getOption| here as we're not in the App monad
yet.

> runApp :: Connection -> ConfigParser -> App CGIResult -> CGI CGIResult
> runApp c cp (AppT a) = do
>   let salt = forceEither $ get cp "DEFAULT" "authtokensalt"
>   token <- verifiedAuthToken salt
>   email <- liftIO $ maybe (return Nothing)
>                           (\n -> do
>                              e <- queryValue c  "SELECT email FROM member \
>                                                 \WHERE member_no = ?" [toSql n]
>                              return $ fromSql <$> e)
>                           (authMemberNo <$> token)
>   res <- runReaderT a $ AppEnv {  appDB          = c,
>                                   appHDB         = mkDatabase generator c,
>                                   appCP          = cp,
>                                   appMemberNo    = authMemberNo `liftM` token,
>                                   appMemberName  = authUsername `liftM` token,
>                                   appMemberEmail = email }
>   return res

At some point it's going to be essential to have all errors and notices logged
in 1 location. For now, the profusion of monads and exception handlers makes
this difficult. |logApp| will write a message to the database. It takes a type
name which are enumerated in the database.

> logApp :: String -> String -> App (String)
> logApp type' message = do
>   c <- asks appDB
>   liftIO $ logMsg c type' message

\subsection{Convenience Functions}

Here are some functions that abstract away even having to ask for values from
the Reader environment in the App monad.

\subsubsection{Identity}

|withMemberNumber| accepts a default action (for if the client isn't logged in)
and a function to carry out with the member's number otherwise.

> withMemberNumber :: a -> (Integer -> App a) -> App a
> withMemberNumber d f = asks appMemberNo >>= maybe (return d) f

|withRequiredMemberNumber| is like |withMemberNumber|, but it also checks to
see if the member has confirmed their email adress and provides a ``logged out
default'' of redirecting the client to the login page.

Use this any time a member number is generally required. If you only use
|withMemberNumber| you will allow actions from unconfirmed members.

> withRequiredMemberNumber :: (Integer -> App CGIResult) -> App CGIResult
> withRequiredMemberNumber f = do
>   memberNo <- asks appMemberNo
>   email <- asks appMemberEmail
>   case (memberNo, email) of
>     (Just mn, Just _)  -> f mn
>     (Just _, Nothing)  -> redirect =<< reversibleRedirect "/member/confirmation"
>     _                  -> redirect =<< reversibleRedirect "/member/login"

When we direct a user to some page, we might want to make sure that they can
find their way back to where they were. To do so, we get the current URI and
append it to the target page in the query string. The receiving page might know
what to do with it.

> reversibleRedirect :: String -> App String
> reversibleRedirect path = do
>   request <- fromMaybe "/" `liftM` getVar "REQUEST_URI"
>   return $ path ++ "?redirect=" ++ escapeURIString isUnescapedInURI request

We want to log 404 errors in the database, as they may indicate a problem or
opportunity with the site. This takes a list of Strings that are stored in the
log. It outputs to the user the requested URI.

> output404 :: [String] -> App CGIResult
> output404 s = do  logApp "404" (show s)
>                   outputNotFound $ intercalate "/" s

\subsubsection{Database}

When we're dealing with the database, there's always a chance we're going to
have some sort of error (there's a seemingly infinite number of possible
sources). We don't want the entire page to blow up if there are errors. Also,
we don't really care what the cause of the error is at the time of execution.
SQL errors are not something we can generally recover from. We just need to
log the error, return some sort of error indicator to the calling function (in
this case, Nothing), and get on with it.

In many cases, the calling function will still need to do data validation
anyway (make sure that a list of the expected size is returned, etc), so the
extra Maybe wrapper shouldn't be much extra trouble. In fact, in some cases
it's much easier than manually wrapping the query with |catchSql|.

Don't worry about understanding these definitions until you've read through the
DB module.

Maybe there's some way to cut down on this code with template Haskell or
somesuch, but it works for now.

> queryTuple' :: String -> [SqlValue] -> App (Maybe [SqlValue])
> queryTuple' sql vs = do
>   c <- asks appDB
>   liftIO $ (queryTuple c sql vs >>= return . Just) `catchSqlD` Nothing

> queryTuples' :: String -> [SqlValue] -> App (Maybe [[SqlValue]])
> queryTuples' sql vs = do
>   c <- asks appDB
>   liftIO $ (quickQuery' c sql vs >>= return . Just) `catchSqlD` Nothing

> queryValue' :: String -> [SqlValue] -> App (Maybe SqlValue)
> queryValue' sql vs = do
>   c <- asks appDB
>   liftIO $ (queryValue c sql vs) `catchSqlD` Nothing

> queryAttribute' :: String -> [SqlValue] -> App (Maybe [SqlValue])
> queryAttribute' sql vs = do
>   c <- asks appDB
>   liftIO $ (queryAttribute c sql vs >>= return . Just) `catchSqlD` Nothing

> quickInsertNo' :: String -> [SqlValue] -> String -> App (Maybe Integer)
> quickInsertNo' sql vs seqname = do
>   c <- asks appDB
>   liftIO $ quickInsertNo c sql vs seqname `catchSqlD` Nothing

> runStmt' :: String -> [SqlValue] -> App (Maybe Integer)
> runStmt' sql vs = do
>   c <- asks appDB
>   liftIO $ (run c sql vs >>= return . Just) `catchSqlD` Nothing

It may seem strange to return Maybe (), but we want to know if the database
change succeeded.

> quickStmt' :: String -> [SqlValue] -> App (Maybe ())
> quickStmt' sql vs = do
>   c <- asks appDB
>   liftIO $ (quickStmt c sql vs >>= return . Just) `catchSqlD` Nothing

Working with transactions outside of the App monad can be done, but we might as
well make a version that fits with the rest of the style of the program (logs
the exception and returns Nothing).

> withTransaction' :: App a -> App (Maybe a)
> withTransaction' actions = do
>   c <- asks appDB
>   r <- tryApp actions
>   case r of
>     Right x  -> do  liftIO $ commit c
>                     return $ Just x
>     Left e   -> do  logApp "exception" $ show e
>                     liftIO $ try (rollback c) -- Discard any exception here
>                     return Nothing

> run' :: String -> [SqlValue] -> App (Integer)
> run' sql vs = do
>   c <- asks appDB
>   liftIO $ run c sql vs

\subsubsection{Exceptions}

|tryApp| is like |tryCGI|. It allows us to catch exceptions within the App
monad. To do so, we unwrap the Reader monad and use |tryCGI| (which unwraps
another Reader and Writer).

> tryApp :: App a -> App (Either Exception a)
> tryApp (AppT c) = AppT (ReaderT (\r -> tryCGI (runReaderT c r)))

\subsubsection{Configuration}

Return a configuration option or log an error.

This always pulls from the @DEFAULT@ section. It also only supports strings.

> getOption :: String -> App (Maybe String)
> getOption option = do
>   cp <- asks appCP
>   opt <- runErrorT $ get cp "DEFAULT" option
>   case opt of
>     Left e   -> logApp "config" (show e) >> return Nothing
>     Right o  -> return $ Just o
