\section{The App Monad}
\label{App}

When I wrote the first version of Vocabulink, many functions passed around a
database connection. I now understand monads a little bit more, and it's easier
to store some information within an ``App'' monad. This reduces our function
signatures a little bit.

> module Vocabulink.App (      App, AppEnv(..), runApp, logApp,
>                              withMemberNumber, withRequiredMemberNumber,
>                              queryTuple', queryValue', queryAttribute',
>  {- Control.Monad.Reader -}  asks) where

> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Member.AuthToken
> import Vocabulink.Utils

> import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
> import Control.Monad.Trans (lift)

> import Network.CGI.Monad (MonadCGI(..))
> import Network.FastCGI (CGI, CGIT)
> import Network.URI (escapeURIString, isUnescapedInURI)

> data AppEnv = AppEnv {  db            :: Connection,
>                         memberNumber  :: Maybe Integer,
>                         memberName    :: Maybe String }

The App monad is a combination of the CGI and Reader monads.

> newtype AppT m a = App (ReaderT AppEnv (CGIT m) a)
>   deriving (Monad, MonadIO, MonadReader AppEnv)

...and IO monad.

> type App a = AppT IO a

To make the App monad an instance of MonadCGI, we need to define basic CGI
functions. CGI is relatively simple and its functionality can be defined on top
of just an environment getter and a function for adding headers. We reuse the
existing methods.

> instance MonadCGI (AppT IO) where
>   cgiAddHeader n v = App $ lift $ cgiAddHeader n v
>   cgiGet x = App $ lift $ cgiGet x

|runApp| does the job of creating the Reader environment and returning the
CGIResult from within the App monad to the CGI monad.

> runApp :: Connection -> App CGIResult -> CGI CGIResult
> runApp c (App a) = do
>   token <- verifiedAuthToken
>   res <- runReaderT a $ AppEnv {  db            = c,
>                                   memberNumber  = Just . authMemberNo =<< token,
>                                   memberName    = Just . authUsername =<< token}
>   return res

At some point it's going to be essential to have all errors and notices logged
in 1 location. For now, the profusion of monads and exception handlers makes
this difficult.

> logApp :: String -> String -> App (String)
> logApp t s = do
>   c <- asks db
>   liftIO $ logMsg c t s

\subsection{Convenience Functions}

Here are some functions that abstract away even having to ask for the
environment in the App monad.

\subsubsection{Identity}

|withMemberNumber| accepts a default value (for if the client isn't logged in)
and a function to carry out with the member's number otherwise.

> withMemberNumber :: a -> (Integer -> App a) -> App a
> withMemberNumber d f = asks memberNumber >>= maybe (return d) f

|withRequiredMemberNumber| is like |withMemberNumber|, but it provides a
``logged out default'' of redirecting the client to the login page.

> withRequiredMemberNumber :: (Integer -> App CGIResult) -> App CGIResult
> withRequiredMemberNumber f =  asks memberNumber >>=
>                               maybe (loginRedirectPage >>= redirect) f

When we direct a user to the login page, we want to make sure that they can
find their way back to where they were. To do so, we get the current URI and
append it to the login page in the query string. The login page will know what
to do with it.

> loginRedirectPage :: App String
> loginRedirectPage = do
>   request <- fromMaybe "/" `liftM` getVar "REQUEST_URI"
>   return $ "/member/login?redirect=" ++ escapeURIString isUnescapedInURI request

\subsubsection{Database}

When we're dealing with the database, there's always a chance we're going to
have some sort of error (there's a seemingly infinite number of possible
sources). We don't want the entire page to blow up if there are. Also, we don't
really care what the cause of the error is at the time of execution. SQL errors
are not something you can generally recover from. We just need to log the
error, return some sort of error indicator to the calling function (in this
case, Nothing), and get on with it.

In many cases, the calling function will still need to do data validation
anyway (make sure that a list of the expected size is returned, etc), so the
extra Maybe wrapper shouldn't be much extra trouble. In fact, in some cases
it's much easier than manually wrapping the query with |catchSql|.

> queryTuple' :: String -> [SqlValue] -> App (Maybe [SqlValue])
> queryTuple' sql vs = do
>   c <- asks db
>   liftIO $ (queryTuple c sql vs >>= return . Just) `catchSqlD` Nothing

> queryValue' :: String -> [SqlValue] -> App (Maybe (Maybe SqlValue))
> queryValue' sql vs = do
>   c <- asks db
>   liftIO $ (queryValue c sql vs >>= return . Just) `catchSqlD` Nothing

> queryAttribute' :: String -> [SqlValue] -> App (Maybe [SqlValue])
> queryAttribute' sql vs = do
>   c <- asks db
>   liftIO $ (queryAttribute c sql vs >>= return . Just) `catchSqlD` Nothing
