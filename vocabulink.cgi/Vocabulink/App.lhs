\section{The App Monad}
\label{App}

When I wrote the first version of Vocabulink, many functions passed around a
database connection. I now understand monads a little bit more, and it's easier
to store some information within an ``App'' monad. This reduces our function
signatures a little bit.

> module Vocabulink.App (  App, AppEnv(..), runApp, logApp,
>  {- Network.FastCGI -}   liftIO, CGIResult) where

> import Vocabulink.DB
> import {-# SOURCE #-} Vocabulink.Member (memberNameFromNumber)
> import {-# SOURCE #-} Vocabulink.Member.Auth (loginNumber)

> import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
> import Control.Monad.Trans (lift)

> import Database.HDBC (disconnect)
> import Network.CGI.Monad (MonadCGI(..))
> import Network.FastCGI (CGI, CGIT, CGIResult, liftIO, MonadIO)

> data AppEnv = AppEnv { db           :: Connection,
>                        memberNumber :: Maybe Integer,
>                        memberName   :: Maybe String }

> newtype AppT m a = App (ReaderT AppEnv (CGIT m) a)
>   deriving (Monad, MonadIO, MonadReader AppEnv)

> type App a = AppT IO a

> instance MonadCGI (AppT IO) where
>   cgiAddHeader n v = App $ lift $ cgiAddHeader n v
>   cgiGet x = App $ lift $ cgiGet x

There might be a chance that we don't close our database connection properly if
we trigger an exception within runReaderT. I don't know if that's possible.

> runApp :: App CGIResult -> CGI CGIResult
> runApp (App a) = do
>   c <- liftIO connect
>   memberNum <- loginNumber
>   username <- liftIO $ maybe (return Nothing) (memberNameFromNumber c) memberNum
>   res <- runReaderT a $ AppEnv {db = c, memberNumber = memberNum, memberName = username}
>   liftIO $ disconnect c
>   return res

At some point it's going to be essential to have all errors and notices logged
in 1 location. For now, the profusion of monads and exception handlers makes
this difficult.

> logApp :: String -> String -> App (String)
> logApp t s = do
>   c <- asks db
>   liftIO $ logMsg c t s