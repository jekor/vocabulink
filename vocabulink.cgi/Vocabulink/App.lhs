\section{The App Monad}
\label{App}

> module Vocabulink.App (App, AppEnv(..), runApp, liftIO, CGIResult) where

> import {-# SOURCE #-} Vocabulink.Member.Auth (loginNumber)

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
> import Control.Monad.Trans (lift)
> import Database.HDBC (IConnection, disconnect, quickQuery, toSql, fromSql,
>                       catchSql, SqlError(..))
> import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
> import Network.CGI.Monad (MonadCGI(..))
> import Network.FastCGI (CGI, CGIT, CGIResult, liftIO, MonadIO, logCGI)

Let's make getting at a database handle easier.

> data AppEnv = AppEnv { db           :: Connection,
>                        memberNumber :: Maybe Integer,
>                        memberName   :: Maybe String }

> newtype AppT m a = App (ReaderT AppEnv (CGIT m) a)
>   deriving (Monad, MonadIO, MonadReader AppEnv)

> type App a = AppT IO a

> instance MonadCGI (AppT IO) where
>   cgiAddHeader n v = App $ lift $ cgiAddHeader n v
>   cgiGet x = App $ lift $ cgiGet x

> runApp :: App CGIResult -> CGI CGIResult
> runApp (App a) = do
>   c <- liftIO $ connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"
>   memberNum <- loginNumber
>   username <- liftIO $ maybe (return Nothing) (memberNameFromNumber c) memberNum
>   res <- runReaderT a $ AppEnv {db = c, memberNumber = memberNum, memberName = username}
>   liftIO $ disconnect c
>   return res

> memberNameFromNumber :: IConnection conn => conn -> Integer -> IO (Maybe String)
> memberNameFromNumber c memberNo = do
>   n <- quickQuery c "SELECT username FROM member \
>                     \WHERE member_no = ?" [toSql memberNo]
>          `catchSql` (\e -> logCGI ("Failed to retrieve member name from number. \
>                                    \SQL Error: " ++ (init (seErrorMsg e))) >> return [])
>   case n of
>     [[x]] -> return $ Just $ decodeString $ fromSql x
>     _     -> return Nothing
