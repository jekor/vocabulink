\section{The App Monad}
\label{App}

> module Vocabulink.App (App, AppEnv(..), runApp, liftIO, CGIResult,
>                        logApp) where

> import {-# SOURCE #-} Vocabulink.Member.Auth (loginNumber)

> import Codec.Binary.UTF8.String (decodeString)
> import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
> import Control.Monad.Trans (lift)
> import Database.HDBC (IConnection, disconnect, quickQuery, toSql, fromSql,
>                       catchSql, SqlError(..), withTransaction, run)
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

At some point it's going to be essential to have all errors and notices logged
in 1 location. For now, the profusion of monads and exception handlers makes
this difficult.

|logApp| takes a log type name ("SQL error", "404", etc.) and a descriptive
message. The type and message is then logged to the database along with a
timestamp.

> logApp :: String -> String -> App (String)
> logApp t s = do
>   c' <- asks db
>   liftIO $ withTransaction c' $ \c ->
>     run c "INSERT INTO log (type, message) \
>                    \VALUES (COALESCE((SELECT name FROM log_types \
>                                      \WHERE name = ?), 'error'), ?)"
>           [toSql t, toSql s]
>   return s

> memberNameFromNumber :: IConnection conn => conn -> Integer -> IO (Maybe String)
> memberNameFromNumber c memberNo = do
>   n <- quickQuery c "SELECT username FROM member \
>                     \WHERE member_no = ?" [toSql memberNo]
>          `catchSql` (\e -> logCGI ("Failed to retrieve member name from number. \
>                                    \SQL Error: " ++ (init (seErrorMsg e))) >> return [])
>   case n of
>     [[x]] -> return $ Just $ decodeString $ fromSql x
>     _     -> return Nothing
