> module Vocabulink.DB where

> import Codec.Binary.UTF8.String
> import Control.Exception
> import Control.Monad.Reader
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Network.CGI
> import System.IO

> import Vocabulink.CGI

> db :: IO Connection
> db =  connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"

Most of the time, if we have a SQL error, we're not prepared for it. We want to
log it and fail with some message to the user.

> catchSqlE :: IO a -> String -> IO a
> catchSqlE sql msg = sql `catchSql` (\e -> logSqlError e >> error msg)

Eventually, I'd like for a database handle to just be available at all times to
any function that needs it. For now, I still don't quite have the hange of
monad transformers.

> type DbM a = ReaderT ConnWrapper IO a

This is like quickQuery but in the DbM monad.

> fetch :: String -> [SqlValue] -> DbM [[SqlValue]]
> fetch sql vs = do
>   conn <- ask
>   liftIO $ quickQuery conn sql vs

-- > testDbM :: CGI CGIResult
-- > testDbM = do
-- >   conn <- liftIO db
-- >   ts <- liftIO $ runReaderT (qckQuery "blah" [toSql (1 :: Integer)]) (ConnWrapper conn)
-- >   output $ show ts

Sometimes you just want to query a single value.

> query1 :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe SqlValue)
> query1 c sql vs = do
>   query <- prepare c sql
>   execute query vs
>   row <- fetchRow query
>   finish query
>   case row of
>     Nothing -> return Nothing
>     Just r  -> case r of
>                []      -> throwDyn $ SqlError {seState = "", seNativeError = 0,
>                                                seErrorMsg = "Empty tuple."}
>                (x:_)   -> return (Just x)

Sometimes we want to query a value without wanting to bother checking to see if
we actually received a value. We'd rather just get an exception, since we don't
expect failure and we're checking for exceptions already. This returns our own
error masquerading as a SqlError so that it's caught by catchSql. Since we
don't expect it to happen, it /shouldn't/ be an issue. I'd just rather have a
semantic problem than show users that Prelude.head failed.

> query1e :: IConnection conn => conn -> String -> [SqlValue] -> IO (SqlValue)
> query1e c sql vs = do
>   query <- prepare c sql
>   execute query vs
>   tuple <- fetchRow query
>   finish query
>   case tuple of
>     Nothing -> throwDyn $ SqlError {seState = "", seNativeError = 0,
>                                     seErrorMsg = "No results."}
>     Just t  -> case t of
>                  []    -> throwDyn $ SqlError {seState = "", seNativeError = 0,
>                                                seErrorMsg = "Empty tuple."}
>                  (x:_) -> return x

It's often tedious to work with transactions if you're just inserting 1 tuple.

> quickInsert :: IConnection conn => conn -> String -> [SqlValue] -> IO ()
> quickInsert c sql vs = do
>   withTransaction c $ \c' -> run c' sql vs >> return ()

Run a quick insert and return the sequence number it created.

> quickInsertNo :: IConnection conn => conn -> String -> [SqlValue] -> String -> IO (Maybe Integer)
> quickInsertNo c' sql vs seqName = do
>   withTransaction c' $ \c -> do 
>                           run c sql vs
>                           seqNo <- query1 c "SELECT currval(?)"
>                                             [toSql' seqName]
>                           case seqNo of
>                             Just n -> return $ fromSql n
>                             Nothing -> return Nothing

Let's define some helpers to keep from forgetting to decode/encode UTF8
strings. Ultimately, I'd like to have a UTF8String type that could use the type
system to make sure that we always decode/encode properly. For now, these will
have to do.

> class SqlType' a where
>   toSql'   :: a -> SqlValue
>   fromSql' :: SqlValue -> a

> instance SqlType' String where
>   toSql'   = toSql . encodeString
>   fromSql' = decodeString . fromSql

> instance SqlType' (Maybe String) where
>   toSql' Nothing  = SqlNull
>   toSql' (Just s) = toSql $ encodeString s
>   fromSql' (SqlString s) = Just (decodeString s)
>   fromSql' _             = Nothing -- SqlNull and everything else

> instance SqlType' Integer where
>   toSql' = toSql
>   fromSql' = fromSql

> instance SqlType' Int where
>   toSql' = toSql
>   fromSql' = fromSql
