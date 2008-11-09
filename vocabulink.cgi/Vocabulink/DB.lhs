> module Vocabulink.DB (catchSqlE, catchSqlD, query1, queryColumn, quickStmt, quickInsertNo, SqlType'(..)) where

> import Vocabulink.CGI (logSqlError)

> import Codec.Binary.UTF8.String (encodeString, decodeString)
> import Control.Monad (liftM)
> import Data.Maybe (catMaybes)
> import Database.HDBC

Most of the time, if we have a SQL error, we're not prepared for it. We want to
log it and fail with some message to the user.

> catchSqlE :: IO a -> String -> IO a
> catchSqlE sql msg = sql `catchSql` (\e -> logSqlError e >> error msg)

Instead of erroring out, return a default value. Useful for errors that I don't
want the entire page crashing on.

> catchSqlD :: IO a -> a -> IO a
> catchSqlD sql d = sql `catchSql` (\e -> logSqlError e >> return d)

Sometimes you just want to query a single value.

> query1 :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe SqlValue)
> query1 c sql vs = do
>   query <- prepare c sql
>   execute query vs
>   row <- fetchRow query
>   finish query
>   return $ (row >>= sqlFst)

> sqlFst :: [SqlValue] -> Maybe SqlValue
> sqlFst (x:_) = Just x
> sqlFst _     = Nothing

queryColumn is like query1, but for multiple rows.

> queryColumn :: IConnection conn => conn -> String -> [SqlValue] -> IO [SqlValue]
> queryColumn c sql vs = do
>   rows <- quickQuery c sql vs
>   return $ catMaybes $ map safeHead rows

> safeHead :: [a] -> Maybe a
> safeHead []    = Nothing
> safeHead (x:_) = Just x

It's often tedious to work with transactions if you're just issuing a single
statement.

> quickStmt :: IConnection conn => conn -> String -> [SqlValue] -> IO ()
> quickStmt c sql vs = do
>   withTransaction c $ \c' -> run c' sql vs >> return ()

Run a quick insert and return the sequence number it created.

> quickInsertNo :: IConnection conn => conn -> String -> [SqlValue] -> String -> IO (Maybe Integer)
> quickInsertNo c sql vs seqName = do
>   withTransaction c $ \c' -> do
>     run c' sql vs
>     seqNo <- query1 c' "SELECT currval(?)"
>                        [toSql' seqName]
>     return $ fromSql `liftM` seqNo

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

> instance SqlType' Bool where
>   toSql' = toSql
>   fromSql' = fromSql