> module Vocabulink.DB where

> import Vocabulink.Utils

> import Codec.Binary.UTF8.String
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Network.CGI (logCGI)
> import System.IO

> db :: IO Connection
> db =  connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"

It's often tedious to work with transactions if you're just inserting 1 tuple.

> quickInsert :: String -> [SqlValue] -> IO Integer
> quickInsert sql vs = do conn <- db
>                         withTransaction conn (\_ -> run conn sql vs >>= return)

Run a quick insert that should modify (add) 1 row or log and throw an error if
it doesn't.

> quickInsertCGI :: String -> [SqlValue] -> String -> IO ()
> quickInsertCGI sql vs err = do
>   n <- quickInsert sql vs
>   n == 1 ? return () $ do
>     logCGI $ "Query modified " ++ show n ++ " rows."
>     error err

> quickQuery1' :: String -> [SqlValue] -> IO (Maybe SqlValue)
> quickQuery1' sql vs = do
>   conn <- db
>   quickQuery1'' conn sql vs

> quickQuery1'' :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe SqlValue)
> quickQuery1'' conn sql vs = do
>   query <- prepare conn sql
>   execute query vs
>   row <- fetchRow query
>   finish query
>   case row of
>     Nothing -> return Nothing
>     Just r  -> case r of
>                []      -> return Nothing
>                (x:_)   -> return (Just x)

logSqlError will write the error to stderr where it should be picked up and added
to an appropriate logfile.

> logSqlError :: SqlError -> IO ()
> logSqlError e = do logCGI $ "SQL Error: " ++ (init (seErrorMsg e))
>                    return ()

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
