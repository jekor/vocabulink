> module Vocabulink.DB where

> import Codec.Binary.UTF8.String
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Network.CGI (logCGI)
> import System.IO

> db :: IO Connection
> db =  connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"

Sometimes you just want to query a single value.

> query1 :: String -> [SqlValue] -> IO (Maybe SqlValue)
> query1 sql vs = do
>   conn <- db
>   query1c conn sql vs

> query1c :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe SqlValue)
> query1c conn sql vs = do
>   query <- prepare conn sql
>   execute query vs
>   row <- fetchRow query
>   finish query
>   case row of
>     Nothing -> return Nothing
>     Just r  -> case r of
>                []      -> return Nothing
>                (x:_)   -> return (Just x)
>   `catchSql` (\e -> logSqlError e >> error (show e))

It's often tedious to work with transactions if you're just inserting 1 tuple.

> quickInsert :: String -> [SqlValue] -> IO ()
> quickInsert sql vs = do
>   conn <- db
>   withTransaction conn (\_ -> run conn sql vs >> return ())

Run a quick insert and return the sequence number it created.

> quickInsertSeqNo :: String -> [SqlValue] -> String -> IO (Maybe Integer)
> quickInsertSeqNo sql vs seqName = do
>   conn <- db
>   withTransaction conn (\_ -> do 
>                           run conn sql vs
>                           seqNo <- query1c conn "SELECT currval(?)"
>                                                      [toSql' seqName]
>                           case seqNo of
>                             Just n -> return $ fromSql n
>                             Nothing -> return Nothing)

Run a quick insert that should modify (add) 1 row or log and throw an error if
it doesn't.

I'm not sure if this is necessary, since the database should throw an exception
on a failed insert. I'll leave it here for a while.

-- > quickInsert' :: String -> [SqlValue] -> String -> IO ()
-- > quickInsert' sql vs err = do
-- >   n <- quickInsert sql vs
-- >   n == 1 ? return () $ do
-- >     logCGI $ "Query modified " ++ show n ++ " rows."
-- >     error err

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
