\section{Database}

We use @Database.HDBC@ for interfacing with PostgreSQL.

> module Vocabulink.DB (  catchSqlE, catchSqlD, query1, quickStmt,
>                         insertNo, quickInsertNo, queryColumn, queryTuple,
>                         logMsg, logException, connect,
>  {- Database.HDBC -}    quickQuery', fromSql, toSql, IConnection(..),
>  {- Database.HDBC.PostgreSQL -}  Connection) where

> import Vocabulink.Utils

> import Control.Exception (Exception(..), IOException, bracket)
> import Control.Monad (liftM)
> import Data.Maybe (catMaybes)
> import Database.HDBC
> import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
> import System.IO.Error (isUserError, ioeGetErrorString)

Here's how we establish a connection to the database.

> connect :: IO Connection
> connect = connectPostgreSQL "host=localhost \
>                             \dbname=vocabulink \
>                             \user=vocabulink \
>                             \password=phae9Xom"

Most of the time, if we have a SQL error, we're not prepared for it. We want to
log it and fail with some message to the user.

> catchSqlE :: IO a -> String -> IO a
> catchSqlE sql msg = catchSqlD sql (error msg)

Instead of erroring out, return a default value. Useful for errors that we
don't want the entire page crashing on.

This is a little bit wasteful, but we establish a new database connection. We
don't know if we have a connection at this point, and we don't want to have to
pass one if we do. Establishing an extra connection shouldn't be too much extra
hassle once we've already encountered an error condition.

> catchSqlD :: IO a -> a -> IO a
> catchSqlD sql d = sql `catchSql` (\e -> bracket (connect)
>                                                 (disconnect)
>                                                 (\c -> do  logSqlError c e
>                                                            return d))

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
>   rows <- quickQuery' c sql vs
>   return $ catMaybes $ map safeHead rows

And finally, sometimes we want a single tuple only. In this case, less than or
more than 1 tuple are regarded as error conditions.

> queryTuple :: IConnection conn => conn -> String -> [SqlValue] -> IO [SqlValue]
> queryTuple c sql vs = do
>   rows <- quickQuery' c sql vs
>   case rows of
>     []  -> error "No tuple found."
>     [r] -> return r
>     _   -> error "Multiple tuples found."

It's often tedious to work with transactions if you're just issuing a single
statement.

> quickStmt :: IConnection conn => conn -> String -> [SqlValue] -> IO ()
> quickStmt c' sql vs = do
>   withTransaction c' $ \c -> run c sql vs >> return ()

Run a quick insert and return the sequence number it created.

> quickInsertNo :: IConnection conn => conn -> String -> [SqlValue] -> String -> IO (Maybe Integer)
> quickInsertNo c' sql vs seqName =
>   withTransaction c' $ \c -> insertNo c sql vs seqName

> insertNo :: IConnection conn => conn -> String -> [SqlValue] -> String -> IO (Maybe Integer)
> insertNo c sql vs seqName = do
>   run c sql vs
>   seqNo <- query1 c "SELECT currval(?)" [toSql seqName]
>   return $ fromSql `liftM` seqNo

It's useful to have all errors logged in 1 location: the database.

|logMsg| takes a log type name ("SQL exception", "404", etc.) and a descriptive
message. The type and message is then logged to the database along with a
timestamp. If the message type is not found, it defaults to 'unknown'.

> logMsg :: IConnection conn => conn -> String -> String -> IO (String)
> logMsg c t s = do
>   quickStmt c "INSERT INTO log (type, message) \
>               \VALUES (COALESCE((SELECT name FROM log_types \
>                                 \WHERE name = ?), 'unknown'), ?)"
>               [toSql t, toSql s]
>     `catchSqlD` ()
>   return s

> logException :: IConnection conn => conn -> Exception -> IO (String)
> logException c e =
>   case sqlExceptions e of
>     Nothing  -> case e of
>                   (ErrorCall msg)   -> logMsg c "exception" msg
>                   (IOException ie)  -> logMsg c "IO exception" $
>                                          readableIOException ie
>                   e'                -> logMsg c "exception" (show e')
>     Just se  -> do  logSqlError c se
>                     return "Database Error"

> readableIOException :: IOException -> String
> readableIOException ioe =
>   isUserError ioe ? ioeGetErrorString ioe $ show ioe

> logSqlError :: IConnection conn => conn -> SqlError -> IO (String)
> logSqlError c se = logMsg c "SQL error" (init (seErrorMsg se))