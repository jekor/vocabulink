\section{Database}

We use @Database.HDBC@ for interfacing with PostgreSQL.

> module Vocabulink.DB (catchSqlE, catchSqlD, query1, quickStmt,
>                       insertNo, quickInsertNo, quickQuery', queryColumn,
>                       queryTuple, fromSql, toSql, IConnection(..)) where

> import Vocabulink.CGI (logSqlError)

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
>   rows <- quickQuery' c sql vs
>   return $ catMaybes $ map safeHead rows

> safeHead :: [a] -> Maybe a
> safeHead []    = Nothing
> safeHead (x:_) = Just x

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