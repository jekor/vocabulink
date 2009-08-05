% Copyright 2008, 2009 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\section{Database}

Vocabulink makes heavy use of PostgreSQL. We use @Database.HDBC@ for
interfacing with it.

This module is possibly the most dangerous and error-prone we have. Because we
interface with the database via strings, we can't do type checking. We're also
dealing with ever-changing state.

> module Vocabulink.DB (           queryTuple, queryValue, queryAttribute,
>                                  quickStmt, insertNo, quickInsertNo,
>                                  catchSqlD, catchSqlE, logMsg, logException,
>                                  connect,
>  {- Database.HDBC -}             SqlValue(..), toSql, fromSql, iToSql,
>                                  withTransaction, quickQuery, quickQuery',
>                                  IConnection(..), execute, catchSql,
>  {- Database.HDBC.PostgreSQL -}  Connection) where

We need to keep this module independent of most other modules as most modules
need to utilize the database in some way.

> import Vocabulink.Utils

> import Control.Exception (bracket)
> import Database.HDBC
> import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

Here's how we establish a connection to the database. I'd like to have the
database password stored in the configuration file, but it would make the code
far more complex.

> connect :: IO Connection
> connect = connectPostgreSQL  "host=localhost \
>                              \dbname=vocabulink \
>                              \user=vocabulink \
>                              \password=phae9Xom"

\subsection{Query Helpers}

HDBC provides a pretty basic interface. If we relied on it, we'd be doing a lot
of housekeeping and repetitive work throughout the code. Here are some
higher-level interfaces to the database.

Sometimes we want just the first tuple of a query result. If the query returns
multiple tuples, all but the first will be silently discarded.

> queryTuple :: IConnection conn => conn -> String -> [SqlValue] -> IO [SqlValue]
> queryTuple c sql vs = safeHead [] `liftM` quickQuery' c sql vs

Sometimes we just want to retrieve a single attribute from a single tuple.
This will return either Just the value you were expecting or Nothing.

> queryValue :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe SqlValue)
> queryValue c sql vs = do
>   t <- queryTuple c sql vs
>   return $ case t of
>     [SqlNull]  -> Nothing
>     [x]        -> Just x
>     _          -> Nothing

And finally, sometimes we just want to retrieve a single attribute for
multiple tuples. This assumes that the attribute you want is the first one
@SELECT@ed.

> queryAttribute :: IConnection conn => conn -> String -> [SqlValue] -> IO [SqlValue]
> queryAttribute c sql vs = map head `liftM` quickQuery' c sql vs

It's often tedious to work with transactions if you're just issuing a single
statement.

> quickStmt :: IConnection conn => conn -> String -> [SqlValue] -> IO ()
> quickStmt c' sql vs = withTransaction c' $ \c -> run c sql vs >> return ()

A common task is to insert a tuple and get its sequence number. This should
only be used for inserting into tables with a sequence-based primary key
(e.g. the @SERIAL@ type).

> insertNo ::  IConnection conn =>
>              conn -> String -> [SqlValue] -> String -> IO (Maybe Integer)
> insertNo c sql vs seqName = do
>   run c sql vs
>   seqNo <- queryValue c "SELECT currval(?)" [toSql seqName]
>   return $ fmap fromSql seqNo

This is the same as |insertNo|, but with its own transaction.

> quickInsertNo ::  IConnection conn =>
>                   conn -> String -> [SqlValue] -> String -> IO (Maybe Integer)
> quickInsertNo c' sql vs seqName =
>   withTransaction c' $ \c -> insertNo c sql vs seqName

\subsection{Error Handling}

Most of the time, if we have a SQL error, we're not prepared for it. We want to
log it and fail with some message to the user. We hope that we don't ever end
up in this code, because it blows up the entire HTTP request. It's here only
for unrecoverable situations and its only purpose is to log the error and hide
the gory details from the client.

For now, we should use the following catches after every database query or
statement we execute. It's tedious, but that's what we have to deal with if we
want access to a database. There may be a better way of handling errors, but I
haven't found it yet.

The most common cause for triggering a database exception is through malformed
SQL, so we probably find most of these during early testing.

> catchSqlE :: IO a -> String -> IO a
> catchSqlE sql msg = catchSqlD sql (error msg)

Instead of erroring out, it might make more sense to return a default value.
When we don't want the entire request crashing, this is a better alternative to
|catchSqlE|.

This is a little bit wasteful, but if we do catch an exception we establish a
new database connection. We do this so that we don't have to pass around a
database connection. But also, we may have been in a transaction or otherwise
ruined our main connection and need a new one for logging the error message.
Establishing an extra connection shouldn't be too much extra trouble: we've
already encountered an error condition, how much worse can it get?

> catchSqlD :: IO a -> a -> IO a
> catchSqlD sql d = sql `catchSql` (\e -> bracket (connect)
>                                                 (disconnect)
>                                                 (\c -> do  logSqlError c e
>                                                            return d))

It's useful to have all errors logged in 1 location: the database.

|logMsg| takes a log type name (``SQL exception'', ``404'', etc.) and a descriptive
message. The type and message is then logged to the database along with a
timestamp. If the message type is not found, it defaults to ``unknown''.

> logMsg :: IConnection conn => conn -> String -> String -> IO (String)
> logMsg c t s = do
>   quickStmt c  "INSERT INTO log (type, message) \
>                \VALUES ((SELECT name FROM log_type WHERE name = ?), ?)"
>                [toSql t, toSql s]
>     `catchSqlD` ()
>   return s

Exceptions come in different shapes and sizes, and we'd like to have log
information about them when we encounter them. Or, we might want to ignore
certain exceptions.

-- > logException :: (IConnection conn, Exception e) => conn -> e -> IO (String)
-- > logException c e =
-- >   case cast e of
-- >     Just (SqlError _ _ m)  -> do  logMsg c "SQL error" m
-- >                                   return "Database Error"
-- >     _                      -> logMsg c "exception" (show e)

> logException :: (IConnection conn) => conn -> SomeException -> IO (String)
> logException c e = logMsg c "exception" (show e)

|logSqlError| is used by |logException|, |catchSqlD|, and |catchSqlE|. It's
special because it's potentially the most common type of exception we'll
encounter, and the one we most want to hear about.

> logSqlError :: IConnection conn => conn -> SqlError -> IO (String)
> logSqlError c se = logMsg c "SQL error" (init (seErrorMsg se))