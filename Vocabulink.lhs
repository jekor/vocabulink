> module Main (main) where

> import Control.Monad
> import Control.Concurrent
> import Control.Exception

> import Data.Maybe
> import Data.List
> import Data.Dynamic

> import System.IO

> import Network.FastCGI
> import Network.URI

> import Text.ParserCombinators.Parsec hiding (getInput, try)
> import Text.XHtml.Strict (renderHtml, stringToHtml, (+++))
> import Codec.Binary.UTF8.String

> import Database.HDBC
> import Database.HDBC.PostgreSQL

> import Vocabulink.Utils
> import Vocabulink.Pages

> db :: IO Connection
> db =  connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"

> type Flashcard = (String, String) -- (question, answer)

It's often tedious to work with transactions if you're just inserting 1 tuple.

> quickInsert :: String -> [SqlValue] -> IO Integer
> quickInsert sql vs = do conn <- db
>                         withTransaction conn (\_ -> run conn sql vs >>= return)

Run a quick insert that should modify (add) 1 row or log and throw an error if
it doesn't.

> quickInsertCGI :: String -> [SqlValue] -> String -> IO ()
> quickInsertCGI sql vs err =
>     do n <- quickInsert sql vs
>        n == 1 ? return () $ do
>          logCGI $ "Query modified " ++ show n ++ " rows."
>          throwIO (DynException (toDyn $ err))

Encode a string to UTF8 and convert to a SQL value (or skip encoding if there
is no string).

> toSql' :: String -> SqlValue
> toSql' =  toSql . encodeString

> toSql'' :: Maybe String -> SqlValue
> toSql'' s = isNothing s ? SqlNull $ toSql $ encodeString (fromJust s)

getParameters works for string parameters. There is no type conversion. This
should also ensure that the parameters are not empty strings.

It might be nice to eventually have a context-sensitive way of getting CGI
inputs, similar to the context-sensitive regular expression operators in
Text.Regex. Things like :: String and :: Maybe String would be convenient.

> class CGIInputContext o where
>   getGET :: String -> CGI o

> instance CGIInputContext String where
>   getGET r = do s <- getInput r
>                 case s of
>                   Nothing -> throwCGI $ DynException (toDyn $ required r)
>                   Just "" -> throwCGI $ DynException (toDyn $ required r)
>                   Just x  -> return $ decodeString x
>                where required x = x ++ " parameter is required."

> instance CGIInputContext (Maybe String) where
>   getGET r = do s <- getInput r
>                 case s of
>                   Nothing -> return Nothing
>                   Just "" -> return Nothing
>                   Just x  -> return $ Just (decodeString x)

Add a member to the database. We're going to do validation of acceptable
username characters at this level because PostgreSQL's CHECK constraint doesn't
handle Unicode regular expressions properly.

A username should be allowed to have any alphanumeric characters (in any
language) and URL-safe punctuation.

> addMember :: String -> String -> Maybe String -> IO ()
> addMember username password email =
>     (length username) < 3  ? error "Your username must have 3 characters or more."  $
>     (length username) > 32 ? error "Your username must have 32 characters or less." $
>     -- TODO: Add password constraints?
>     quickInsertCGI "INSERT INTO member (username, email, password_hash) \
>                    \VALUES (?, ?, crypt(?, gen_salt('bf')))"
>                    [toSql' username, toSql'' email, toSql' password] errMsg
>        `catchSql` (logSqlError >> error errMsg)
>            where errMsg = "Failed to add member."

> addMember' :: CGI CGIResult
> addMember' = do
>     username <- getGET "username"
>     password <- getGET "password"
>     email    <- getGET "email"
>     liftIO $ addMember username password email
>     output $ "Welcome!"

Add a flashcard to the database. Provide a Flashcard and a username.

> addCard :: Flashcard -> String -> IO ()
> addCard f u = do quickInsertCGI  "INSERT INTO card (username, question, answer) \
>                                  \VALUES (?, ?, ?)"
>                                  [toSql u, toSql (fst f), toSql (snd f)] errMsg
>               `catchSql` (logSqlError >> error errMsg)
>                   where errMsg = "Failed to add card."

> addCard' :: CGI CGIResult
> addCard' = do question <- getInput "question"
>               answer <- getInput "answer"
>               if isNothing question || isNothing answer
>                  then outputError 400 "question, and answer parameters are required." []
>                  else do liftIO $ addCard (fromJust question, fromJust answer) "jekor"
>                          output $ "Added card."
                          
> getCard    :: Integer -> IO (Maybe Flashcard)
> getCard i  =  do conn <- db
>                  sql <- prepare conn "SELECT question, answer \
>                                      \FROM card WHERE card_no = ?"
>                  execute sql [toSql i]
>                  row <- fetchRow sql
>                  let [question, answer] = map fromSql (fromMaybe [toSql "", toSql ""] row) in
>                      return $ Just (question, answer)
>                  `catchSql` logError
>                      where logError e = error (show e) >> return Nothing

> intFromString :: String -> IO (Either Exception Integer)
> intFromString s = try (readIO s >>= return)

> getCard' :: String -> CGI CGIResult
> getCard' s = do n' <- liftIO $ intFromString s
>                 case n' of
>                   Left _  -> outputError 400 msg [msg]
>                   Right n -> do o <- liftIO $ getCard n
>                                 if isNothing o
>                                    then outputError 500 "Failed to retrieve card." []
>                                    else output $ show o
>     where msg = renderHtml $ stringToHtml s +++ " is not a valid card identifier."

We handle all requests in this module using a dispatcher.

> dispatch' :: CGI CGIResult
> dispatch' =  do uri <- requestURI
>                 method <- requestMethod
>                 setHeader "Content-Type" "text/html; charset=utf-8"
>                 case (pathPart uri) of
>                   Left err    -> outputError 500 (show err) []
>                   Right []    -> outputError 400 "Request not understood." []
>                   Right path' -> dispatch method path'
>                                  `catchCGI` displayError
>     where pathPart = (parse pathComponents "") . uriPath
>           displayError (DynException e) = outputError 400 (fromDyn e "Unknown") []
>           displayError e = throwCGI e

> dispatch :: String -> [String] -> CGI CGIResult
> dispatch "GET" ["test"] = output testPage
> dispatch "GET" ["card","new"] = output newCardPage
> dispatch "GET" ["card",c] = getCard' c
> dispatch "GET" ["member","new"] = output newMemberPage
> dispatch "GET" _ = outputError 404 "" []

It would be nice to automatically respond with "Method Not Allowed" on pages
that exist but don't take the POST/whatever method (as opposed to responding
with 404).

> dispatch "POST" ["card","new"] = addCard'
> dispatch "POST" ["member","new"] = addMember'
> dispatch "POST" _ = outputError 404 "Resource not found or POST not allowed on it." []

> dispatch _ _ = outputMethodNotAllowed ["GET", "POST"]

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepBy (many (noneOf "/")) (char '/')

logCGI will write the error to stderr where it should be picked up and added
to an appropriate logfile.

> logSqlError :: SqlError -> IO ()
> logSqlError e = do logCGI $ "SQL Error: " ++ (init (seErrorMsg e))
>                    return ()

It would be nice to have a way to hijack outputError in order to change the
encoding, but I don't know of a way to short of modifying the source of
Network.CGI. I'm going to leave it be for now as I'll probably end up with my
own error output functions in time.

> handleErrors' :: CGI CGIResult -> CGI CGIResult
> handleErrors' r = do setHeader "Content-Type" "text/html; charset=utf-8"
>                      handleErrors r

> main  :: IO ()
> main  =  runFastCGIConcurrent' forkIO 10 (handleErrors' dispatch')
