> module Main (main) where

> import Control.Monad
> import Control.Concurrent
> import Control.Exception

> import Data.Maybe

> import Network.FastCGI
> import Network.URI

> import Text.ParserCombinators.Parsec hiding (getInput, try)

> import Database.HDBC
> import Database.HDBC.PostgreSQL

> import Text.XHtml.Strict (renderHtml, stringToHtml, (+++))

> import Flashcard.Pages

> db :: IO Connection
> db =  connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"

> type Flashcard = (String, String) -- (question, answer)

> getCard    :: Integer -> IO (Maybe Flashcard)
> getCard i  =  do conn <- db
>                  sql <- prepare conn "SELECT question, answer \
>                                      \FROM card WHERE card_no = ?"
>                  execute sql [toSql i]
>                  row <- fetchRow sql
>                  let [question, answer] = map fromSql (fromMaybe [toSql "", toSql ""] row) in
>                      return $ Just (question, answer)
>                  `catchSql` logError
>                      where logError e = do error $ show e
>                                            return Nothing

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

It's often tedious to work with transactions if you're just inserting 1 tuple.

> quickInsert :: String -> [SqlValue] -> IO Integer
> quickInsert sql vs = do conn <- db
>                         withTransaction conn (\_ -> run conn sql vs >>= return)

Add a flashcard to the database. Provide a Flashcard, flashcard name, and
username.

> addCard :: Flashcard -> String -> String -> IO ()
> addCard f n u = do c <- quickInsert  "INSERT INTO card (username, card_name, question, answer) \
>                                      \VALUES (?, ?, ?, ?)"
>                                      [toSql u, toSql n, toSql (fst f), toSql (snd f)]
>                    if c == 1
>                       then return ()
>                       else logCGI $ "Query modified " ++ show c ++ " rows." >> error "Failed to add card."
>                    `catchSql` (logSqlError >> error "Failed to add card.")

> addCard' :: CGI CGIResult
> addCard' = do name <- getInput "name"
>               question <- getInput "question"
>               answer <- getInput "answer"
>               if isNothing name || isNothing question || isNothing answer
>                  then outputError 400 "name, question, and answer parameters are required" []
>                  else do liftIO $ addCard (fromJust question, fromJust answer)
>                                              (fromJust name) "jekor"
>                          output $ "Added card."
                          
We handle all requests in this module using a dispatcher.

> dispatch' :: CGI CGIResult
> dispatch' =  do uri <- requestURI
>                 method <- requestMethod
>                 case (pathPart uri) of
>                   Left err    -> outputError 500 (show err) []
>                   Right []    -> outputError 400 "Request not understood." []
>                   Right path' -> dispatch method path'
>     where pathPart = (parse pathComponents "") . uriPath

> dispatch :: String -> [String] -> CGI CGIResult
> dispatch "GET" ["card",c] = getCard' c
> dispatch "GET" ["new"] = output newCardPage
> dispatch "GET" _ = outputError 404 "" []

It would be nice to automatically respond with "Method Not Allowed" on pages
that exist but don't take the POST/whatever method (as opposed to responding
with 404).

> dispatch "POST" ["new"] = addCard'
> dispatch "POST" _ = outputError 404 "Resource not found or POST not allowed on it." []

> dispatch _ _ = outputMethodNotAllowed ["GET", "POST"]

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepBy (many (noneOf "/")) (char '/')

logCGI will write the error to stderr where it should be picked up and added
to an appropriate logfile.

> logSqlError :: SqlError -> IO ()
> logSqlError e = do logCGI $ "SQL Error: " ++ (init (seErrorMsg e))
>                    return ()

> main  :: IO ()
> main  =  runFastCGIConcurrent' forkIO 10 (handleErrors dispatch')
