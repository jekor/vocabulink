> module Vocabulink.Card where

> import Vocabulink.Utils
> import Vocabulink.CGI
> import Vocabulink.DB

> import Data.Maybe
> import Database.HDBC
> import Network.FastCGI
> import Text.XHtml.Strict

> type Flashcard = (String, String) -- (question, answer)

Add a flashcard to the database. Provide a Flashcard and a username.

> addCard :: Flashcard -> String -> IO ()
> addCard f u = do quickInsertCGI  "INSERT INTO card (username, question, answer) \
>                                  \VALUES (?, ?, ?)"
>                                  [toSql u, toSql (fst f), toSql (snd f)] errMsg
>               `catchSql` (\e -> logSqlError e >> error errMsg)
>                   where errMsg = "Failed to add card."

> addCard' :: CGI CGIResult
> addCard' = do question <- getInput "question"
>               answer <- getInput "answer"
>               if isNothing question || isNothing answer
>                  then outputError' 400 "question, and answer parameters are required." []
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

> getCard' :: String -> CGI CGIResult
> getCard' s = do n' <- liftIO $ intFromString s
>                 case n' of
>                   Left _  -> outputError' 400 msg [msg]
>                   Right n -> do o <- liftIO $ getCard n
>                                 if isNothing o
>                                    then outputError' 500 "Failed to retrieve card." []
>                                    else output' $ show o
>     where msg = renderHtml $ stringToHtml s +++ " is not a valid card identifier."