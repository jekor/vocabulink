> import Control.Monad
> import Control.Concurrent
> import Control.Exception

> import Network.FastCGI
> import Network.URI
> import qualified Text.XHtml.Strict as H

> import Text.ParserCombinators.Parsec

> import Database.HDBC
> import Database.HDBC.PostgreSQL

> db :: IO Connection
> db =  connectPostgreSQL "host=localhost dbname=vocabulink user=vocabulink password=phae9Xom"

> welcome :: CGI CGIResult
> welcome =  output $ H.renderHtml $
>            H.header H.<< H.thetitle H.<< "Welcome" H.+++ H.body H.<< H.h1 H.<< "Welcome"

> getCard    :: Integer -> IO (String)
> getCard i  =  do conn <- db
>                  paths <- quickQuery conn
>                           "SELECT filepath \
>                           \FROM card WHERE card_no = ?" [toSql i]
>                  let path' = head $ head paths in
>                      return $ fromSql path'
>                  `catchSql` logSqlError

> getCard' :: String -> CGI CGIResult
> getCard' c = do o <- liftIO $ handle notInt (getCard n)
>                 output o
>     where n = read c :: Integer
>           notInt = error "Card is not an integer"

We handle all requests in this module using a dispatcher.

> get            :: [String] -> CGI CGIResult
> get ["card",c] = getCard' c
> get [""]       = welcome
> get _          = outputError 404 ("not found") []

> dispatch :: CGI CGIResult
> dispatch =  do uri <- requestURI
>                method <- requestMethod
>                case (pathPart uri) of
>                  Left err        -> outputError 400 (show err) []
>                  Right []        -> outputError 400 "Request not understood." []
>                  Right path'     -> case method of
>                                     "GET" -> get path'
>                                     _     -> outputMethodNotAllowed ["GET"]
>     where pathPart = (parse pathComponents "") . uriPath

> pathComponents :: Parser [String]
> pathComponents =  char '/' >> sepBy (many (noneOf "/")) (char '/')

Log a SQL error to a file. We could use logCGI to send the error to stderr, but
spawn-fcgi doesn't seem to support logging stderr. For now, we'll dump the
messages into /tmp.

-- > logSqlError :: SqlError -> IO (String)
-- > logSqlError e = do logCGI (show e)
-- >                    return $ show e

> logSqlError :: SqlError -> IO (String)
> logSqlError e = do appendFile "/tmp/flashcard.log" msg
>                    return msg
>     where msg = show e

> main  :: IO ()
> main  =  runFastCGIConcurrent' forkIO 10 (handleErrors dispatch)
