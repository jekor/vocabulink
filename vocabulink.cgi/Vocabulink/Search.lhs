> module Vocabulink.Search where

> import Vocabulink.CGI (App, AppEnv(..))
> import Vocabulink.DB (fromSql', toSql')
> import Vocabulink.Html (stdPage)

> import Control.Monad.Reader (asks)
> import Data.Maybe (catMaybes)
> import Database.HDBC (SqlValue, fromSql, quickQuery)
> import Network.FastCGI (liftIO)

-- > searchPage :: App Html
-- > searchPage = stdPage

This is a basic search that searches through the origin and destination names
of the links in the database.

We'll stick to just searching through 10 results per page for now.

> search :: String -> App [(Integer, String, String)]
> search term = do
>   c <- asks db
>   results <- liftIO $
>     quickQuery c "SELECT link_no, origin, destination \
>                  \FROM link WHERE origin = ? OR destination = ? \
>                  \LIMIT 10"
>                  [toSql' term, toSql' term]
>   return $ catMaybes $ map tuplize results
>       where tuplize :: [SqlValue] -> Maybe (Integer, String, String)
>             tuplize [l,o,d] = Just (fromSql l, fromSql' o, fromSql' d)
>             tuplize _       = Nothing