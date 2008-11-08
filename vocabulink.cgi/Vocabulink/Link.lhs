> module Vocabulink.Link where

> import Vocabulink.CGI (App, getInput', getInputDefault)
> import Vocabulink.DB (quickInsertNo, catchSqlE, fromSql', toSql')
> import Vocabulink.Html (outputHtml, page, Dependency(..), pager)
> import Vocabulink.Member (loginNumber)
> import Vocabulink.Review.Html (reviewHtml)
> import Vocabulink.Utils (intFromString)

> import Codec.Binary.UTF8.String (encodeString)
> import Control.Monad (liftM)
> import Control.Monad.Reader (ask)
> import Database.HDBC (quickQuery', toSql, fromSql, SqlValue)
> import Network.FastCGI (CGIResult, liftIO, redirect, outputError)
> import Text.XHtml.Strict

origin should already be UTF8 encoded.

> linkHtml :: Html -> Html -> Html
> linkHtml origin destination = concatHtml
>   [ thespan ! [theclass "lexeme"] << origin,
>     image ! [src "http://s.vocabulink.com/black.png", width "20%", height "1"],
>     thespan ! [theclass "lexeme"] << destination ]

> getLink :: Integer -> App (String, String)
> getLink linkNo = do
>   c <- ask
>   r <- liftIO $ quickQuery' c "SELECT origin, destination FROM link \
>                               \WHERE link_no = ?" [toSql linkNo]
>                   `catchSqlE` "Link not found."
>   case r of
>     [[o,d]] -> return (fromSql' o, fromSql' d)
>     _       -> error "Link not found."

> newLinkPage :: App CGIResult
> newLinkPage = do
>   origin <- encodeString `liftM` getInput' "origin"
>   destination <- encodeString `liftM` getInput' "destination"
>   let t = origin ++ " -> " ++ destination
>   outputHtml $ page t [CSS "lexeme"]
>     [ form ! [action "", method "post"] <<
>        [ thediv ! [identifier "baseline", theclass "link"] <<
>            linkHtml (stringToHtml origin) (stringToHtml destination),
>          paragraph ! [identifier "association"] <<
>            [ textarea ! [name "association", cols "80", rows "20"] <<
>                "Describe the association here.",
>              br,
>              submit "" "Associate" ] ] ]

> linkLexemes :: String -> String -> String -> Integer -> App (Maybe Integer)
> linkLexemes origin destination association n = do
>   c <- ask
>   liftIO $ quickInsertNo c "INSERT INTO link (origin, destination, link_type, \
>                                              \language, representation, author) \
>                            \VALUES (?, ?, 'association', 'en', ?, ?)"
>                            [toSql' origin, toSql' destination, toSql' association, toSql n]
>                            "link_link_no_seq"
>              `catchSqlE` "Failed to establish link."

> linkLexemes' :: App CGIResult
> linkLexemes' = do
>   origin <- getInput' "origin"
>   destination <- getInput' "destination"
>   association <- getInput' "association"
>   loginNo <- loginNumber
>   linkNo <- linkLexemes origin destination association loginNo
>   case linkNo of
>     Just n  -> redirect $ "/link/" ++ (show n)
>     Nothing -> error "Failed to establish link."

> linkPage :: String -> App CGIResult
> linkPage link = do
>   no <- liftIO $ intFromString link
>   case no of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       memberNo <- loginNumber
>       c <- ask
>       ts <- liftIO $ quickQuery' c "SELECT origin, destination, representation FROM link \
>                                    \WHERE link_no = ?" [toSql n]
>                        `catchSqlE` "Failed to retrieve link."
>       review <- reviewHtml memberNo n
>       case ts of
>         [x@[_,_,_]] -> do
>             let [origin, destination, association] = map (encodeString . fromSql') x
>                 t = origin ++ " -> " ++ destination
>             outputHtml $ page t [CSS "lexeme"]
>               [ review,
>                 thediv ! [identifier "baseline", theclass "link"] <<
>                   linkHtml (stringToHtml origin) (stringToHtml destination),
>                 paragraph ! [identifier "association"] << association ]
>         _ -> error "Link does not exist or failed to retrieve."

Generate a page of links for the specified member or all members (for Nothing).

> linksPage :: App CGIResult
> linksPage = do
>   pg  <- getInputDefault 1 "pg"
>   n   <- getInputDefault 25 "n"
>   links <- getLinks ((pg - 1) * n) (n + 1)
>   pagerControl <- pager n pg $ (length links) + ((pg - 1) * n)
>   outputHtml $ page "Links" [CSS "lexeme"]
>     [ (take n $ map displayLink links) +++ pagerControl ]

> getLinks :: Int -> Int -> App [[SqlValue]]
> getLinks offset limit = do
>   c <- ask
>   liftIO $ quickQuery' c "SELECT link_no, origin, destination FROM link \
>                          \ORDER BY link_no OFFSET ? LIMIT ?"
>                          [toSql offset, toSql limit]
>              `catchSqlE` "Failed to retrieve links."

> displayLink :: [SqlValue] -> Html
> displayLink [no, origin, destination] = 
>   let no' = fromSql no :: Integer
>       origin' = fromSql' origin :: String
>       destination' = fromSql' destination :: String in
>   thediv ! [theclass "link"] << anchor ! [href $ "/link/" ++ (show no')] <<
>     linkHtml (stringToHtml (encodeString origin')) (stringToHtml (encodeString destination'))
> displayLink _ = thediv ! [theclass "link"] << "Link is malformed."