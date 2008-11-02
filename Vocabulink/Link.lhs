> module Vocabulink.Link where

> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member
> import Vocabulink.Utils

> import Codec.Binary.UTF8.String
> import Database.HDBC
> import Network.CGI
> import Text.XHtml.Strict

> linkHtml :: String -> String -> Html
> linkHtml origin destination = concatHtml
>   [ thespan ! [theclass "lexeme"] << encodeString origin,
>     image ! [src "http://s.vocabulink.com/black.png", width "20%", height "1"],
>     thespan ! [theclass "lexeme"] << encodeString destination ]

> newLinkPage :: CGI CGIResult
> newLinkPage = do
>   origin <- getInput' "origin"
>   destination <- getInput' "destination"
>   let t = (encodeString origin) ++ " -> " ++ (encodeString destination)
>   output $ renderHtml $ page t ["lexeme"]
>     [ form ! [action "", method "post"] <<|
>        [ thediv ! [identifier "baseline", theclass "link"] <<
>            (linkHtml origin destination),
>          paragraph ! [identifier "association"] <<|
>            [ textarea ! [name "association", cols "80", rows "20"] <<
>                "Describe the association here.",
>              br,
>              input ! [thetype "submit", value "Associate"] ] ] ]

> linkLexemes :: IConnection conn => conn -> String -> String -> String -> Integer -> IO (Maybe Integer)
> linkLexemes c origin destination association n = do
>   quickInsertNo c "INSERT INTO link (origin, destination, link_type, \
>                                      \lingvo, representation, author) \
>                   \VALUES (?, ?, 'association', 'en', ?, ?)"
>                   [toSql' origin, toSql' destination, toSql' association, toSql n]
>                   "link_link_no_seq"
>     `catchSqlE` "Failed to establish link."

> linkLexemes' :: CGI CGIResult
> linkLexemes' = do
>   c <- liftIO db
>   origin <- getInput' "origin"
>   destination <- getInput' "destination"
>   association <- getInput' "association"
>   loginNo <- loginNumber
>   linkNo <- liftIO $ linkLexemes c origin destination association loginNo
>   case linkNo of
>     Just n  -> redirect $ "/link/" ++ (show n)
>     Nothing -> error "Failed to establish link."

> linkPage :: String -> CGI CGIResult
> linkPage link = do
>   no <- liftIO $ intFromString link
>   case no of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       c <- liftIO db
>       ts <- liftIO $ quickQuery c "SELECT origin, destination, representation FROM link \
>                                   \WHERE link_no = ?" [toSql n]
>                        `catchSqlE` "Failed to retrieve link."
>       case ts of
>         [x@[_,_,_]] -> do
>             let [origin, destination, association] = map fromSql' x
>                 t = origin ++ " -> " ++ destination
>             output $ renderHtml $ page t ["lexeme"]
>               [ thediv ! [identifier "baseline", theclass "link"] <<
>                   linkHtml origin destination,
>                 paragraph ! [identifier "association"] << encodeString association ]
>         _ -> error "Link does not exist or failed to retrieve."

Generate a page of links for the specified member or all members (for Nothing).

> linksPage :: Maybe Integer -> CGI CGIResult
> linksPage memberNo = do
>   c <- liftIO db
>   pg  <- getInputDefault "pg" 1
>   n   <- getInputDefault "n" 25
>   links <- liftIO $ getLinks c memberNo ((pg - 1) * n) (n + 1)
>     `catchSqlE` "Failed to retrieve links."
>   pagerControl <- pager n pg $ (length links) + ((pg - 1) * n)
>   output $ renderHtml $ page "Links" ["lexeme"]
>     [ (take n $ map displayLink links) +++ pagerControl ]

> getLinks :: IConnection conn => conn -> Maybe Integer -> Int -> Int -> IO [[SqlValue]]
> getLinks c memberNo offset limit =
>   case memberNo of
>     Nothing -> quickQuery c "SELECT link_no, origin, destination FROM link \
>                             \ORDER BY link_no OFFSET ? LIMIT ?" $
>                             [toSql offset, toSql limit]
>     Just n  -> quickQuery c "SELECT link_no, origin, destination FROM link \
>                             \WHERE author = ? \
>                             \ORDER BY link_no OFFSET ? LIMIT ?" $
>                             [toSql n, toSql offset, toSql limit]

> displayLink :: [SqlValue] -> Html
> displayLink [no, origin, destination] = 
>   let no' = fromSql no :: Integer
>       origin' = fromSql' origin :: String
>       destination' = fromSql' destination :: String in
>   thediv ! [theclass "link"] << anchor ! [href $ "/link/" ++ (show no')] <<
>     linkHtml origin' destination'
> displayLink _ = thediv ! [theclass "link"] << "Link is malformed."