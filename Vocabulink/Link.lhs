> module Vocabulink.Link where

> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Member
> import Vocabulink.Utils

> import Codec.Binary.UTF8.String
> import Database.HDBC
> import Network.CGI
> import Text.XHtml.Strict

> newLinkPage :: CGI CGIResult
> newLinkPage = do
>   origin <- getInput' "origin"
>   destination <- getInput' "destination"
>   output $ renderHtml $
>     header <<|
>       [ thetitle << ((encodeString origin) ++ " -> " ++ (encodeString destination)),
>         thelink ! [href "http://s.vocabulink.com/lexeme.css",
>                    rel "stylesheet",
>                    thetype "text/css"] << noHtml ] +++
>     body <<|
>       [ form ! [action "", method "post"] <<|
>          [ paragraph ! [identifier "baseline"] <<|
>              [ thespan ! [theclass "lexeme"] << encodeString origin,
>                image ! [src "http://s.vocabulink.com/black.png", width "20%",
>                         height "1"],
>                thespan ! [theclass "lexeme"] << encodeString destination ],
>            paragraph ! [identifier "association"] <<|
>              [ textarea ! [name "association", cols "80", rows "20"] <<
>                  "Describe the association here.",
>                br,
>                input ! [thetype "submit", value "Associate"] ] ] ]

> linkLexemes :: String -> String -> String -> Integer -> IO (Maybe Integer)
> linkLexemes origin destination association n = do
>   quickInsertSeqNo "INSERT INTO link (origin, destination, link_type, \
>                                      \lingvo, representation, author) \
>                    \VALUES (?, ?, 'association', 'en', ?, ?)"
>                    [toSql' origin, toSql' destination, toSql' association, toSql n]
>                    "link_link_no_seq"
>       `catchSql` (\e -> logSqlError e >> error "Failed to establish link.")

> linkLexemes' :: CGI CGIResult
> linkLexemes' = do
>   origin <- getInput' "origin"
>   destination <- getInput' "destination"
>   association <- getInput' "association"
>   loginNo <- loginNumber
>   linkNo <- liftIO $ linkLexemes origin destination association loginNo
>   case linkNo of
>     Just n  -> redirect $ "/link/" ++ (show n)
>     Nothing -> error "Failed to establish link."

> linkPage :: Integer -> CGI CGIResult
> linkPage n = do
>   conn <- liftIO $ db
>   ts <- liftIO $ quickQuery conn "SELECT origin, destination, representation FROM link \
>                                  \WHERE link_no = ?" [toSql n]
>                    `catchSql` (\e -> logSqlError e >> error "Failed to retrieve link.")
>   case ts of
>     [x@[_,_,_]] -> do
>         let [origin, destination, association] = map fromSql' x
>         output $ renderHtml $
>           header <<|
>             [ thetitle << ((encodeString origin) ++ " -> " ++ (encodeString destination)),
>               thelink ! [href "http://s.vocabulink.com/lexeme.css",
>                          rel "stylesheet",
>                          thetype "text/css"] << noHtml ] +++
>           body <<|
>             [ thediv <<|
>                [ paragraph ! [identifier "baseline"] <<|
>                    [ thespan ! [theclass "lexeme"] << encodeString origin,
>                      image ! [src "http://s.vocabulink.com/black.png", width "20%",
>                               height "1"],
>                      thespan ! [theclass "lexeme"] << encodeString destination ],
>                  paragraph ! [identifier "association"] << encodeString association ] ]
>     _ -> error "Link does not exist or failed to retrieve."
