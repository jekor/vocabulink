> module Vocabulink.Lexeme where

> import Vocabulink.Html (outputHtml, page, Dependency(..))
> import Vocabulink.DB (db, query1, toSql', fromSql')
> import Vocabulink.Link (linkHtml)

> import Codec.Binary.UTF8.String (encodeString)
> import Network.CGI (CGI, CGIResult, liftIO, redirect)
> import Text.XHtml.Strict

When retrieving the page for a lexeme, we first check to see if a lemma for
this lexeme is defined. If not, we assume it to be canonical.

> lexemePage :: String -> CGI CGIResult
> lexemePage l = do
>   c <- liftIO db
>   lemma <- liftIO $ query1 c "SELECT lemma FROM lexeme \
>                              \WHERE lexeme = ?" [toSql' l]
>   case lemma of
>     Just lm -> redirect $ "/lexeme/" ++ encodeString (fromSql' lm)
>     Nothing -> outputHtml $ page (encodeString l) [CSS "lexeme"]
>       [ form ! [action "/link", method "get"] <<
>         [ hidden "origin" (encodeString l),
>           thediv ! [identifier "baseline", theclass "link"] <<
>             linkHtml (stringToHtml l)
>                      (textfield "destination" +++
>                       submit "" "link") ] ]
