> module Vocabulink.Lexeme where

> import Vocabulink.Html
> import Vocabulink.DB
> import Vocabulink.Link

> import Codec.Binary.UTF8.String
> import Network.CGI
> import Text.XHtml.Strict

When retrieving the page for a lexeme, we first check to see if a lemma for
this lexeme is defined. If not, we assume it to be canonical.

> lexemePage :: String -> CGI CGIResult
> lexemePage l = do
>   c <- liftIO db
>   lemma <- liftIO $ query1 c "SELECT lemma FROM lexeme \
>                              \WHERE lexeme = ?" [toSql' l]
>   case lemma of
>     Nothing -> outputHtml $ showLexeme l
>     Just lm -> redirect $ "/lexeme/" ++ encodeString (fromSql' lm)

> showLexeme :: String -> Html
> showLexeme l = page (encodeString l) [CSS "lexeme"]
>   [ form ! [action "/link", method "get"] <<|
>      [ input ! [thetype "hidden", name "origin", value (encodeString l)],
>        thediv ! [identifier "baseline", theclass "link"] <<
>          linkHtml l
>                   (input ! [identifier "new-association", thetype "text",
>                             name "destination"] +++
>                    input ! [thetype "submit", value "link"]) ] ]
