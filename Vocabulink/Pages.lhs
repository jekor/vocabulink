> module Vocabulink.Pages where

> import Vocabulink.Html
> import Vocabulink.Member

> import Network.CGI
> import Network.CGI.Protocol
> import Network.CGI.Monad
> import Text.XHtml.Strict

> testPage :: CGI CGIResult
> testPage = do
>   username <- loginName
>   vars <- getVars
>   inputs <- cgiGet cgiInputs
>   output $ renderHtml $ h1 << ("Hello " ++ username) +++
>     paragraph << (pre << map (\x -> show x ++ "\n") vars) +++
>                  (pre << show inputs)

> newMemberPage :: String
> newMemberPage = renderHtml $
>   header << thetitle << "Join Vocabulink" +++
>   body <<|
>     [ h1 << "Join Vocabulink",
>       form ! [action "", method "post"] <<|
>         [ label << "Username:",
>           input ! [name "username", thetype "text"],
>           br,
>           label << "Password:",
>           input ! [name "password", thetype "password"],
>           br,
>           label << "Email:",
>           input ! [name "email", thetype "text"],
>           br,
>           input ! [thetype "submit", value "Join"]]]

> loginPage :: String
> loginPage = renderHtml $
>   header << thetitle << "Log In" +++
>   body <<|
>     [ h1 << "Log In",
>       form ! [action "", method "post"] <<|
>         [ label << "Username:",
>           input ! [name "username", thetype "text"],
>           br,
>           label << "Password:",
>           input ! [name "password", thetype "password"],
>           br,
>           input ! [thetype "submit", value "Log In"]]]