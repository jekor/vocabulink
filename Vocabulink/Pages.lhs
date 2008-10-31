> module Vocabulink.Pages where

> import Vocabulink.Utils
> import Vocabulink.Member

> import Network.CGI
> import Text.XHtml.Strict

> testPage :: CGI CGIResult
> testPage = do
>   username <- loginName
>   output $ renderHtml $ h1 << ("Hello " ++ username)

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