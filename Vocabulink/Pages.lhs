> module Vocabulink.Pages where

> import Text.XHtml.Strict
> import Codec.Binary.UTF8.String

A common idiom is to use concatHtml for an element's contents.

> infixl 3 <<|
> (<<|) :: (Html -> Html) -> [Html] -> Html
> h <<| l = h << concatHtml l

José

> testPage :: String
> testPage =  renderHtml $ h1 << encodeString "日本語"

> newCardPage :: String
> newCardPage =  renderHtml $
>   header << thetitle << "Create a New Card" +++
>   body <<|
>     [ h1 << "Create a New Card",
>       form ! [action "", method "post"] <<|
>         [ label << "Question/Front/Foreign:",
>           textarea ! [name "question"] << "",
>           br,
>           label << "Answer/Back/Native:",
>           textarea ! [name "answer"] << "",
>           br,
>           input ! [thetype "submit", value "Create"]]]

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