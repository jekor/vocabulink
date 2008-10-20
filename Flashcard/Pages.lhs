> module Flashcard.Pages where

> import Text.XHtml.Strict

A common idiom is to use concatHtml for an element's contents.

> infixl 3 <<|
> (<<|) :: (Html -> Html) -> [Html] -> Html
> h <<| l = h << concatHtml l

> newCardPage :: String
> newCardPage =  renderHtml $
>   header << thetitle << "Create a New Card" +++
>   body <<|
>     [ h1 << "Create a New Card"
>     , form ! [action "", method "post"] <<|
>         [ label << "Card name:"
>         , input ! [name "name", thetype "text"]
>         , br
>         , label << "Question/Front/Foreign"
>         , textarea ! [name "question"] << ""
>         , br
>         , label << "Answer/Back/Native"
>         , textarea ! [name "answer"] << ""
>         , br
>         , input ! [thetype "submit"]
>         ]
>     ]