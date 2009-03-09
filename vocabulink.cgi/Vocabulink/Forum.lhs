\section{Forum}

Vocabulink uses a custom forums implementation. It does so for a couple reasons:

\begin{enumerate}

\item We want maximum integration and control as the forums are an integral
part of the site, not just an afterthought (they are here to direct the
evolution of the site).

\item Much of the forum logic is based on comments, which is common to the
forums, articles, and most importantly, links. Once we have comment threads,
the forums are almost free.

\end{enumerate}

> module Vocabulink.Forum (forumsPage) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Html

> import qualified Text.XHtml.Strict.Formlets as F

The Forums page is a high-level look into Vocabulink's forums. Each forum is
part of a group of like forums. We have a placeholder for an eventual search
function which will search through the text of all comments in all forums.
Also, we include an administrative interface for creating new groups.

> forumsPage :: App CGIResult
> forumsPage = do
>   res <- runForm ("Name" `formLabel` F.input Nothing) "Create"
>   memberName <- asks appMemberName
>   case (res, memberName) of
>     (Right s, Just "jekor")  -> createNewForumGroup s
>     (Left xhtml, _)          -> do
>       simplePage "Forums" [CSS "forum", JS "MochiKit", JS "forum", JS "form"]
>         [  form ! [action "/forum/threads", method "POST"] <<
>              [  textfield "containing", submit "" "Search" ],
>            if memberName == Just "jekor"
>              then button ! [theclass "reveal forum-group-creator"] <<
>                     "New Forum Group" +++
>                   thediv ! [  identifier "forum-group-creator",
>                               theclass "forum-group",
>                               thestyle "display: none" ] <<
>                     xhtml
>              else noHtml ]
>     _                        -> outputError 403 "Access Denied" []

Create a forum group. For now, just a stub.

> createNewForumGroup :: String -> App CGIResult
> createNewForumGroup _ = redirect =<< referrerOrVocabulink
