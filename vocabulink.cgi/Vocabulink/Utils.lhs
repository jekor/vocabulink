\section{Utility Functions}

Here are some functions that aren't specific to Vocabulink, but that don't
exist in any libraries I know of.

maybeRead is borrowed from Network.CGI.Protocol until it makes its way into
Haskell some other way.

> module Vocabulink.Utils (    if', (?), safeHead,
>  {- Network.CGI.Protocol -}  maybeRead) where

> import Network.CGI.Protocol (maybeRead)

It's often useful to have the compactness of the traditional tertiary operator
rather than an if then else. The |(?)| operator can be used like:

\begin{quote}|Bool ? trueExpression $ falseExpression|\end{quote}

> infixl 1 ?
> (?)  :: Bool -> a -> a -> a
> (?)  = if'

> if' :: Bool -> a -> a -> a
> if' True   x  _  = x
> if' False  _  y  = y

In case we want don't want our program to crash when taking the head of the
empty list:

> safeHead :: [a] -> Maybe a
> safeHead []    = Nothing
> safeHead (x:_) = Just x