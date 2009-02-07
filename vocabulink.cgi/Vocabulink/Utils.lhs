\section{Utility Functions}

Here are some functions that aren't specific to Vocabulink, but that don't
exist in any libraries I know of.

> module Vocabulink.Utils (if', (?), intFromString) where

> import Control.Exception (try)

It's often useful to have the compactness of the traditional tertiary operator
rather than an if then else. The |(?)| operator can be used like:

\begin{quote}|Bool ? trueExpression $ falseExpression|\end{quote}

> infixl 1 ?
> (?)  :: Bool -> a -> a -> a
> (?)  = if'

> if' :: Bool -> a -> a -> a
> if' True   x  _  = x
> if' False  _  y  = y

Since we're handling CGI requests, all parameters arrive as strings. We need to
safely convert them to the type we need without throwing an exception or
halting the program. Our solution is simple: attempt the conversion and return
a Maybe value. Then the function requesting the value can decide what to do in
case of failure.

Integers are a simple case where we can just use the Haskell reader (since
integer syntax is basic).

> intFromString :: String -> IO (Maybe Integer)
> intFromString s = do n <- try $ readIO s
>                      case n of
>                        Left _   -> return Nothing
>                        Right n' -> return $ Just n'