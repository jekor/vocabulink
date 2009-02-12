\section{Utility Functions}

Here are some functions that aren't specific to Vocabulink, but that don't
exist in any libraries I know of.

maybeRead is borrowed from Network.CGI.Protocol until it makes its way into
Haskell some other way.

> module Vocabulink.Utils (        if', (?), safeHead, translate,
>                                  currentDay, currentYear,
>  {- Network.CGI.Protocol -}      maybeRead,
>  {- Control.Monad -}             liftM,
>  {- Control.Monad.Trans -}       liftIO,
>  {- Data.Maybe -}                maybe, fromMaybe, fromJust, isJust,
>  {- Codec.Binary.UTF8.String -}  encodeString, decodeString,
>  {- Data.Time.Calendar -}        Day) where

We make extensive use of the |liftM| and the Maybe monad.

> import Codec.Binary.UTF8.String (encodeString, decodeString)
> import Control.Monad (liftM)
> import Control.Monad.Trans (liftIO)
> import Data.Maybe (maybe, fromMaybe, fromJust, isJust)
> import Data.Time.Calendar (Day, toGregorian)
> import Data.Time.Clock (getCurrentTime)
> import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, LocalTime(..))
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
empty list, we need to provide a default:

> safeHead :: a -> [a] -> a
> safeHead d []     = d
> safeHead _ (x:_)  = x

This is like the Unix tr utility. It takes a list of search/replacements and
then performs them on the list.

> translate :: (Eq a) => [(a, a)] -> [a] -> [a]
> translate sr = map (\s -> maybe s id $ lookup s sr)

> currentDay :: IO Day
> currentDay = do
>   now  <- getCurrentTime
>   tz   <- getCurrentTimeZone
>   let (LocalTime day _) = utcToLocalTime tz now
>   return day

Return the current year (in the server's timezone) as a 4-digit number.

> currentYear :: IO Integer
> currentYear = do
>   day <- currentDay
>   let (year, _, _) = toGregorian day
>   return year