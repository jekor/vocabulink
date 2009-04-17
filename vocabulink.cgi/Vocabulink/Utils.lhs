\section{Utility Functions}

Here are some functions that aren't specific to Vocabulink, but that don't
exist in any libraries I know of.

> module Vocabulink.Utils (         if', (?), safeHead, currentDay, currentYear,
>                                   basename, translate, formatTime', (<$$>),
>  {- Codec.Binary.UTF8.String -}   encodeString, decodeString,
>  {- Control.Applicative -}        pure, (<$>), (<*>),
>  {- Control.Applicative.Error -}  Failing(..), maybeRead,
>  {- Control.Monad -}              liftM,
>  {- Control.Monad.Trans -}        liftIO, MonadIO,
>  {- Data.Char -}                  toLower,
>  {- Data.Maybe -}                 maybe, fromMaybe, fromJust, isJust, catMaybes,
>  {- Data.Time.Calendar -}         Day,
>  {- Data.Time.Clock -}            UTCTime,
>  {- Data.Time.Format -}           formatTime,
>  {- Data.Time.LocalTime -}        ZonedTime,
>  {- System.Locale -}              defaultTimeLocale, rfc822DateFormat) where

We make extensive use of the |liftM| and the Maybe monad.

> import Codec.Binary.UTF8.String (encodeString, decodeString)
> import Control.Applicative (pure, (<$>), (<*>))
> import Control.Applicative.Error (Failing(..), maybeRead)
> import Control.Monad (liftM)
> import Control.Monad.Trans (liftIO, MonadIO)
> import Data.Char (toLower)
> import Data.Maybe (maybe, fromMaybe, fromJust, isJust, catMaybes)
> import Data.Time.Calendar (Day, toGregorian)
> import Data.Time.Clock (getCurrentTime, UTCTime)
> import Data.Time.Format (formatTime, FormatTime)
> import Data.Time.LocalTime (  getCurrentTimeZone, utcToLocalTime,
>                               LocalTime(..), ZonedTime)
> import System.Locale (defaultTimeLocale, rfc822DateFormat)

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

Return the  current day (in the server's timezone).

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

For files from forms, we can't make assumptions about the path separator.

> basename :: FilePath -> FilePath
> basename = reverse . takeWhile (`notElem` "/\\") . reverse

This is like the Unix tr utility. It takes a list of search/replacements and
then performs them on the list.

> translate :: (Eq a) => [(a, a)] -> [a] -> [a]
> translate sr = map (\s -> maybe s id $ lookup s sr)

> formatTime' :: FormatTime t => String -> t -> String
> formatTime' = formatTime defaultTimeLocale

Often it's handy to be able to lift an operation into 2 monads with little
verbosity. Parsec may have claimed this before me, but |<$$>| just makes too
much sense as 2 |<$>|s.

> (<$$>) :: (Monad m1, Monad m) => (a -> r) -> m (m1 a) -> m (m1 r)
> (<$$>) = liftM . liftM