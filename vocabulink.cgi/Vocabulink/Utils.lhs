% Copyright 2008, 2009 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\section{Utility Functions}

Here are some functions that aren't specific to Vocabulink, but that don't
exist in any libraries I know of. We also use this module to export some
oft-used functions for other modules.

> module Vocabulink.Utils (         if', (?), safeHead, currentDay, currentYear,
>                                   formatSimpleTime, basename, translate, (<$$>),
>                                   sendMail, every2nd, every3rd,
>                                   memcacheGet, memcacheSet, memcacheDelete,
>                                   memcacheFlush,
>  {- Codec.Binary.UTF8.String -}   encodeString, decodeString,
>  {- Control.Applicative -}        pure, (<$>), (<*>),
>  {- Control.Applicative.Error -}  Failing(..), maybeRead,
>  {- Control.Monad -}              liftM,
>  {- Control.Monad.Trans -}        liftIO, MonadIO,
>  {- Data.Char -}                  toLower,
>  {- Data.Maybe -}                 maybe, fromMaybe, fromJust, isJust, isNothing,
>                                   catMaybes,
>  {- Data.Time.Calendar -}         Day,
>  {- Data.Time.Clock -}            UTCTime,
>  {- Data.Time.Format -}           formatTime,
>  {- Data.Time.LocalTime -}        ZonedTime,
>  {- System.FilePath -}            (</>), takeExtension, addExtension,
>                                   replaceExtension, takeBaseName,
>  {- System.Locale -}              defaultTimeLocale, rfc822DateFormat) where

Vocabulink deals with all strings as UTF8. Every part of the website
potentially makes use of foreign writing systems. Occasionally I also like
using other Unicode characters.

> import Codec.Binary.UTF8.String (encodeString, decodeString)

Applicative was first introduced to Vocabulink for working with Formlets.
However, it seems to be a style that the Haskell community is using more and
more.

> import Control.Applicative (pure, (<$>), (<*>))
> import Control.Applicative.Error (Failing(..), maybeRead)
> import Control.Exception (try, bracket)

We make particularly extensive use of |liftM| and the Maybe monad.

> import Control.Monad (liftM)
> import Control.Monad.Trans (liftIO, MonadIO)
> import Data.Char (toLower)
> import Data.Maybe (maybe, fromMaybe, fromJust, isJust, isNothing, catMaybes)

Time is notoriously difficult to deal with in Haskell. It gets especially
tricky when working with the database and libraries that expect different
formats.

> import Data.Time.Calendar (Day, toGregorian)
> import Data.Time.Clock (getCurrentTime, UTCTime)
> import Data.Time.Format (formatTime)
> import Data.Time.LocalTime (  getCurrentTimeZone, utcToLocalTime,
>                               LocalTime(..), ZonedTime)
> import qualified Network.Memcache.Protocol as Memcached
> import qualified Network.Memcache as Memcache
> import System.Cmd (system)
> import System.FilePath (  (</>), takeExtension, addExtension, takeBaseName,
>                           replaceExtension )
> import System.Locale (defaultTimeLocale, rfc822DateFormat)
> import System.Exit (ExitCode(..))

It's often useful to have the compactness of the traditional tertiary operator
rather than an if then else. The |(?)| operator can be used like:

\begin{quote}|Bool ? trueExpression $ falseExpression|\end{quote}

I think I originally saw this on the Haskell wiki.

> infixl 1 ?
> (?)  :: Bool -> a -> a -> a
> (?)  = if'

> if' :: Bool -> a -> a -> a
> if' True   x  _  = x
> if' False  _  y  = y

\subsection{Time}

Return the current day. I'm not sure that this is useful on its own.

> currentDay :: IO Day
> currentDay = do
>   now  <- getCurrentTime
>   tz   <- getCurrentTimeZone
>   let (LocalTime day _) = utcToLocalTime tz now
>   return day

Return the current year as a 4-digit number.

> currentYear :: IO Integer
> currentYear = do
>   day <- currentDay
>   let (year, _, _) = toGregorian day
>   return year

Displaying a time is a common enough task.

> formatSimpleTime :: UTCTime -> String
> formatSimpleTime = formatTime defaultTimeLocale "%a %b %d, %Y %R"

For files we receive via HTTP, we can't make assumptions about the path
separator.

> basename :: FilePath -> FilePath
> basename = reverse . takeWhile (`notElem` "/\\") . reverse

This is like the Unix tr utility. It takes a list of search/replacements and
then performs them on the list.

> translate :: (Eq a) => [(a, a)] -> [a] -> [a]
> translate sr = map (\s -> maybe s id $ lookup s sr)

Often it's handy to be able to lift an operation into 2 monads with little
verbosity. Parsec may have claimed this operator name before me, but |<$$>|
just makes too much sense as 2 |<$>|s.

> (<$$>) :: (Monad m1, Monad m) => (a -> r) -> m (m1 a) -> m (m1 r)
> (<$$>) = liftM . liftM

Sending mail is pretty easy. We just deliver it to a local MTA. Even if we have
no MTA running locally, there are sendmail emulators that will handle the SMTP
forwarding for us so that we don't have to deal with SMTP here.

> sendMail :: String -> String -> String -> IO (Maybe ())
> sendMail to subject body = do
>   let body' = unlines [  "To: <" ++ to ++ ">",
>                          "Subject: " ++ subject,
>                          "",
>                          body ]
>   res <- try $ system $
>            (  "export MAILUSER=vocabulink; \
>               \export MAILHOST=vocabulink.com; \
>               \export MAILNAME=Vocabulink; \
>               \echo -e \""   ++ body'  ++ "\" | \
>               \sendmail \""  ++ to     ++ "\"" )
>   case res of
>     Right ExitSuccess  -> return $ Just ()
>     _                  -> return Nothing

\subsection{Memcache}

For now, we don't use memcache for anything more than page-level caching. As
such, we only need to connect, issue a command, and then disconnect. In the
future, if we use it more we could add a memcache server to the App monad.

> memcacheServer :: IO Memcached.Server
> memcacheServer = Memcached.connect "127.0.0.1" 11211

> withMemcache :: (Memcached.Server -> IO a) -> IO a
> withMemcache f = do
>   bracket  (memcacheServer)
>            (Memcached.disconnect)
>            f

> memcacheGet :: String -> IO (Maybe String)
> memcacheGet key = withMemcache $ \mc -> decodeString <$$> Memcache.get mc key

> memcacheSet :: String -> String -> IO (Bool)
> memcacheSet key value = withMemcache $ \mc ->
>   Memcache.set mc key (encodeString value)

> memcacheDelete :: String -> IO (Bool)
> memcacheDelete key = withMemcache $ \mc -> Memcache.delete mc key 0

While out dataset and traffic is still small, the easiest thing to do when
something is updated is to just flush the entire cache rather than try and
figure out what exactly we need to invalidate. It's a brute force approach but
it's better than no cache at all.

> memcacheFlush :: IO ()
> memcacheFlush = withMemcache $ \mc -> Memcache.flush mc

\subsection{Lists}

In case we want don't want our program to crash when taking the head of the
empty list, we need to provide a default:

> safeHead :: a -> [a] -> a
> safeHead d []     = d
> safeHead _ (x:_)  = x

If we want to layout items from left to right in HTML columns, we need to break
1 list down into smaller lists. |everyNth| is not a great name, but |cycleN| is
equally confusing. These use a neat |foldr| trick I found on the Haskell wiki.

every2nd [1,2,3] =>
3 ([],[]) => ([3],[])
2 ([3],[]) => ([2],[3])
1 ([2],[3]) => ([1,3],[2])

> every2nd :: [a] -> ([a], [a])
> every2nd l = foldr (\a ~(x,y) -> (a:y,x)) ([],[]) l

every3rd [1,2,3,4,5] =>
5 ([],[],[]) => ([5],[],[])
4 ([5],[],[]) => ([4],[5],[])
3 ([4],[5],[]) => ([3],[4],[5])
2 ([3],[4],[5]) => ([2,5],[3],[4])
1 ([2,5],[3],[4]) => ([1,4],[2,5],[3])

> every3rd :: [a] -> ([a], [a], [a])
> every3rd l = foldr (\a ~(x,y,z) -> (a:z,x,y)) ([],[],[]) l
