\section{Authentication Tokens}

Much of what you can do on Vocabulink requires us to know and trust who you say
you are (or at least can be enhanced by that knowledge).

> module Vocabulink.Member.AuthToken (  AuthToken(..), verifiedAuthToken,
>                                       setAuthCookie, deleteAuthCookie) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Utils

> import Data.ByteString.Char8 (pack)
> import Data.Digest.OpenSSL.HMAC (hmac, sha1)
> import Data.Time.Calendar (addDays, diffDays, showGregorian)
> import Data.Time.Format (parseTime)
> import System.Locale (defaultTimeLocale, iso8601DateFormat)
> import System.Time (TimeDiff(..), getClockTime, addToClockTime, toCalendarTime)
> import Network.URI (escapeURIString, unEscapeString, isUnescapedInURI)
> import Text.ParserCombinators.Parsec (  Parser, parse, manyTill, many1,
>                                         anyChar, char, string)

\subsection{Creating the Auth Token}

Each time a user logs in, we send an authentication cookie to their browser.
The cookie is a digest of some state information. We then use the cookie for
authenticating their identity on subsequent requests.

We don't want to store information like IP address in our database. All of the
session state stays in the cookie. This also keeps us from having to deal with
storing session state on our end.

> data AuthToken = AuthToken {
>   authExpiry     :: Day,
>   authMemberNo   :: Integer,
>   authUsername   :: String,
>   authIPAddress  :: String,
>   authDigest     :: String
> }

We give the token 30 days to expire. We don't want it expiring too soon because
it becomes bothersome to keep logging in. However, we don't want members to
stay logged in forever, and using a non-expiring cookie would require us to
check the database for each authentication to see if we need to de-authenticate
the client.

> cookieShelfLife :: Integer
> cookieShelfLife = 30

Here is the format of the actual cookie we send to the client.

> instance Show AuthToken where
>   show a =  "exp="    ++ showGregorian (authExpiry a) ++
>             "&no="    ++ show (authMemberNo a) ++
>             "&name="  ++ escapeURIString isUnescapedInURI
>                            (encodeString $ authUsername a) ++
>             "&ip="    ++ authIPAddress a ++
>             "&mac="   ++ authDigest a

This creates an AuthToken with the default expiration time, automatically
calculating the digest.

> authToken :: Integer -> String -> String -> String -> IO (AuthToken)
> authToken memberNo username ip salt = do
>   now <- currentDay
>   let expires = addDays cookieShelfLife now
>   digest <- tokenDigest (AuthToken {  authExpiry     = expires,
>                                       authMemberNo   = memberNo,
>                                       authUsername   = username,
>                                       authIPAddress  = ip,
>                                       authDigest     = "" }) salt
>   return AuthToken {  authExpiry     = expires,
>                       authMemberNo   = memberNo,
>                       authUsername   = username,
>                       authIPAddress  = ip,
>                       authDigest     = digest }

This generates the HMAC digest of the auth token using SHA1.

Eventually, we need to rotate the key used to generate the HMAC, while still
storing old keys long enough to use them for any valid login session. Without
this, authentication is less secure.

> tokenDigest :: AuthToken -> String -> IO (String)
> tokenDigest a salt = hmac sha1 (pack salt) (pack token)
>   where token =  showGregorian (authExpiry a) ++
>                  show (authMemberNo a) ++
>                  encodeString (authUsername a) ++
>                  authIPAddress a

Setting the cookie is rather simple by this point. We just create the auth
token and send it to the client.

> setAuthCookie :: Integer -> String -> String -> App ()
> setAuthCookie memberNo username ip = do
>   salt <- fromJust <$> getOption "authtokensalt"
>   authTok <- liftIO $ authToken memberNo username ip salt
>   now <- liftIO getClockTime
>   expires <- liftIO $ toCalendarTime
>                (addToClockTime (TimeDiff {  tdYear     = 0,
>                                             tdMonth    = 0,
>                                             tdDay      = fromIntegral cookieShelfLife,
>                                             tdHour     = 0,
>                                             tdMin      = 0,
>                                             tdSec      = 0,
>                                             tdPicosec  = 0 }) now)
>   setCookie Cookie {  cookieName     = "auth",
>                       cookieValue    = show authTok,
>                       cookieExpires  = Just expires,
>                       cookieDomain   = Just "vocabulink.com",
>                       cookiePath     = Just "/",
>                       cookieSecure   = False }

> deleteAuthCookie :: MonadCGI m => m ()
> deleteAuthCookie =
>   deleteCookie Cookie {  cookieName   = "auth",
>                          cookieDomain = Just "vocabulink.com",
>                          cookiePath   = Just "/",
>                          -- The following are only here to get rid of GHC warnings.
>                          cookieValue  = "",
>                          cookieExpires = Nothing,
>                          cookieSecure = False }


\subsection{Reading the Auth Token}

This retrieves the auth token from the HTTP request, verifies it, and if valid,
returns it. To verify an auth token, we verify the token digest, check that the
cookie hasn't expired, and verify the sending IP address against the one in the
token.

> verifiedAuthToken :: (MonadCGI m, MonadIO m) => String -> m (Maybe AuthToken)
> verifiedAuthToken salt = do
>   cookie <- getCookie "auth"
>   ip <- remoteAddr
>   case parseAuthToken =<< cookie of
>     Nothing  -> return Nothing
>     Just a   -> do
>       now <- liftIO currentDay
>       digest <- liftIO $ tokenDigest a salt
>       if digest == authDigest a && diffDays (authExpiry a) now > 0 &&
>          ip == (authIPAddress a)
>          then return $ Just a
>          else return Nothing

This is a Parsec parser for auth tokens (as stored in cookies).

> authTokenParser :: Parser (Maybe AuthToken)
> authTokenParser = do
>   string "exp=";   day'      <- manyTill anyChar $ char '&'
>   string "no=";    memberNo  <- manyTill anyChar $ char '&'
>   string "name=";  username  <- manyTill anyChar $ char '&'
>   string "ip=";    ip        <- manyTill anyChar $ char '&'
>   string "mac=";   digest    <- many1 anyChar
>   let day = parseTime defaultTimeLocale (iso8601DateFormat Nothing) day'
>   return $ day >>= \d -> Just AuthToken {  authExpiry     = d,
>                                            authMemberNo   = read memberNo,
>                                            authUsername   = decodeString $ unEscapeString username,
>                                            authIPAddress  = ip,
>                                            authDigest     = digest }

It'd be nice to log an error if parsing fails, but we don't have a database
handle.

> parseAuthToken :: String -> Maybe AuthToken
> parseAuthToken s = case parse authTokenParser "" s of
>                      Left   _  -> Nothing
>                      Right  x  -> x