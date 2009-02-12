\section{Authentication}

Much of what you can do on Vocabulink requires us to know and trust who you say
you are.

> module Vocabulink.Member.Auth (  withMemberNumber, withRequiredMemberNumber,
>                                  loginNumber, setAuthCookie) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Utils

> import Data.ByteString.Char8 (pack)
> import Data.Digest.OpenSSL.HMAC (hmac, sha1)
> import Data.Time.Calendar (addDays, diffDays, showGregorian)
> import Data.Time.Format (parseTime)
> import System.Locale (defaultTimeLocale, iso8601DateFormat)
> import System.Time (TimeDiff(..), getClockTime, addToClockTime, toCalendarTime)
> import Network.FastCGI (CGI)
> import Network.URI (escapeURIString, isUnescapedInURI)
> import Text.ParserCombinators.Parsec (  Parser, parse, manyTill, many1,
>                                         anyChar, char, string)

\subsection{Helpers}

Here are a couple functions that most pages on the site will use for
determining the identity for a member.

|withMemberNumber| accepts a default value (for if the client isn't logged in)
and a function to carry out with the member's number otherwise.

> withMemberNumber :: a -> (Integer -> App a) -> App a
> withMemberNumber d f = asks memberNumber >>= maybe (return d) f

|withRequiredMemberNumber| is like |withMemberNumber|, but it provides a
``logged out default'' of redirecting the client to the login page.

> withRequiredMemberNumber :: (Integer -> App CGIResult) -> App CGIResult
> withRequiredMemberNumber f =  asks memberNumber >>=
>                               maybe (loginRedirectPage >>= redirect) f

When we direct a user to the login page, we want to make sure that they can
find their way back to where they were. To do so, we get the current URI and
append it to the login page in the query string. The login page will know what
to do with it.

> loginRedirectPage :: App String
> loginRedirectPage = do
>   request' <- getVar "REQUEST_URI"
>   let request = fromMaybe "/" request'
>   return $ "/member/login?redirect=" ++ escapeURIString isUnescapedInURI request

\subsection{Creating the Auth Cookie}

Each time a user logs in, we send an authentication cookie to their browser.
The cookie is a digest of some state information. We then use the cookie for
authenticating their identity on subsequent requests.

We don't want to store information like IP address in our database. All of the
session state stays in the cookie. This also keeps us from having to deal with
storing session state on our end.

> data AuthToken = AuthToken {
>   expiry    :: Day,
>   member    :: Integer,
>   ipAddress :: String,
>   digest    :: String
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
>   show a =  "exp="   ++ (showGregorian (expiry a)) ++
>             "&no="   ++ (show (member a)) ++
>             "&ip="   ++ (ipAddress a) ++
>             "&mac="  ++ (digest a)

This creates an AuthToken with the default expiration time, automatically
calculating the digest.

> authToken :: Integer -> String -> IO (AuthToken)
> authToken memberNo ip = do
>   now <- currentDay
>   let expires = addDays cookieShelfLife now
>   digest' <- digestToken $ AuthToken {  expiry     = expires,
>                                         member     = memberNo,
>                                         ipAddress  = ip,
>                                         digest     = "" }
>   return AuthToken {  expiry     = expires,
>                       member     = memberNo,
>                       ipAddress  = ip,
>                       digest     = digest' }

This generates the HMAC digest of the auth token using SHA1.

Eventually, we need to rotate the key used to generate the HMAC, while still
storing old keys long enough to use them for any valid login session. Without
this, authentication is less secure.

TODO: We must replace this key before publishing this document.

> digestToken :: AuthToken -> IO (String)
> digestToken a = hmac sha1 (pack "blahblahblah")
>                   (pack $  (showGregorian (expiry a)) ++
>                            (show (member a)) ++
>                            (ipAddress a))

Setting the cookie is rather simple by this point. We just create the auth
token and send it to the client.

> setAuthCookie :: Integer -> String -> App ()
> setAuthCookie memberNo ip = do
>   authTok <- liftIO $ authToken memberNo ip
>   now <- liftIO getClockTime
>   expires <- liftIO $ toCalendarTime
>                (addToClockTime (TimeDiff {  tdYear     = 0,
>                                             tdMonth    = 0,
>                                             tdDay      = fromIntegral cookieShelfLife,
>                                             tdHour     = 0,
>                                             tdMin      = 0,
>                                             tdSec      = 0,
>                                             tdPicosec  = 0}) now)
>   setCookie Cookie {  cookieName     = "auth",
>                       cookieValue    = (show authTok),
>                       cookieExpires  = Just expires,
>                       cookieDomain   = Just "vocabulink.com",
>                       cookiePath     = Just "/",
>                       cookieSecure   = False }

\subsection{Reading the Auth Cookie}

Each time a request comes in, we check it for an authentication cookie. If
there is one, we verify it and then pack the information from it into the App
monad.

This is in the CGI monad instead of the App monad because it needs to be used
for creating the App monad.

> loginNumber :: CGI (Maybe Integer)
> loginNumber = do
>   authCookie <- getCookie "auth"
>   case authCookie of
>     Nothing  -> return Nothing
>     Just c   -> do
>       ip <- remoteAddr
>       memberNo <- liftIO $ verifyMember c ip
>       return memberNo

To verify an auth token, we verify the token digest, check that the cookie
hasn't expired, and verify the sending IP address against the one in the token.

> verifyMember :: String -> String -> IO (Maybe Integer)
> verifyMember cookie ip =
>   case parseAuthToken cookie of
>     Nothing -> return Nothing
>     Just a  -> do
>       now <- currentDay
>       valid <- verifyTokenDigest a
>       if valid && (diffDays (expiry a) now > 0) && ip == (ipAddress a)
>          then return $ Just (member a)
>          else return Nothing

This is a Parsec parser for auth tokens (as stored in cookies).

> authTokenParser :: Parser (Maybe AuthToken)
> authTokenParser = do
>   string "exp=";  day'      <- manyTill anyChar $ char '&'
>   string "no=";   memberNo  <- manyTill anyChar $ char '&'
>   string "ip=";   ip        <- manyTill anyChar $ char '&'
>   string "mac=";  digest'   <- many1 anyChar
>   let day = parseTime defaultTimeLocale (iso8601DateFormat Nothing) day'
>   return $ day >>= \d -> Just AuthToken {  expiry    = d,
>                                            member    = read memberNo,
>                                            ipAddress = ip,
>                                            digest    = digest' }

It'd be nice to log an error if parsing fails, but we don't have a database
handle.

> parseAuthToken :: String -> Maybe AuthToken
> parseAuthToken s = case parse authTokenParser "" s of
>                      Left _   -> Nothing
>                      Right x  -> x

To verify the token digest (HMAC), we digest it like we did when we created it
and compare it against what we've received.

> verifyTokenDigest :: AuthToken -> IO (Bool)
> verifyTokenDigest a = do
>   digest' <- digestToken a
>   return $ (digest a) == digest'