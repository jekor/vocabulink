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

\section{Authentication Tokens}

Much of what you can do on Vocabulink requires us to know and trust who you say
you are.

> module Vocabulink.Member.AuthToken (  AuthToken(..), verifiedAuthToken,
>                                       setAuthCookie, deleteAuthCookie) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Utils hiding ((<$$>))

> import Data.ByteString.Char8 (pack)
> import Data.Digest.OpenSSL.HMAC (hmac, sha1)
> import Data.Time.Calendar (addDays, diffDays, showGregorian)
> import Data.Time.Format (parseTime)
> import System.Locale (defaultTimeLocale, iso8601DateFormat)
> import System.Time (TimeDiff(..), getClockTime, addToClockTime, toCalendarTime)
> import Network.Gravatar (gravatar)
> import Network.URI (escapeURIString, unEscapeString, isUnescapedInURI)
> import Text.ParserCombinators.Parsec (  Parser, parse, noneOf, many1, try,
>                                         char, string, optional)
> import Text.ParserCombinators.Parsec.Perm ((<$$>), (<||>), (<|?>), permute)
> import Text.Regex.Posix ((=~))

\subsection{Creating the Auth Token}

Each time a member logs in, we send an authentication cookie to their browser.
The cookie is a digest of some state information. We then use the cookie for
authenticating their identity on subsequent requests.

We don't want to associate personally-identifying information like IP addresses
to member names in our database. However, we do need to know which IP address
the member logged in from so that we can protect against request spoofing. The
solution is to store all of the session state stays in the cookie. This also
means we don't have to deal with storing session state on our end.

Storing the member's username and gravatar hash are not necessary for
authentication, but it's useful to have them here for client-side code to use
for generating dynamic UI elements (such as comment boxes). We store the
gravatar hash instead of the member's email address for privacy/security.

> data AuthToken = AuthToken {
>   authExpiry     :: Day,
>   authMemberNo   :: Integer,
>   authUsername   :: String,
>   authGravatar   :: Maybe String,
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
>             maybe "" ("&grav=" ++) (authGravatar a) ++
>             "&ip="    ++ authIPAddress a ++
>             "&mac="   ++ authDigest a

This creates an AuthToken with the default expiration time, automatically
calculating the digest.

> authToken :: Integer -> String -> Maybe String -> String -> String -> IO (AuthToken)
> authToken memberNo username email ip key = do
>   now <- currentDay
>   let expires = addDays cookieShelfLife now
>   digest <- tokenDigest AuthToken {  authExpiry     = expires,
>                                      authMemberNo   = memberNo,
>                                      authUsername   = username,
>                                      authGravatar   = gravatarHash =<< email,
>                                      authIPAddress  = ip,
>                                      authDigest     = "" } key
>   return AuthToken {  authExpiry     = expires,
>                       authMemberNo   = memberNo,
>                       authUsername   = username,
>                       authGravatar   = gravatarHash =<< email,
>                       authIPAddress  = ip,
>                       authDigest     = digest }

This generates the HMAC digest of the auth token using SHA1.

Eventually, we need to rotate the key used to generate the HMAC, while still
storing old keys long enough to use them for any valid login session. Without
this, authentication is less secure.

> tokenDigest :: AuthToken -> String -> IO (String)
> tokenDigest a key = hmac sha1 (pack key) (pack token)
>   where token =  showGregorian (authExpiry a) ++
>                  show (authMemberNo a) ++
>                  encodeString (authUsername a) ++
>                  maybe "" ("&grav=" ++) (authGravatar a) ++
>                  authIPAddress a

Setting the cookie is rather simple by this point. We just create the auth
token and send it to the client.

> setAuthCookie :: Integer -> String -> Maybe String -> String -> App ()
> setAuthCookie memberNo username email ip = do
>   key <- fromJust <$> getOption "authtokenkey"
>   authTok <- liftIO $ authToken memberNo username email ip key
>   now <- liftIO getClockTime
>   expires <- liftIO $ toCalendarTime
>                (addToClockTime TimeDiff {  tdYear     = 0,
>                                            tdMonth    = 0,
>                                            tdDay      = fromIntegral cookieShelfLife,
>                                            tdHour     = 0,
>                                            tdMin      = 0,
>                                            tdSec      = 0,
>                                            tdPicosec  = 0 } now)
>   setCookie Cookie {  cookieName     = "auth",
>                       cookieValue    = show authTok,
>                       cookieExpires  = Just expires,
>                       cookieDomain   = Just "www.vocabulink.com",
>                       cookiePath     = Just "/",
>                       cookieSecure   = False }

> deleteAuthCookie :: MonadCGI m => m ()
> deleteAuthCookie = do
>   deleteCookie Cookie {  cookieName   = "auth",
>                          cookieDomain = Just "www.vocabulink.com",
>                          cookiePath   = Just "/",
>                          -- The following are only here to get rid of GHC warnings.
>                          cookieValue  = "",
>                          cookieExpires = Nothing,
>                          cookieSecure = False }
>   -- For the next 30 days, we need to support old cookies
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
cookie hasn't expired, and check the requestor's IP address against the one in
the token.

> verifiedAuthToken :: (MonadCGI m, MonadIO m) => String -> m (Maybe AuthToken)
> verifiedAuthToken key = do
>   cookie <- getCookie "auth"
>   ip <- remoteAddr
>   case parseAuthToken =<< cookie of
>     Nothing  -> return Nothing
>     Just a   -> do
>       now <- liftIO currentDay
>       digest <- liftIO $ tokenDigest a key
>       if digest == authDigest a && diffDays (authExpiry a) now > 0 &&
>          ip == authIPAddress a
>          then return $ Just a
>          else return Nothing

This is a Parsec parser for auth tokens (as stored in cookies). An auth token looks like:
exp=2009-12-01&no=1&name=jekor&ip=127.0.0.1&mac=d0170bc011b6260a1de7596bad1cd4de

> authTokenParser :: Parser AuthToken
> authTokenParser = permute
>   (mkAuthToken  <$$>  authFrag "exp"
>                 <||>  authFrag "no"
>                 <||>  authFrag "name"
>                 <|?>  (Nothing, Just <$> authFrag "grav")
>                 <||>  authFrag "ip"
>                 <||>  authFrag "mac")
>  where authFrag key = do
>          try $ string (key ++ "=")
>          frag <- many1 $ noneOf "&"
>          optional $ char '&'
>          return frag
>        mkAuthToken exp' no name grav ip mac =
>          let day = parseTime defaultTimeLocale (iso8601DateFormat Nothing) exp' in
>          case day of
>            Nothing  -> error "Failed to parse expiration time"
>            Just d   -> AuthToken {  authExpiry     = d,
>                                     authMemberNo   = read no,
>                                     authUsername   = decodeString $
>                                                      unEscapeString name,
>                                     authGravatar   = grav,
>                                     authIPAddress  = ip,
>                                     authDigest     = mac }

It'd be nice to log an error if parsing fails, but we don't have a database
handle.

> parseAuthToken :: String -> Maybe AuthToken
> parseAuthToken s = case parse authTokenParser "" s of
>                      Left   _  -> Nothing
>                      Right  x  -> Just x

The gravatar library will generate the entire URL, but sometimes we just need
the hash. Rather than implement the hashing ourselves, we'll dissect the one we
receive from the gravatar library.

> gravatarHash :: String -> Maybe String
> gravatarHash email =
>   let url = gravatar email
>       (_, _, _, matches) = url =~ "gravatar_id=([0-9a-f]+)" :: (String, String, String, [String]) in
>   case matches of
>     [hash]  -> Just hash
>     _       -> Nothing
