> module Vocabulink.Member.Auth (loginNumber, setAuthCookie) where

> import Vocabulink.App (App)

> import Data.ByteString.Char8 (pack)
> import Data.Digest.OpenSSL.HMAC (hmac, sha1)
> import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixDayLength)
> import Network.FastCGI
> import qualified Text.ParserCombinators.Parsec as P

> loginNumber :: CGI (Maybe Integer)
> loginNumber = do
>   authCookie <- getCookie "auth"
>   case authCookie of
>     Nothing -> return Nothing
>     Just c  -> do
>       ip <- remoteAddr
>       memberNo <- liftIO $ verifyMember c ip
>       return memberNo

> verifyMember :: String -> String -> IO (Maybe Integer)
> verifyMember cookie ip =
>   case parseAuthToken cookie of
>     Nothing -> return Nothing
>     Just a  -> do
>       utc <- getPOSIXTime
>       valid <- verifyTokenDigest a
>       if valid && ip == (ipAddress a) && (not $ expired utc (expiry a))
>          then return $ Just (member a)
>          else return Nothing

Each time a user logs in, we send an authentication cookie to their browser.
The cookie is an digest of some state information we store in our database
(expiration time, IP address). We then use the cookie for authenticating their
identity on subsequent requests.

We don't want to store information like IP address for a user. All of the
session state stays in the cookie. This also keeps us from having to deal with
storing session state on our end.

> data AuthToken = AuthToken {
>   expiry    :: POSIXTime,
>   member    :: Integer,
>   ipAddress :: String,
>   digest    :: String
> }

> instance Show AuthToken where
>     show a = "exp=" ++ (show (floor (expiry a) :: Integer)) ++ "&no=" ++ (show (member a)) ++
>              "&ip=" ++ (ipAddress a) ++ "&mac=" ++ (digest a)

This really needs some cleanup.

> authTokenParser :: P.Parser AuthToken
> authTokenParser = do
>   P.string "exp="
>   seconds <- P.manyTill P.anyChar $ P.char '&'
>   P.string "no="
>   member_no <- P.manyTill P.anyChar $ P.char '&'
>   P.string "ip="
>   ip <- P.manyTill P.anyChar $ P.char '&'
>   P.string "mac="
>   digest' <- P.many1 P.anyChar
>   return AuthToken { expiry    = fromInteger (read seconds) :: POSIXTime,
>                      member    = read member_no,
>                      ipAddress = ip,
>                      digest    = digest' }

> parseAuthToken :: String -> Maybe AuthToken
> parseAuthToken s = case P.parse authTokenParser "" s of
>                    Left e  -> error (show e)
>                    Right x -> Just x

Create an AuthToken with the default expiration time, automatically calculating
the digest.

> authToken :: Integer -> String -> IO (AuthToken)
> authToken member_no ip = do
>   utc <- getPOSIXTime
>   let expires = utc + posixDayLength
>   -- TODO: Use a proper key.
>   digest' <- digestToken $ AuthToken { expiry    = expires,
>                                        member    = member_no,
>                                        ipAddress = ip,
>                                        digest    = "" }
>   return AuthToken { expiry    = expires,
>                      member    = member_no,
>                      ipAddress = ip,
>                      digest    = digest' }

Eventually, we need to rotate the key used to generate the HMAC, while still
storing old keys long enough to use them for any valid login session. Without
this, authentication is less secure.

> digestToken :: AuthToken -> IO (String)
> digestToken a = hmac sha1 (pack "blahblahblah")
>                      (pack $ (show (floor (expiry a) :: Integer)) ++ (show (member a)) ++ (ipAddress a))

> verifyTokenDigest :: AuthToken -> IO (Bool)
> verifyTokenDigest a = do
>   digest' <- digestToken a
>   return ((digest a) == digest')

Returns the member number or Nothing if the cookie is invalid or expired.

> expired :: POSIXTime -> POSIXTime -> Bool
> expired now ex = (floor ex :: Integer) - (floor now :: Integer) < 0

> setAuthCookie :: Integer -> String -> App ()
> setAuthCookie memberNo ip = do
>   authTok <- liftIO $ authToken memberNo ip
>   setCookie Cookie { cookieName    = "auth",
>                      cookieValue   = (show authTok),
>                      cookieExpires = Nothing,
>                      cookieDomain  = Just "vocabulink.com",
>                      cookiePath    = Just "/",
>                      cookieSecure  = False }
