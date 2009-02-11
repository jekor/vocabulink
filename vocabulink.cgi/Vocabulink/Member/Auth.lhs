\section{Authentication}

Much of what you can do on Vocabulink requires that you have a unique identity
to us and that we can trust who you say you are.

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
> import Network.FastCGI (CGI)
> import Network.URI (escapeURIString, isUnescapedInURI)
> import qualified Text.ParserCombinators.Parsec as P

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

> loginRedirectPage :: App String
> loginRedirectPage = do
>   request <- getVar "REQUEST_URI"
>   let request' = fromMaybe "/" request
>   return $ "/member/login?redirect=" ++ escapeURIString isUnescapedInURI request'

\subsection{Creating the Auth Cookie}

Each time a user logs in, we send an authentication cookie to their browser.
The cookie is a digest of some state information. We then use the cookie for
authenticating their identity on subsequent requests.

We don't want to store information like IP address for a user. All of the
session state stays in the cookie. This also keeps us from having to deal with
storing session state on our end.

> data AuthToken = AuthToken {
>   expiry    :: Day,
>   member    :: Integer,
>   ipAddress :: String,
>   digest    :: String
> }

Here is the format of the actual cookie we send to the client.

> instance Show AuthToken where
>   show a =  "exp="   ++ (showGregorian (expiry a)) ++
>             "&no="   ++ (show (member a)) ++
>             "&ip="   ++ (ipAddress a) ++
>             "&mac="  ++ (digest a)

This creats an AuthToken with the default expiration time, automatically
calculating the digest. We give the token 30 days to expire. We don't want it
expiring too soon because it becomes bothersome to keep logging in. However, we
don't want members to stay logged in forever, and using a non-expiring cookie
would require us to check the database for each authentication to see if we
need to de-authenticate the client.

> authToken :: Integer -> String -> IO (AuthToken)
> authToken memberNo ip = do
>   now <- currentDay
>   let expires = addDays 30 now
>   digest' <- digestToken $ AuthToken { expiry    = expires,
>                                        member    = memberNo,
>                                        ipAddress = ip,
>                                        digest    = "" }
>   return AuthToken { expiry    = expires,
>                      member    = memberNo,
>                      ipAddress = ip,
>                      digest    = digest' }

This generates the HMAC digest of the auth token using SHA1.

Eventually, we need to rotate the key used to generate the HMAC, while still
storing old keys long enough to use them for any valid login session. Without
this, authentication is less secure.

TODO: We must replace this key before publishing the file.

> digestToken :: AuthToken -> IO (String)
> digestToken a = hmac sha1 (pack "blahblahblah")
>                   (pack $  (showGregorian (expiry a)) ++
>                            (show (member a)) ++
>                            (ipAddress a))

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
hasn't expired, and verify the sending IP against the IP in the token.

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

This really needs some cleanup.

> authTokenParser :: P.Parser (Maybe AuthToken)
> authTokenParser = do
>   P.string "exp="
>   day <- P.manyTill P.anyChar $ P.char '&'
>   let day' = parseTime defaultTimeLocale (iso8601DateFormat Nothing) day
>   case day' of
>     Nothing  -> return Nothing
>     Just d   -> do
>       P.string "no="
>       memberNo <- P.manyTill P.anyChar $ P.char '&'
>       P.string "ip="
>       ip <- P.manyTill P.anyChar $ P.char '&'
>       P.string "mac="
>       digest' <- P.many1 P.anyChar
>       return $ Just AuthToken {  expiry    = d,
>                                  member    = read memberNo,
>                                  ipAddress = ip,
>                                  digest    = digest' }

> parseAuthToken :: String -> Maybe AuthToken
> parseAuthToken s = case P.parse authTokenParser "" s of
>                    Left e  -> error (show e)
>                    Right x -> x

> verifyTokenDigest :: AuthToken -> IO (Bool)
> verifyTokenDigest a = do
>   digest' <- digestToken a
>   return ((digest a) == digest')

> setAuthCookie :: Integer -> String -> App ()
> setAuthCookie memberNo ip = do
>   authTok <- liftIO $ authToken memberNo ip
>   setCookie Cookie { cookieName    = "auth",
>                      cookieValue   = (show authTok),
>                      cookieExpires = Nothing,
>                      cookieDomain  = Just "vocabulink.com",
>                      cookiePath    = Just "/",
>                      cookieSecure  = False }
