> module Vocabulink.Member where

> import Vocabulink.CGI (App, AppEnv(..), getInput', getInputDefault, referer)
> import Vocabulink.DB (query1, quickInsertNo, toSql', fromSql', catchSqlE)
> import Vocabulink.Html (outputHtml, page)
> import Vocabulink.Utils ((?))

> import Control.Monad.Reader (asks)
> import Data.ByteString.Char8 (pack)
> import Data.Digest.OpenSSL.HMAC (hmac, sha1)
> import Data.Maybe (fromMaybe)
> import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixDayLength)
> import Database.HDBC (toSql, fromSql)
> import Network.FastCGI
> import Network.URI (escapeURIString, isUnescapedInURI)
> import qualified Text.ParserCombinators.Parsec as P
> import Text.XHtml.Strict

Add a member to the database. We're going to do validation of acceptable
username characters at this level because PostgreSQL's CHECK constraint doesn't
handle Unicode regular expressions properly.

A username should be allowed to have any alphanumeric characters (in any
language) and URL-safe punctuation.

This returns the new member number.

> addMember :: String -> String -> Maybe String -> App (Maybe Integer)
> addMember username passwd email = do
>   c <- asks db
>   (length username) < 3  ? error "Your username must have 3 characters or more."  $
>     (length username) > 32 ? error "Your username must have 32 characters or less." $
>     (length passwd)   > 72 ? error "Your password must have 72 characters or less." $
>     -- TODO: Add password constraints?
>     liftIO $ quickInsertNo c "INSERT INTO member (username, email, password_hash) \
>                            \VALUES (?, ?, crypt(?, gen_salt('bf')))"
>                            [toSql' username, toSql' email, toSql' passwd]
>                            "member_member_no_seq"
>                `catchSqlE` "Failed to add member."

> addMember' :: App CGIResult
> addMember' = do
>   username <- getInput' "username"
>   passwd   <- getInput' "password"
>   email    <- getInput' "email"
>   memberNo <- addMember username passwd email
>   case memberNo of
>     Nothing -> error "Failed to add member."
>     Just no -> do
>       ip <- remoteAddr
>       setAuthCookie no ip
>       redirect "/"

> newMemberPage :: String
> newMemberPage = renderHtml $ page "Join Vocabulink" []
>   [ h1 << "Join Vocabulink",
>     form ! [action "", method "post"] <<
>       [ label << "Username:",
>         textfield "username",
>         br,
>         label << "Password:",
>         password "password",
>         br,
>         label << "Email:",
>         textfield "email",
>         br,
>         submit "" "Join" ] ]

> memberNumber :: String -> App (Integer)
> memberNumber username = do
>   c <- asks db
>   n <- liftIO $ query1 c "SELECT member_no FROM member \
>                          \WHERE username = ?" [toSql' username]
>                   `catchSqlE` "Failed to retrieve member number from username."
>   return $ maybe (error "fail") fromSql' n

> memberName :: Integer -> App (String)
> memberName memberNo = do
>   c <- asks db
>   n <- liftIO $ query1 c "SELECT username FROM member \
>                          \WHERE member_no = ?" [toSql memberNo]
>                   `catchSqlE` "Failed to retrieve username from member number."
>   return $ maybe (error "fail") fromSql' n

Sometimes we don't care about anonymous.

> memberName' :: Integer -> App (Maybe String)
> memberName' memberNo = case memberNo of
>                          0 -> return Nothing
>                          _ -> memberName memberNo >>= \n -> return $ Just n

Login attempts to match the username and password supplied against the
information in the database.

> validPassword :: String -> String -> App (Bool)
> validPassword username passwd = do
>   c <- asks db
>   n <- liftIO $ query1 c "SELECT password_hash = crypt(?, password_hash) \
>                          \FROM member WHERE username = ?"
>                          [toSql' passwd, toSql' username]
>                   `catchSqlE` "Internal authentication failure (this is not your fault)."
>   return $ maybe (error "fail") fromSql n

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

> expired :: POSIXTime -> POSIXTime -> Bool
> expired now ex = (floor ex :: Integer) - (floor now :: Integer) < 0

> loginNumber :: App (Integer)
> loginNumber = do
>   authCookie <- getCookie "auth"
>   case authCookie of
>     Nothing -> return 0
>     Just c  -> do
>       ip <- remoteAddr
>       memberNo <- liftIO $ verifyMember c ip
>       return $ fromMaybe 0 memberNo -- 0 == anonymous

> loginName :: App (String)
> loginName = do
>   n <- loginNumber
>   memberName n

> loginName' :: App (Maybe String)
> loginName' = do
>   n <- loginNumber
>   memberName' n

> loggedIn :: App (Bool)
> loggedIn = do
>   n <- loginNumber
>   return $ n > 0

If we want to set the expiration, we have to muck around with CalendarTimes and
use something like
  addToClockTime TimeDiff { tdYear = 0,
                            tdMonth = 0,
                            tdDay = 1,
                            tdHour = 0,
                            tdMin = 0,
                            tdSec = 0,
                            tdPicosec = 0 }

For now, I'm just going to let it expire at the end of the session. If the
session lasts longer than the expiration time, we can invalidate the cookie.

> login' :: App CGIResult
> login' = do
>   username  <- getInput' "username"
>   memberNo  <- memberNumber username
>   passwd    <- getInput' "password"
>   ref       <- referer
>   redirect' <- getInputDefault ref "redirect"
>   ip <- remoteAddr
>   valid <- validPassword username passwd
>   not valid ? error "Login failed." $ do
>     setAuthCookie memberNo ip
>     redirect redirect'

> setAuthCookie :: Integer -> String -> App ()
> setAuthCookie memberNo ip = do
>   authTok <- liftIO $ authToken memberNo ip
>   setCookie Cookie { cookieName    = "auth",
>                      cookieValue   = (show authTok),
>                      cookieExpires = Nothing,
>                      cookieDomain  = Just "vocabulink.com",
>                      cookiePath    = Just "/",
>                      cookieSecure  = False }

> logout' :: App CGIResult
> logout' = do
>   redirect' <- getInputDefault "/" "redirect"
>   deleteCookie Cookie { cookieName   = "auth",
>                         cookieDomain = Just "vocabulink.com",
>                         cookiePath   = Just "/",
>                         -- The following are only here to get rid of GHC warnings.
>                         cookieValue  = "",
>                         cookieExpires = Nothing,
>                         cookieSecure = False }
>   redirect redirect'

> logoutForm :: Html
> logoutForm = form ! [action "/member/logout", method "post"] <<
>                submit "" "Log Out"

> loginPage :: App CGIResult
> loginPage = do
>   referer'  <- referer
>   redirect' <- getInputDefault referer' "redirect"
>   outputHtml $ page "Log In" []
>     [ h1 << "Log In",
>       form ! [action "", method "post"] <<
>         [ hidden "redirect" redirect',
>           label << "Username:",
>           textfield "username",
>           br,
>           label << "Password:",
>           password "password",
>           br,
>           submit "" "Log In" ],
>       paragraph << ("Not a member? " +++ anchor ! [href "/member/join"] << "Join!") ]

> loginRedirectPage :: App String
> loginRedirectPage = do
>   request <- getVar "REQUEST_URI"
>   let request' = fromMaybe "/" request
>   return $ "/member/login?redirect=" ++ escapeURIString isUnescapedInURI request'

> redirectToLoginPage :: App CGIResult
> redirectToLoginPage = loginRedirectPage >>= redirect
