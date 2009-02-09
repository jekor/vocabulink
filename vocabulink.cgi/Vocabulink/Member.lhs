> module Vocabulink.Member where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB (query1, quickInsertNo, fromSql, toSql, catchSqlE)
> import Vocabulink.Html (stdPage)
> import Vocabulink.Utils

> import Vocabulink.Member.Auth (setAuthCookie)

> import Control.Monad.Reader (asks)
> import Data.Maybe (fromMaybe)
> import Network.URI (escapeURIString, isUnescapedInURI)
> import Text.XHtml.Strict

Run the App with the currently logged in member's number or redirect to the
login page if the user isn't logged in.

> withMemberNumber :: (Integer -> App CGIResult) -> App CGIResult
> withMemberNumber f = do
>   memberNo <- asks memberNumber
>   case memberNo of
>     Nothing -> redirectToLoginPage
>     Just n  -> f n

> withMemberNumber' :: (Integer -> App a) -> App a
> withMemberNumber' f = do
>   memberNo <- asks memberNumber
>   case memberNo of
>     Nothing -> error "No logged in member."
>     Just n  -> f n

Add a member to the database. We're going to do validation of acceptable
username characters at this level because PostgreSQL's CHECK constraint doesn't
handle Unicode regular expressions properly.

A username should be allowed to have any alphanumeric characters (in any
language) and URL-safe punctuation.

This returns the new member number.

> addMember' :: String -> String -> Maybe String -> App (Maybe Integer)
> addMember' username passwd email = do
>   c <- asks db
>   (length username) < 3  ? error "Your username must have 3 characters or more."  $
>     (length username) > 32 ? error "Your username must have 32 characters or less." $
>     (length passwd)   > 72 ? error "Your password must have 72 characters or less." $
>     -- TODO: Add password constraints?
>     liftIO $ quickInsertNo c "INSERT INTO member (username, email, password_hash) \
>                              \VALUES (?, ?, crypt(?, gen_salt('bf')))"
>                              [toSql username, toSql email, toSql passwd]
>                              "member_member_no_seq"
>                `catchSqlE` "Failed to add member."

> addMember :: App CGIResult
> addMember = do
>   username <- getRequiredInput "username"
>   passwd   <- getRequiredInput "password"
>   email    <- getInput "email"
>   memberNo <- addMember' username passwd email
>   case memberNo of
>     Nothing -> error "Failed to add member."
>     Just n -> do
>       ip <- remoteAddr
>       setAuthCookie n ip
>       redirect "/"

> newMemberPage :: App CGIResult
> newMemberPage = do
>   -- The user may have gotten to this page in some way in which they've
>   -- already indicated which username they want.
>   username <- getInputDefault "" "username"
>   stdPage "Join Vocabulink" []
>     [ h1 << "Join Vocabulink",
>       form ! [action "", method "post"] <<
>         [ label << "Username:",
>           widget "textfield" "username" [value username],
>           br,
>           label << "Password:",
>           password "password",
>           br,
>           label << "Email:",
>           textfield "email",
>           br,
>           submit "" "Join" ] ]

> getMemberNumber :: String -> App (Integer)
> getMemberNumber username = do
>   c <- asks db
>   n <- liftIO $ query1 c "SELECT member_no FROM member \
>                          \WHERE username = ?" [toSql username]
>                   `catchSqlE` "Failed to retrieve member number from username."
>   return $ maybe (error "fail") fromSql n

Login attempts to match the username and password supplied against the
information in the database.

> validPassword :: String -> String -> App (Bool)
> validPassword username passwd = do
>   c <- asks db
>   n <- liftIO $ query1 c "SELECT password_hash = crypt(?, password_hash) \
>                          \FROM member WHERE username = ?"
>                          [toSql passwd, toSql username]
>                   `catchSqlE` "Internal authentication failure (this is not your fault)."
>   return $ maybe (error "fail") fromSql n

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

> login :: App CGIResult
> login = do
>   join <- getInputDefault "" "join"
>   if join /= ""
>      then do -- The user clicked the Join button in the login box.
>        -- Carry over the username if they entered it.
>        username <- getInputDefault "" "username"
>        if username /= ""
>           then redirect $ "/member/join?username=" ++ username
>           else redirect "/member/join"
>      else do
>        username  <- getRequiredInput "username"
>        memberNo  <- getMemberNumber username
>        passwd    <- getRequiredInput "password"
>        ref       <- refererOrVocabulink
>        redirect' <- getInputDefault ref "redirect"
>        ip <- remoteAddr
>        valid <- validPassword username passwd
>        not valid ? error "Login failed." $ do
>          setAuthCookie memberNo ip
>          redirect redirect'

> logout :: App CGIResult
> logout = do
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
>   referer'  <- refererOrVocabulink
>   redirect' <- getInputDefault referer' "redirect"
>   stdPage "Log In" []
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
