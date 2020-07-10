-- To register a new member we need their desired username, a password, and
-- optionally an email address.

module Vocabulink.Member.Registration ( usernameAvailable, emailAvailable
                                      , signup, confirmEmail, resendConfirmEmail
                                      , login, logout
                                      , sendPasswordReset, passwordResetPage, passwordReset
                                      , changeEmail, changePassword, deleteAccount
                                      ) where

import Vocabulink.CGI
import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Html
import Vocabulink.Page
import Vocabulink.Review
import Vocabulink.Utils

import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.Exit (ExitCode(..))
import System.IO (hPutStr, hClose)
import System.Process (createProcess, waitForProcess, proc, std_in, StdStream(..))

import Prelude hiding (div, id, span)

-- Once a user registers, they can log in. However, they won't be able to use
-- most member-specific functions until they've confirmed their email address.
-- This is to make sure that people cannot impersonate or spam others.

-- Email confirmation consists of generating a unique random string and
-- emailing it to the member as a hyperlink. Once they click the hyperlink we
-- consider the email address confirmed.

signup :: E (SCGI Response)
signup = do
  when (isJust ?member) $ error "You're already logged in."
  username <- bodyVarRequired "username"
  email    <- bodyVarRequired "email"
  password <- bodyVarRequired "password"
  learned  <- fromMaybe [] `liftM` (parseLearned =<<) `liftM` bodyVar "learned"
  userAvail <- liftIO $ usernameAvailable username
  emailAvail <- liftIO $ emailAvailable email
  unless userAvail $ error "The username you chose is unavailable or invalid."
  unless emailAvail $ error "The email address you gave is unavailable or invalid."
  memberNo <- liftIO $ withTransaction ?db $ do
    memberNo' <- fromJust <$> $(queryTuple
                                "INSERT INTO member (username, password_hash) \
                                \VALUES ({username}, crypt({password}, gen_salt('bf'))) \
                                \RETURNING member_no") ?db
    hash <- fromJust <$> $(queryTuple
                           "INSERT INTO member_confirmation (member_no, hash, email) \
                           \VALUES ({memberNo'}, md5(random()::text), {email}) \
                           \RETURNING hash") ?db
    sendConfirmationEmail email username hash
    $(execute "UPDATE member_confirmation \
              \SET email_sent = current_timestamp \
              \WHERE member_no = {memberNo'}") ?db
    return memberNo'
  let m = Member { memberNumber = memberNo
                 , memberName   = username
                 , memberEmail  = Nothing
                 }
  liftIO $ mapM_ (newReview m) learned
  setCookie =<< liftIO (authCookie memberNo ?tokenKey)
  bounce MsgSuccess "Welcome! Please check your email to confirm your account."
 where parseLearned :: String -> Maybe [Int32]
       parseLearned = decode . BLU.fromString

-- This should be in the App monad and look up the support address itself.
-- However, TemplatePG's withTransaction only works in the IO monad. So we have
-- to duplicate some effort here with the from argument.
sendConfirmationEmail :: E (String -> String -> String -> IO ())
sendConfirmationEmail email username hash =
  let body = unlines ["Welcome to Vocabulink, " ++ username ++ "."
                     ,""
                     ,"Please click http://www.vocabulink.com/member/confirmation/" ++
                      hash ++ " to confirm your email address."
                     ] in
  sendMail email "Please confirm your email address." body

resendConfirmEmail :: E (Member -> IO ())
resendConfirmEmail m = do
  (hash, email) <- fromJust <$> $(queryTuple
                     "SELECT hash, email FROM member_confirmation \
                     \WHERE member_no = {memberNumber m}") ?db
  sendConfirmationEmail email (memberName m) hash

login :: E (SCGI Response)
login = do
  userid' <- bodyVarRequired "userid"
  password' <- bodyVarRequired "password"
  match' <- liftIO $ $(queryTuple "SELECT username, password_hash = crypt({password'}, password_hash) \
                                  \FROM member WHERE username = {userid'} OR email = {userid'}") ?db
  match <- liftIO $ if' (isJust match') match' <$> $(queryTuple "SELECT username, password_hash = crypt({password'}, password_hash) \
                                                                \FROM member \
                                                                \INNER JOIN member_confirmation mc USING (member_no) \
                                                                \WHERE mc.email = {userid'}") ?db
  case match of
    Just (username, Just True) -> do
      member' <- liftIO $ memberByName username
      case member' of
        Nothing -> error "Failed to lookup username."
        Just m -> do
          setCookie =<< liftIO (authCookie (memberNumber m) ?tokenKey)
          redirect =<< referrerOrVocabulink
    _ -> bounce MsgError "Username and password do not match (or don't exist)."

logout :: SCGI Response
logout = do
  setCookie $ emptyAuthCookie {setCookieMaxAge = Just $ secondsToDiffTime 0}
  redirect "http://www.vocabulink.com/"

-- We could attempt to check username availability by looking for a user's
-- page. However, a 404 does not necessarily indicate that a username is
-- available:
-- 1. The username might be invalid.
-- 2. The page might be hidden.
-- 3. The casing might be different.
usernameAvailable :: E (String -> IO Bool)
usernameAvailable u =
  if' (length u < 4)  (return False) $
  if' (length u > 24) (return False) $
  if' ('@' `elem` u) (return False) $
  (isNothing :: Maybe Text -> Bool) <$> $(queryTuple "SELECT username FROM member \
                                                     \WHERE username ILIKE {u}") ?db

-- TODO: Validate email addresses.
emailAvailable :: E (String -> IO Bool)
emailAvailable e =
  -- We could check if the address exists in member_confirmation, but that
  -- would allow someone who doesn't control an address to block its use.
  (isNothing :: Maybe (Maybe Text) -> Bool) <$> $(queryTuple "SELECT email FROM member \
                                                             \WHERE email ILIKE {e}") ?db

-- This is the place that the dispatcher will send the client to if they click
-- the hyperlink in the email. If confirmation is successful it redirects them
-- to some hopefully useful page.

-- Once we have confirmed the member's email, we need to set a new auth token
-- cookie for them that contains their gravatar hash.

confirmEmail :: E (String -> SCGI Response)
confirmEmail hash =
  case ?member of
    Nothing -> do
      toResponse $ simplePage "Please Login to Confirm Your Account"
        [ReadyJS "V.loginPopup();"] mempty
    Just m  -> do
      case (memberEmail m) of
        Nothing -> do
          match <- liftIO $ maybe False fromJust <$> $(queryTuple
            "SELECT hash = {hash} FROM member_confirmation \
            \WHERE member_no = {memberNumber m}") ?db
          if match
            then do liftIO $ withTransaction ?db $ do
                      $(execute "UPDATE member SET email = \
                                 \(SELECT email FROM member_confirmation \
                                  \WHERE member_no = {memberNumber m}) \
                                \WHERE member_no = {memberNumber m}") ?db
                      $(execute "DELETE FROM member_confirmation \
                                \WHERE member_no = {memberNumber m}") ?db
                    -- We can't just look at the App's member object, since we just
                    -- updated it.
                    -- TODO: The logic isn't quite right on this.
                    setCookie =<< liftIO (authCookie (memberNumber m) ?tokenKey)
                    bounce MsgSuccess "Congratulations! You've confirmed your account."
            else bounce MsgError "The confirmation code does not match who you're logged in as."
        Just _ -> bounce MsgNotice "You've already confirmed your email."

sendPasswordReset :: E (String -> IO ())
sendPasswordReset email = do
  -- The member's email address is either in the member table (for confirmed
  -- email addresses), or in the member_confirmation table. We keep them
  -- distinct in order to avoid sending email to unconfirmed addresses. In this
  -- case, however, we'll make an exception. It's likely that a user has
  -- forgotten their password shortly after signing up (before confirming their
  -- email address).
  memberNo <- $(queryTuple "SELECT member_no FROM member WHERE email = {email} \
                           \UNION \
                           \SELECT member_no FROM member_confirmation WHERE email = {email} \
                           \LIMIT 1") ?db
  case memberNo of
    Just (Just (mn :: Int32)) -> do
      $(execute "DELETE FROM password_reset_token WHERE member_no = {mn}") ?db
      hash <- fromJust <$> $(queryTuple
        "INSERT INTO password_reset_token (member_no, hash, expires) \
                                  \VALUES ({mn}, md5(random()::text), current_timestamp + interval '4 hours') \
        \RETURNING hash") ?db
      let body = unlines [ "Password Reset"
                         , ""
                         , "Click http://www.vocabulink.com/member/password/reset/" ++
                           hash ++ " to reset your password."
                         , ""
                         , "The password reset page will only be available for 4 hours."
                         ]
      sendMail email "Vocabulink Password Reset" body
    _ -> error "No member exists with that email address. Please try again."

passwordResetPage :: E (String -> IO Html)
passwordResetPage hash = do
  memberNo <- $(queryTuple "SELECT member_no FROM password_reset_token \
                           \WHERE hash = {hash} AND expires > current_timestamp") ?db
  case memberNo of
    Just (_ :: Int32) -> simplePage "Change Your Password" [] $ do
                form ! action (toValue $ "/member/password/reset/" ++ hash)
                     ! method "post"
                     ! style "width: 33em; margin-left: auto; margin-right: auto; text-align: center" $ do
                  label "Choose a new password: "
                  input ! type_ "password" ! name "password" ! customAttribute "required" "required"
                  br
                  br
                  input ! class_ "light" ! type_ "submit" ! value "Change Password"
    _ -> error "Invalid or expired password reset."

passwordReset :: E (String -> SCGI Response)
passwordReset hash = do
  password <- bodyVarRequired "password"
  memberNo <- liftIO $ $(queryTuple "SELECT member_no FROM password_reset_token \
                                    \WHERE hash = {hash} AND expires > current_timestamp") ?db
  case memberNo of
    Just (mn :: Int32) -> do
      member' <- uncurryN Member <$$> (liftIO $ $(queryTuple "UPDATE member SET password_hash = crypt({password}, password_hash) \
                                                             \WHERE member_no = {mn} \
                                                             \RETURNING member_no, username, email") ?db)
      -- As a convenience, log the user in before redirecting them.
      case member' of
        Nothing -> error "Failed to lookup member."
        Just m -> do
          setCookie =<< liftIO (authCookie (memberNumber m) ?tokenKey)
          bounce MsgSuccess "Password reset successfully."
    _ -> error "Invalid or expired password reset."

changeEmail :: E (SCGI Response)
changeEmail = withLoggedInMember $ \ m -> do
  email <- bodyVarRequired "email"
  password <- bodyVarRequired "password"
  match <- liftIO $ $(queryTuple "SELECT password_hash = crypt({password}, password_hash) \
                                 \FROM member WHERE member_no = {memberNumber m}") ?db
  case match of
    Just (Just True) -> do
      liftIO $ withTransaction ?db $ do
        $(execute "UPDATE member SET email = NULL WHERE member_no = {memberNumber m}") ?db
        $(execute "DELETE FROM member_confirmation WHERE member_no = {memberNumber m}") ?db
        hash' <- fromJust <$> $(queryTuple
          "INSERT INTO member_confirmation (member_no, hash, email) \
          \VALUES ({memberNumber m}, md5(random()::text), {email}) \
          \RETURNING hash") ?db
        sendConfirmationEmail email (memberName m) hash'
        $(execute "UPDATE member_confirmation \
                  \SET email_sent = current_timestamp \
                  \WHERE member_no = {memberNumber m}") ?db
      bounce MsgSuccess "Email address changed successfully. Please check your email to confirm the change."
    _ -> error "Wrong password."

changePassword :: E (SCGI Response)
changePassword = withLoggedInMember $ \ m -> do
  oldPassword <- bodyVarRequired "old-password"
  newPassword <- bodyVarRequired "new-password"
  match <- liftIO $ $(queryTuple "SELECT password_hash = crypt({oldPassword}, password_hash) \
                                 \FROM member WHERE member_no = {memberNumber m}") ?db
  case match of
    Just (Just True) -> do
      liftIO $ $(execute "UPDATE member SET password_hash = crypt({newPassword}, gen_salt('bf'))") ?db
      bounce MsgSuccess "Password changed successfully."
    _ -> error "Wrong password."

deleteAccount :: E (SCGI Response)
deleteAccount = withLoggedInMember $ \ m -> do
  password <- bodyVarRequired "password"
  match <- liftIO $ $(queryTuple "SELECT password_hash = crypt({password}, password_hash) \
                                 \FROM member WHERE member_no = {memberNumber m}") ?db
  case match of
    Just (Just True) -> do
      liftIO $ $(execute "DELETE FROM member WHERE member_no = {memberNumber m}") ?db
      liftIO $ sendMail "support@vocabulink.com" "Member deleted account." ("Member " ++ memberName m ++ " deleted their account.")
      setCookie $ emptyAuthCookie {setCookieMaxAge = Just $ secondsToDiffTime 0}
      bounce MsgSuccess "Your account was successfully deleted."
    _ -> error "Wrong password."

sendMail :: E (String -> String -> String -> IO ())
sendMail address subject body = do
  (Just inF, _, _, pr) <- createProcess (proc ?sendmail [address]) {std_in = CreatePipe}
  hPutStr inF message >> hClose inF
  status <- waitForProcess pr
  when (status /= ExitSuccess) $ error "There was an error sending email from our servers. Please try again later or contact support@vocabulink.com."
 where message = unlines [ "From: \"Vocabulink\" <support@vocabulink.com>"
                         , "To: " ++ address
                         , "Subject: " ++ subject
                         , ""
                         , body
                         ]
