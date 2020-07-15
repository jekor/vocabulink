-- To register a new member we need their desired username, a password, and
-- optionally an email address.

module Vocabulink.Member.Registration ( usernameAvailable, emailAvailable
                                      , signup, confirmEmail, resendConfirmEmail
                                      , login
                                      , sendPasswordReset, passwordResetPage, passwordReset
                                      , changeEmail, changePassword, deleteAccount
                                      ) where

import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Html
import Vocabulink.Page
import Vocabulink.Review
import Vocabulink.Utils

import qualified Data.ByteString.Lazy.UTF8 as BLU

import Prelude hiding (div, id, span)

-- Once a user registers, they can log in. However, they won't be able to use
-- most member-specific functions until they've confirmed their email address.
-- This is to make sure that people cannot impersonate or spam others.

-- Email confirmation consists of generating a unique random string and
-- emailing it to the member as a hyperlink. Once they click the hyperlink we
-- consider the email address confirmed.

signup username email password learned = do
  when (isJust ?member) (vError "You're already logged in.")
  userAvail <- usernameAvailable username
  emailAvail <- emailAvailable email
  unless userAvail (vError "The username you chose is unavailable or invalid.")
  unless emailAvail (vError "The email address you gave is unavailable or invalid.")
  memberNo <- withTransaction ?db $ do
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
  mapM_ (newReview m) (fromMaybe [] (parseLearned =<< learned))
  authCookie memberNo ?tokenKey
 where parseLearned :: String -> Maybe [Int32]
       parseLearned = decode . BLU.fromString

-- This should be in the App monad and look up the support address itself.
-- However, TemplatePG's withTransaction only works in the IO monad. So we have
-- to duplicate some effort here with the from argument.
sendConfirmationEmail email username hash =
  let body = unlines ["Welcome to Vocabulink, " ++ username ++ "."
                     ,""
                     ,"Please click http://www.vocabulink.com/member/confirmation/" ++
                      hash ++ " to confirm your email address."
                     ] in
  sendMail email "Please confirm your email address." body

resendConfirmEmail m = do
  (hash, email) <- fromJust <$> $(queryTuple
                     "SELECT hash, email FROM member_confirmation \
                     \WHERE member_no = {memberNumber m}") ?db
  sendConfirmationEmail email (memberName m) hash

login userid password = do
  match' <- $(queryTuple "SELECT username, password_hash = crypt({password}, password_hash) \
                         \FROM member WHERE username = {userid} OR email = {userid}") ?db
  match <- if' (isJust match') match' <$> $(queryTuple "SELECT username, password_hash = crypt({password}, password_hash) \
                                                       \FROM member \
                                                       \INNER JOIN member_confirmation mc USING (member_no) \
                                                       \WHERE mc.email = {userid}") ?db
  case match of
    Just (username, Just True) -> do
      member' <- memberByName username
      case member' of
        Nothing -> vError "Failed to lookup username."
        Just m -> authCookie (memberNumber m) ?tokenKey
    _ -> vError "Username and password do not match (or don't exist)."

-- We could attempt to check username availability by looking for a user's
-- page. However, a 404 does not necessarily indicate that a username is
-- available:
-- 1. The username might be invalid.
-- 2. The page might be hidden.
-- 3. The casing might be different.
usernameAvailable u =
  if' (length u < 4)  (return False) $
  if' (length u > 24) (return False) $
  if' ('@' `elem` u) (return False) $
  (isNothing :: Maybe Text -> Bool) <$> $(queryTuple "SELECT username FROM member \
                                                     \WHERE username ILIKE {u}") ?db

-- TODO: Validate email addresses.
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

confirmEmail hash =
  case ?member of
    Nothing -> Left `fmap` simplePage "Please Login to Confirm Your Account" [ReadyJS "V.loginPopup();"] mempty
    Just m  -> do
      case (memberEmail m) of
        Nothing -> do
          match <- maybe False fromJust <$> $(queryTuple
            "SELECT hash = {hash} FROM member_confirmation \
            \WHERE member_no = {memberNumber m}") ?db
          if match
            then do withTransaction ?db $ do
                      $(execute "UPDATE member SET email = \
                                 \(SELECT email FROM member_confirmation \
                                  \WHERE member_no = {memberNumber m}) \
                                \WHERE member_no = {memberNumber m}") ?db
                      $(execute "DELETE FROM member_confirmation \
                                \WHERE member_no = {memberNumber m}") ?db
                    -- We can't just look at the App's member object, since we just
                    -- updated it.
                    -- TODO: The logic isn't quite right on this.
                    Right `fmap` authCookie (memberNumber m) ?tokenKey
            else vError "The confirmation code does not match who you're logged in as."
        Just _ -> vError "You've already confirmed your email."

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
    _ -> vError "No member exists with that email address. Please try again."

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
    _ -> vError "Invalid or expired password reset."

passwordReset hash password = do
  memberNo <- $(queryTuple "SELECT member_no FROM password_reset_token \
                           \WHERE hash = {hash} AND expires > current_timestamp") ?db
  case memberNo of
    Just (mn :: Int32) -> do
      member' <- uncurryN Member <$$> ($(queryTuple "UPDATE member SET password_hash = crypt({password}, password_hash) \
                                                    \WHERE member_no = {mn} \
                                                    \RETURNING member_no, username, email") ?db)
      -- As a convenience, log the user in before redirecting them.
      case member' of
        Nothing -> vError "Failed to lookup member."
        Just m -> authCookie (memberNumber m) ?tokenKey
    _ -> vError "Invalid or expired password reset."

changeEmail email password = withLoggedInMember $ \ m -> do
  match <- $(queryTuple "SELECT password_hash = crypt({password}, password_hash) \
                        \FROM member WHERE member_no = {memberNumber m}") ?db
  case match of
    Just (Just True) -> do
      withTransaction ?db $ do
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
    _ -> vError "Wrong password."

changePassword oldPassword newPassword = withLoggedInMember $ \ m -> do
  match <- $(queryTuple "SELECT password_hash = crypt({oldPassword}, password_hash) \
                        \FROM member WHERE member_no = {memberNumber m}") ?db
  case match of
    Just (Just True) -> $(execute "UPDATE member SET password_hash = crypt({newPassword}, gen_salt('bf'))") ?db
    _ -> vError "Wrong password."

deleteAccount password = withLoggedInMember $ \ m -> do
  match <- $(queryTuple "SELECT password_hash = crypt({password}, password_hash) \
                        \FROM member WHERE member_no = {memberNumber m}") ?db
  case match of
    Just (Just True) -> do
      $(execute "DELETE FROM member WHERE member_no = {memberNumber m}") ?db
      sendMail "support@vocabulink.com" "Member deleted account." ("Member " ++ memberName m ++ " deleted their account.")
    _ -> vError "Wrong password."
