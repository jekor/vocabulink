-- Copyright 2008, 2009, 2010, 2011, 2012 Chris Forno

-- This file is part of Vocabulink.

-- Vocabulink is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- Vocabulink is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

-- To register a new member we need their desired username, a password, and
-- optionally an email address.

module Vocabulink.Member.Registration ( usernameAvailable, emailAvailable
                                      , signup, confirmEmail, resendConfirmEmail
                                      , login, logout
                                      , sendPasswordReset, passwordResetPage, passwordReset
                                      , changeEmail, changePassword, deleteAccount
                                      ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Auth
import Vocabulink.Page
import Vocabulink.Review
import Vocabulink.Utils

import Prelude hiding (div, id, span)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 (fromString)

-- Once a user registers, they can log in. However, they won't be able to use
-- most member-specific functions until they've confirmed their email address.
-- This is to make sure that people cannot impersonate or spam others.

-- Email confirmation consists of generating a unique random string and
-- emailing it to the member as a hyperlink. Once they click the hyperlink we
-- consider the email address confirmed.

signup :: App CGIResult
signup = do
  member <- asks appMember
  case member of
    Just _  -> error "You're already logged in."
    _       -> do
      username' <- getRequiredInput "username"
      email'    <- getRequiredInput "email"
      password' <- getRequiredInput "password"
      terms'    <- getInput "terms"
      learned   <- maybe [] parseLearned <$> getInput "learned"
      userAvail <- usernameAvailable username'
      emailAvail <- emailAvailable email'
      unless userAvail $ error "The username you chose is unavailable or invalid."
      unless emailAvail $ error "The email address you gave is unavailable or invalid."
      when (isNothing terms') $ error "You must accept the Terms of Use."
      from <- supportAddressFull
      memberNo <- withConnection $ \c -> do
        withTransaction c $ do
          memberNo' <- fromJust <$> $(queryTuple
                                      "INSERT INTO member (username, password_hash) \
                                      \VALUES ({username'}, crypt({password'}, gen_salt('bf'))) \
                                      \RETURNING member_no") c
          hash <- fromJust <$> $(queryTuple
                                 "INSERT INTO member_confirmation (member_no, hash, email) \
                                 \VALUES ({memberNo'}, md5(random()::text), {email'}) \
                                 \RETURNING hash") c
          return $ Just memberNo'
          res <- sendConfirmationEmail from email' username' hash
          maybe (rollback c >> return Nothing)
                (\_ -> do $(execute "UPDATE member_confirmation \
                                    \SET email_sent = current_timestamp \
                                    \WHERE member_no = {memberNo'}") c
                          return (Just memberNo')) res
      case memberNo of
        Just mn -> do
          let m = Member { memberNumber = mn
                         , memberName   = username'
                         , memberEmail  = Nothing
                         }
          mapM_ (newReview m) learned
          key <- fromJust <$> getOption "authtokenkey"
          authTok <- liftIO $ authToken mn username' key
          setAuthCookie authTok
          bounce MsgSuccess "Welcome! Please check your email to confirm your account."
        Nothing -> error "Registration failure (this is not your fault). Please try again later."
 where parseLearned :: String -> [Integer]
       parseLearned s = fromMaybe [] $ decode $ fromString s

-- This should be in the App monad and look up the support address itself.
-- However, TemplatePG's withTransaction only works in the IO monad. So we have
-- to duplicate some effort here with the from argument.
sendConfirmationEmail :: String -> String -> String -> String -> IO (Maybe ())
sendConfirmationEmail from email username hash =
  let body = unlines ["Welcome to Vocabulink, " ++ username ++ "."
                     ,""
                     ,"Please click http://www.vocabulink.com/member/confirmation/" ++
                      hash ++ " to confirm your email address."
                     ] in
  sendMail from email "Please confirm your email address." body

resendConfirmEmail :: App CGIResult
resendConfirmEmail = do
  member <- asks appMember
  from <- supportAddressFull
  case member of
    Nothing -> do
      simplePage "Please Login to Resend Your Confirmation Email"
        [ReadyJS "V.loginPopup();"] mempty
    Just m  -> do
      (hash, email) <- fromJust <$> $(queryTuple'
                         "SELECT hash, email FROM member_confirmation \
                         \WHERE member_no = {memberNumber m}")
      res <- liftIO $ sendConfirmationEmail from email (memberName m) hash
      case res of
        Nothing -> error "Error sending confirmation email."
        Just _  -> simplePage "Your confirmation email has been sent." mempty mempty

-- To login a member, simply set their auth cookie. Reloading the page and such
-- is handled by the client.

login :: App CGIResult
login = do
  userid' <- getRequiredInput "userid"
  password' <- getRequiredInput "password"
  match' <- $(queryTuple' "SELECT username, password_hash = crypt({password'}, password_hash) \
                          \FROM member WHERE username = {userid'} OR email = {userid'}")
  match <- if' (isJust match') match' <$> $(queryTuple' "SELECT username, password_hash = crypt({password'}, password_hash) \
                                                        \FROM member \
                                                        \INNER JOIN member_confirmation mc USING (member_no) \
                                                        \WHERE mc.email = {userid'}")
  case match of
    Just (username, Just True) -> do
      member' <- memberByName username
      case member' of
        Nothing     -> error "Failed to lookup username."
        Just member -> do
          key <- fromJust <$> getOption "authtokenkey"
          authTok <- liftIO $ authToken (memberNumber member) username key
          setAuthCookie authTok
          redirect' =<< referrerOrVocabulink
    _ -> bounce MsgError "Username and password do not match (or don't exist)."

-- To logout a member, we simply clear their auth cookie and redirect them
-- to the front page.

logout :: App CGIResult
logout = do
  deleteAuthCookie
  redirect "http://www.vocabulink.com/"

-- We could attempt to check username availability by looking for a user's
-- page. However, a 404 does not necessarily indicate that a username is
-- available:
-- 1. The username might be invalid.
-- 2. The page might be hidden.
-- 3. The casing might be different.
usernameAvailable :: String -> App Bool
usernameAvailable u =
  if' (length u < 4)  (return False) $
  if' (length u > 24) (return False) $
  if' ('@' `elem` u) (return False) $
  isNothing <$> $(queryTuple' "SELECT username FROM member \
                              \WHERE username ILIKE {u}")

-- TODO: Validate email addresses.
emailAvailable :: String -> App Bool
emailAvailable e =
  -- We could check if the address exists in member_confirmation, but that
  -- would allow someone who doesn't control an address to block its use.
  isNothing <$> $(queryTuple' "(SELECT email FROM member \
                               \WHERE email ILIKE {e})")

-- This is the place that the dispatcher will send the client to if they click
-- the hyperlink in the email. If confirmation is successful it redirects them
-- to some hopefully useful page.

-- Once we have confirmed the member's email, we need to set a new auth token
-- cookie for them that contains their gravatar hash.

confirmEmail :: String -> App CGIResult
confirmEmail hash = do
  member <- asks appMember
  when (isJust (memberEmail =<< member)) $ error "You've already confirmed your email."
  case member of
    Nothing -> do
      simplePage "Please Login to Confirm Your Account"
        [ReadyJS "V.loginPopup();"] mempty
    Just m  -> do
      match <- maybe False fromJust <$> $(queryTuple'
        "SELECT hash = {hash} FROM member_confirmation \
        \WHERE member_no = {memberNumber m}")
      if match
        then do h <- asks appDB
                liftIO $ withTransaction h $ do
                  $(execute "UPDATE member SET email = \
                             \(SELECT email FROM member_confirmation \
                              \WHERE member_no = {memberNumber m}) \
                            \WHERE member_no = {memberNumber m}") h
                  $(execute "DELETE FROM member_confirmation \
                            \WHERE member_no = {memberNumber m}") h
                -- We can't just look at the App's member object, since we just
                -- updated it.
                -- TODO: The logic isn't quite right on this.
                key <- fromJust <$> getOption "authtokenkey"
                authTok <- liftIO $ authToken (memberNumber m) (memberName m) key
                setAuthCookie authTok
                bounce MsgSuccess "Congratulations! You've confirmed your account."
        else error "Confirmation code does not match logged in user."

sendPasswordReset :: App CGIResult
sendPasswordReset = do
  email <- getRequiredInput "email"
  -- The member's email address is either in the member table (for confirmed
  -- email addresses), or in the member_confirmation table. We keep them
  -- distinct in order to avoid sending email to unconfirmed addresses. In this
  -- case, however, we'll make an exception. It's likely that a user has
  -- forgotten their password shortly after signing up (before confirming their
  -- email address).
  memberNo <- $(queryTuple' "SELECT member_no FROM member WHERE email = {email} \
                            \UNION \
                            \SELECT member_no FROM member_confirmation WHERE email = {email} \
                            \LIMIT 1")
  case memberNo of
    Just (Just mn) -> do
      $(execute' "DELETE FROM password_reset_token WHERE member_no = {mn}")
      hash <- fromJust <$> $(queryTuple'
        "INSERT INTO password_reset_token (member_no, hash, expires) \
                                  \VALUES ({mn}, md5(random()::text), current_timestamp + interval '4 hours') \
        \RETURNING hash")
      let body = unlines [ "Password Reset"
                         , ""
                         , "Click http://www.vocabulink.com/member/password/reset/" ++
                           hash ++ " to reset your password."
                         , ""
                         , "The password reset page will only be available for 4 hours."
                         ]
      from <- supportAddressFull
      res <- liftIO $ sendMail from email "Vocabulink Password Reset" body
      case res of
        Nothing -> error "Failed to send password reset email."
        _       -> outputNothing
    _              -> error "No member exists with that email address. Please try again."

passwordResetPage :: String -> App CGIResult
passwordResetPage hash = do
  memberNo <- $(queryTuple' "SELECT member_no FROM password_reset_token \
                            \WHERE hash = {hash} AND expires > current_timestamp")
  case memberNo of
    Just _  -> simplePage "Change Your Password" [] $ do
                 form ! action (toValue $ "/member/password/reset/" ++ hash)
                      ! method "post"
                      ! style "width: 33em; margin-left: auto; margin-right: auto; text-align: center" $ do
                   label "Choose a new password: "
                   input ! type_ "password" ! name "password" ! customAttribute "required" "required"
                   br
                   br
                   input ! class_ "light" ! type_ "submit" ! value "Change Password"
    _             -> error "Invalid or expired password reset."

passwordReset :: String -> App CGIResult
passwordReset hash = do
  password <- getRequiredInput "password"
  memberNo <- $(queryTuple' "SELECT member_no FROM password_reset_token \
                            \WHERE hash = {hash} AND expires > current_timestamp")
  case memberNo of
    Just mn -> do
      $(execute' "UPDATE member SET password_hash = crypt({password}, password_hash) \
                 \WHERE member_no = {mn}")
      -- As a convenience, log the user in before redirecting them.
      member' <- memberByNumber mn
      case member' of
        Nothing     -> error "Failed to lookup member."
        Just member -> do
          key <- fromJust <$> getOption "authtokenkey"
          authTok <- liftIO $ authToken (memberNumber member) (memberName member) key
          setAuthCookie authTok
          redirect "http://www.vocabulink.com/"
    _       -> error "Invalid or expired password reset."

changeEmail :: App CGIResult
changeEmail = withRequiredMember' $ \ m -> do
  email <- getRequiredInput "email"
  password <- getRequiredInput "password"
  from <- supportAddressFull
  match <- $(queryTuple' "SELECT password_hash = crypt({password}, password_hash) \
                         \FROM member WHERE member_no = {memberNumber m}")
  case match of
    Just (Just True) -> do
      success <- withConnection $ \c -> do
        withTransaction c $ do
          $(execute "UPDATE member SET email = NULL WHERE member_no = {memberNumber m}") c
          $(execute "DELETE FROM member_confirmation WHERE member_no = {memberNumber m}") c
          hash' <- fromJust <$> $(queryTuple
                                 "INSERT INTO member_confirmation (member_no, hash, email) \
                                 \VALUES ({memberNumber m}, md5(random()::text), {email}) \
                                 \RETURNING hash") c
          res <- sendConfirmationEmail from email (memberName m) hash'
          maybe (rollback c >> return False)
                (\_ -> do $(execute "UPDATE member_confirmation \
                                    \SET email_sent = current_timestamp \
                                    \WHERE member_no = {memberNumber m}") c
                          return True) res
      if success
        then bounce MsgSuccess "Email address changed successfully. Please check your email to confirm the change."
        else bounce MsgError "We're sorry. We encountered an unknown error trying to change your email address."
    _ -> bounce MsgError "Wrong password."

changePassword :: App CGIResult
changePassword = withRequiredMember' $ \ m -> do
  oldPassword <- getRequiredInput "old-password"
  newPassword <- getRequiredInput "new-password"
  match <- $(queryTuple' "SELECT password_hash = crypt({oldPassword}, password_hash) \
                         \FROM member WHERE member_no = {memberNumber m}")
  case match of
    Just (Just True) -> do
      $(execute' "UPDATE member SET password_hash = crypt({newPassword}, gen_salt('bf'))")
      bounce MsgSuccess "Password changed successfully."
    _ -> bounce MsgError "Wrong password."

deleteAccount :: App CGIResult
deleteAccount = withRequiredMember' $ \ m -> do
  password <- getRequiredInput "password"
  match <- $(queryTuple' "SELECT password_hash = crypt({password}, password_hash) \
                         \FROM member WHERE member_no = {memberNumber m}")
  case match of
    Just (Just True) -> do
      $(execute' "DELETE FROM member WHERE member_no = {memberNumber m}")
      addr <- supportAddress
      liftIO $ sendMail addr addr "Member deleted account." ("Member " ++ memberName m ++ " deleted their account.")
      deleteAuthCookie
      bounce MsgSuccess "Your account was successfully deleted."
    _ -> bounce MsgError "Wrong password."
