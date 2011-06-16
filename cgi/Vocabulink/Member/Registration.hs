-- Copyright 2008, 2009, 2010, 2011 Chris Forno

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

module Vocabulink.Member.Registration ( usernameAvailable
                                      , signup, confirmEmail, confirmEmailPage
                                      , login, logout
                                      , sendPasswordReset, passwordResetPage, passwordReset
                                      ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Auth
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, id, span)

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
      userAvail <- usernameAvailable username'
      when (not userAvail) $ error "The username you chose is not available."
      when (not $ emailValid email') $ error "The email address you gave is invalid."
      when (isNothing terms') $ error "You must accept the Terms of Use."
      memberNo <- fromJust <$> $(queryTuple'
                                 "INSERT INTO member (username, password_hash) \
                                 \VALUES ({username'}, crypt({password'}, gen_salt('bf'))) \
                                 \RETURNING member_no")
      ip <- remoteAddr
      key <- fromJust <$> getOption "authtokenkey"
      authTok <- liftIO $ authToken memberNo username' Nothing ip key
      setAuthCookie authTok
      res' <- sendConfirmationEmail memberNo email'
      case res' of
        Nothing -> error "Registration failure (this is not your fault)."
        Just _  -> outputNothing

-- To login a member, simply set their auth cookie. Reloading the page and such
-- is handled by the client.

login :: App CGIResult
login = do
  username' <- getRequiredInput "username"
  password' <- getRequiredInput "password"
  match <- $(queryTuple' "SELECT password_hash = crypt({password'}, password_hash) \
                         \FROM member WHERE username = {username'}")
  case match of
    Just (Just True) -> do
      ip      <- remoteAddr
      member' <- memberByName username'
      case member' of
        Nothing     -> error "Failed to lookup username."
        Just member -> do
          key <- fromJust <$> getOption "authtokenkey"
          authTok <- liftIO $ authToken (memberNumber member) username' (memberEmail member) ip key
          setAuthCookie authTok
          outputNothing
    _         -> error "Username and password do not match (or don't exist)."

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
  if' (length u > 32) (return False) $
  isNothing <$> $(queryTuple' "SELECT username FROM member \
                              \WHERE username ILIKE {u}")


-- TODO: Validate email addresses.
emailValid :: String -> Bool
emailValid _ = True

-- Once a user registers, they can log in. However, they won't be able to use
-- most member-specific functions until they've confirmed their email address.
-- This is to make sure that people cannot impersonate or spam others.

-- Email confirmation consists of generating a unique random string and
-- emailing it to the member as a hyperlink. Once they click the hyperlink we
-- consider the email address confirmed.

sendConfirmationEmail :: Integer -> String -> App (Maybe ())
sendConfirmationEmail memberNo email = do
  hash <- fromJust <$> $(queryTuple'
    "INSERT INTO member_confirmation (member_no, hash, email) \
                             \VALUES ({memberNo}, md5(random()::text), {email}) \
    \RETURNING hash")
  let body = unlines [
               "Welcome to Vocabulink.",
               "",
               "Click http://www.vocabulink.com/member/confirmation/" ++
               hash ++ " to confirm your email address." ]
  res <- liftIO $ sendMail email "Welcome to Vocabulink" body
  maybe (return Nothing) (\_ -> do $(execute' "UPDATE member_confirmation \
                                              \SET email_sent = current_timestamp \
                                             \WHERE member_no = {memberNo}")
                                   return $ Just ()) res

-- This is the place that the dispatcher will send the client to if they click
-- the hyperlink in the email. If confirmation is successful it redirects them
-- to some hopefully useful page.

-- Once we have confirmed the member's email, we need to set a new auth token
-- cookie for them that contains their gravatar hash.

confirmEmail :: String -> App CGIResult
confirmEmail hash = do
  member <- asks appMember
  case member of
    Nothing -> do
      simplePage "Please Login to Confirm Your Account" mempty $ do
        script ! type_ "text/javascript" $ preEscapedString "jQuery(function () {V.loginPopup();});"
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
                email <- (fromJust . memberEmail) <$$> memberByNumber (memberNumber m)
                ip <- remoteAddr
                key <- fromJust <$> getOption "authtokenkey"
                authTok <- liftIO $ authToken (memberNumber m) (memberName m) email ip key
                setAuthCookie authTok
                redirect "http://www.vocabulink.com/"
        else error "Confirmation code does not match logged in user."

-- This is the page we redirect unconfirmed members to when they try to
-- interact with the site in a way that requires a confirmed email address.

confirmEmailPage :: App CGIResult
confirmEmailPage = do
  ref <- referrerOrVocabulink
  redirect' <- getInputDefault ref "redirect"
  simplePage "Email Confirmation Required" mempty $ do
    div ! id "central-column" $ do
      p "In order to interact with Vocabulink, \
        \you need to confirm your email address."
      p $ do
        string "If you haven't received a confirmation email or are having trouble, "
        a ! href "#" ! class_ "contact-us" $ "click here for support"
        string "."
      p $ do
        a ! href (stringValue redirect') $ "Click here to go back"
        string " to where you came from."

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
      res <- liftIO $ sendMail email "Vocabulink Password Reset" body
      case res of
        Nothing -> error "Failed to send password reset email."
        _       -> outputNothing
    _              -> error "No member exists with that email address. Please try again."

passwordResetPage :: String -> App CGIResult
passwordResetPage hash = do
  memberNo <- $(queryTuple' "SELECT member_no FROM password_reset_token \
                            \WHERE hash = {hash} AND expires > current_timestamp")
  case memberNo of
    Just mn -> simplePage "Change Your Password" [] $ do
                 form ! action (stringValue $ "/member/password/reset/" ++ hash)
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
      ip      <- remoteAddr
      member' <- memberByNumber mn
      case member' of
        Nothing     -> error "Failed to lookup member."
        Just member -> do
          key <- fromJust <$> getOption "authtokenkey"
          authTok <- liftIO $ authToken (memberNumber member) (memberName member) (memberEmail member) ip key
          setAuthCookie authTok
          redirect "http://www.vocabulink.com/"
    _       -> error "Invalid or expired password reset."
