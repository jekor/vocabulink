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

module Vocabulink.Member.Registration ( registerMember, confirmEmail, confirmEmailPage
                                      , login, logout
                                      ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Form
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Member.Auth
import Vocabulink.Page
import Vocabulink.Support
import Vocabulink.Utils

import qualified Text.Blaze.Html5.Formlets as HF

import Prelude hiding (div, id, span)

data Registration = Registration { regUser  :: String
                                 , regEmail :: String
                                 , regPass  :: String
                                 }

register :: AppForm Registration
register = plug (\html -> table $ do
                            html
                            tfoot $ tabularSubmit "Sign Up")
                (reg  <$> uniqueUser
                      <*> uniqueEmailAddress
                      <*> passConfirmed
                      <*> termsOfUse `check` ensure (== True)
                            "You must agree to the Terms of Use.")
  where reg u e pwd _ = Registration u e pwd

-- We're very permissive with usernames. They just need to be between 3 and 32
-- characters long.

-- Since this is a site about learning languages, we really want members to be
-- able to express themselves with their username. This may turn out to be a
-- major pain to deal with for things like URIs, but there's 1 way to find
-- out...

username :: AppForm String
username = plug (tabularInput "Username") (HF.input Nothing) `check` ensures
  [  ((>= 3)   . length  , "Your username must be 3 characters or longer."),
     ((<= 32)  . length  , "Your username must be 32 characters or shorter.") ]

-- During registration, we want to make sure that the username the client is
-- trying to register with isn't already in use.

uniqueUser :: AppForm String
uniqueUser = username `checkM` ensureM valid err where
  valid user = isNothing <$> $(queryTuple' "SELECT username FROM member \
                                           \WHERE username ILIKE {user}")
  err = "That username is unavailable."

-- Our password input is as permissive as our username input.

passwd :: String -> AppForm String
passwd l = plug (tabularInput l) (HF.password Nothing) `check` ensures
  [  ((>=  4)   . length  , "Your password must be 4 characters or longer."),
     ((<=  72)  . length  , "Your password must be 72 characters or shorter.") ]

-- During registration, we want the client to confirm their password, if for no
-- other reason than that it's common practice.

passConfirmed :: AppForm String
passConfirmed = fst <$> (passwords `check` ensure (\ (x, y) -> x == y) err) where
  passwords = (,) <$> passwd "Password" <*> passwd "Password (confirm)"
  err = "Passwords do not match."

-- To indicate acceptance of the Terms of Use, the member checks a box. This is
-- still a little bit awkward because the checkbox doesn't maintain its state
-- if validation fails. I'm hoping to fix that later when I understand formlets
-- in more depth.

termsOfUse :: AppForm Bool
termsOfUse = plug (\html -> tr $ td ! colspan "2" ! style "text-align: center" $ do
                              html
                              string " I agree to the "
                              a ! href "/terms-of-use" $ "Terms of Use"
                              string ".") (HF.checkbox Nothing)

-- We don't currently do any validation on email addresses other than to check
-- if the address is already in use. We need to check both the @member@ and
-- @member_confirmation@ relations so that we catch unconfirmed email addresses
-- as well.

emailAddress :: AppForm String
emailAddress = plug (tabularInput "Email address") (HF.input Nothing) `check`
  ensures
    [  ((/= ""), "Enter an email address."),
       ((<= 320) . length, "Your email address must be \
                           \320 characters or shorter.") ]

uniqueEmailAddress :: AppForm String
uniqueEmailAddress = emailAddress `checkM` ensureM valid err where
  valid email = isNothing <$> $(queryTuple' "(SELECT email FROM member \
                                             \WHERE email = {email}) \
                                            \UNION \
                                            \(SELECT email FROM member_confirmation \
                                             \WHERE email = {email})")
  err = "That email address is unavailable."

-- The registration process consists of a single form. Once the user has
-- registered, we log them in and redirect them to the front page.

registerMember :: App CGIResult
registerMember = do
  member <- asks appMember
  case member of
    Just _  -> simplePage "You're Already Logged In" mempty mempty
    Nothing -> do
      res <- runForm register $ Right mempty
      key <- fromJust <$> getOption "authtokenkey"
      case res of
        Left html -> simplePage "Sign Up for Vocabulink" mempty html
        Right reg -> do
          memberNo <- fromJust <$> $(queryTuple'
            "INSERT INTO member (username, password_hash) \
                        \VALUES ({regUser reg}, crypt({regPass reg}, gen_salt('bf'))) \
            \RETURNING member_no")
          ip <- remoteAddr
          authTok <- liftIO $ authToken memberNo (regUser reg) Nothing ip key
          setAuthCookie authTok
          res' <- sendConfirmationEmail memberNo reg
          case res' of
            Nothing -> error "Registration failure (this is not your fault)."
            Just _  -> redirect "/"

-- Once a user registers, they can log in. However, they won't be able to use
-- most member-specific functions until they've confirmed their email address.
-- This is to make sure that people cannot impersonate or spam others.

-- Email confirmation consists of generating a unique random string and
-- emailing it to the member as a hyperlink. Once they click the hyperlink we
-- consider the email address confirmed.

sendConfirmationEmail :: Integer -> Registration -> App (Maybe ())
sendConfirmationEmail memberNo reg = do
  hash <- fromJust <$> $(queryTuple'
    "INSERT INTO member_confirmation (member_no, hash, email) \
                             \VALUES ({memberNo}, md5(random()::text), {regEmail reg}) \
    \RETURNING hash")
  let email = unlines [
               "Welcome to Vocabulink.",
               "",
               "Click http://www.vocabulink.com/member/confirmation/" ++
               hash ++ " to confirm your email address." ]
  res <- liftIO $ sendMail (regEmail reg) "Welcome to Vocabulink" email
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
    Nothing -> redirect =<< reversibleRedirect "/member/login"
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
                email <- (fromJust . memberEmail) <$$> (memberByNumber $ memberNumber m)
                ip <- remoteAddr
                key <- fromJust <$> getOption "authtokenkey"
                authTok <- liftIO $ authToken (memberNumber m) (memberName m) email ip key
                setAuthCookie authTok
                redirect =<< referrerOrVocabulink
        else confirmEmailPage

-- This is the page we redirect unconfirmed members to when they try to
-- interact with the site in a way that requires a confirmed email address.

confirmEmailPage :: App CGIResult
confirmEmailPage = do
  ref <- referrerOrVocabulink
  redirect' <- getInputDefault ref "redirect"
  support <- getSupportForm $ Just redirect'
  simplePage "Email Confirmation Required" mempty $ do
    div ! id "central-column" $ do
      p $ "In order to interact with Vocabulink, \
          \you need to confirm your email address."
      p $ "If you haven't received a confirmation email \
          \or are having trouble, let us know."
      support
      p $ do
        a ! href (stringValue redirect') $ "Click here to go back"
        string " to where you came from."

-- TODO: Move this all into AJAX.

-- To authenticate a member, we need their username and password. This is our
-- first example of a formlet. The formlet nicely encapsulates validating the
-- member's password against the database as part of normal formlet validation.

loginForm :: String -> AppForm (String, String)
loginForm ref = plug (\html -> do input ! type_ "hidden" ! name "redirect" ! value (stringValue ref)
                                  table $ do
                                    html
                                    tfoot $ tabularSubmit "Login")
                 ((,) <$> username <*> passwd "Password") `checkM`
                   ensureM passMatch err
  where passMatch (user, pwd) =
          maybe False fromJust <$> $(queryTuple'
            "SELECT password_hash = crypt({pwd}, password_hash) \
            \FROM member WHERE username = {user}")
        err = "Username and password do not match (or don't exist)."

-- If a member authenticates correctly, we redirect them to either the
-- frontpage or whatever redirect parameter we've been given. This allows
-- |withRequiredMember| to function properly: getting the client to login and
-- then continuing where it left off.

login :: App CGIResult
login = do
  ref   <- referrerOrVocabulink
  redir <- getInputDefault ref "redirect"
  res   <- runForm (loginForm redir) $ Right mempty
  key   <- fromJust <$> getOption "authtokenkey"
  case res of
    Left html -> simplePage "Login" [] $ do
      p ! style "text-align: center" $ do
        string "Not a member? "
        a ! href "/member/signup" $ "Sign Up for free!"
      html
    Right (user, _) -> do
      ip      <- remoteAddr
      member' <- memberByName user
      case member' of
        Nothing     -> redirect redir
        Just member -> do
          authTok <- liftIO $ authToken (memberNumber member) user (memberEmail member) ip key
          setAuthCookie authTok
          redirect redir

-- To logout a member, we simply clear their auth cookie and redirect them
-- somewhere sensible. If you want to send a client somewhere other than the
-- front page after logout, add a @redirect@ query string or POST parameter.

logout :: App CGIResult
logout = do
  deleteAuthCookie
  ref <- referrerOrVocabulink
  redirect =<< getInputDefault ref "redirect"
