% Copyright 2008, 2009, 2010 Chris Forno

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

\section{Members}

Most functionality on Vocabulink---such as review scheduling---is for
registered members only.

> module Vocabulink.Member (  login, logout, registerMember,
>                             getMemberNumber, getMemberName, getMemberEmail,
>                             confirmEmail, confirmEmailPage, memberSupport,
>                             UserContent(..) ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Form
> import Vocabulink.Html
> import Vocabulink.Member.AuthToken
> import Vocabulink.Page
> import Vocabulink.Utils

> import qualified Text.Blaze.Html5.Formlets as HF (input, textarea, checkbox, hidden, password)

> import Prelude hiding (div, span, id)

\subsection{Authentication}

To authenticate a member, we need their username and password. This is our
first example of a formlet. The formlet nicely encapsulates validating the
member's password against the database as part of normal formlet validation.

> loginForm :: String -> AppForm (String, String)
> loginForm ref = plug (\html -> do input ! type_ "hidden" ! name "redirect" ! value (stringValue ref)
>                                   table $ do
>                                     html
>                                     tfoot $ tabularSubmit "Login")
>                  ((,) <$> username <*> passwd "Password") `checkM`
>                    ensureM passMatch err
>   where passMatch (user, pwd) =
>           (fromJust . fromJust) <$> $(queryTuple'
>             "SELECT password_hash = crypt({pwd}, password_hash) \
>             \FROM member WHERE username = {user}")
>         err = "Username and password do not match (or don't exist)."

If a member authenticates correctly, we redirect them to either the frontpage
or whatever redirect parameter we've been given. This allows
|withRequiredMemberNumber| to function properly: getting the client to login
and then continuing where it left off.

> login :: App CGIResult
> login = do
>   ref    <- referrerOrVocabulink
>   redir  <- getInputDefault ref "redirect"
>   res    <- runForm (loginForm redir) $ Right mempty
>   key    <- fromJust <$> getOption "authtokenkey"
>   case res of
>     Left html -> simplePage "Login" [] $ do
>       p ! style "text-align: center" $ do
>         string "Not a member? "
>         a ! href "/member/signup" $ "Sign Up for free!"
>       html
>     Right (user, _) -> do
>       ip         <- remoteAddr
>       memberNo   <- getMemberNumber user
>       email      <- maybe (return Nothing) getMemberEmail memberNo
>       case memberNo of
>         Nothing  -> redirect redir
>         Just n   -> do
>           authTok <- liftIO $ authToken n user email ip key
>           setAuthCookie authTok
>           redirect redir

At the time of authentication, we have to fetch the member's number from the
database before it can be packed into their auth token. There may be a way to
put this step into password verification so that we don't need 2 queries.

> getMemberNumber :: String -> App (Maybe Integer)
> getMemberNumber memberName = $(queryTuple' "SELECT member_no FROM member \
>                                            \WHERE username = {memberName}")

> getMemberName :: Integer -> App (Maybe String)
> getMemberName memberNo = $(queryTuple' "SELECT username FROM member \
>                                        \WHERE member_no = {memberNo}")

> getMemberEmail :: Integer -> App (Maybe String)
> getMemberEmail memberNo = fromMaybe Nothing <$> $(queryTuple'
>   "SELECT email FROM member \
>   \WHERE member_no = {memberNo}")

To logout a member, we simply clear their auth cookie and redirect them
somewhere sensible. If you want to send a client somewhere other than the front
page after logout, add a @redirect@ query string or POST parameter.

> logout :: App CGIResult
> logout = do
>   deleteAuthCookie
>   ref <- referrerOrVocabulink
>   redirect =<< getInputDefault ref "redirect"

\subsection{New Members}

To register a new member we need their desired username, a password, and
optionally an email address.

> data Registration = Registration {  regUser   :: String,
>                                     regEmail  :: String,
>                                     regPass   :: String }

They must also agree to the Terms of Use.

> register :: AppForm Registration
> register = plug (\html -> table $ do
>                             html
>                             tfoot $ tabularSubmit "Sign Up")
>                 (reg  <$> uniqueUser
>                       <*> uniqueEmailAddress
>                       <*> passConfirmed
>                       <*> termsOfUse `check` ensure (== True)
>                             "You must agree to the Terms of Use.")
>   where reg u e pwd _ = Registration u e pwd

We're very permissive with usernames. They just need to be between 3 and 32
characters long.

Since this is a site about learning languages, we really want members to be
able to express themselves with their username. This may turn out to be a major
pain to deal with for things like URIs, but there's 1 way to find out...

> username :: AppForm String
> username = plug (tabularInput "Username") (HF.input Nothing) `check` ensures
>   [  ((>= 3)   . length  , "Your username must be 3 characters or longer."),
>      ((<= 32)  . length  , "Your username must be 32 characters or shorter.") ]

During registration, we want to make sure that the username the client is
trying to register with isn't already in use.

> uniqueUser :: AppForm String
> uniqueUser = username `checkM` ensureM valid err where
>   valid user = isNothing <$> $(queryTuple' "SELECT username FROM member \
>                                            \WHERE username ILIKE {user}")
>   err = "That username is unavailable."

Our password input is as permissive as our username input.

> passwd :: String -> AppForm String
> passwd l = plug (tabularInput l) (HF.password Nothing) `check` ensures
>   [  ((>=  4)   . length  , "Your password must be 4 characters or longer."),
>      ((<=  72)  . length  , "Your password must be 72 characters or shorter.") ]

During registration, we want the client to confirm their password, if for no
other reason than that it's common practice.

> passConfirmed :: AppForm String
> passConfirmed = fst <$> (passwords `check` ensure (\ (x, y) -> x == y) err) where
>   passwords = (,) <$> passwd "Password" <*> passwd "Password (confirm)"
>   err = "Passwords do not match."

To indicate acceptance of the Terms of Use, the member checks a box. This is
still a little bit awkward because the checkbox doesn't maintain its state if
validation fails. I'm hoping to fix that later when I understand formlets in
more depth.

> termsOfUse :: AppForm Bool
> termsOfUse = plug (\html -> tr $ td ! colspan "2" ! style "text-align: center" $ do
>                               html
>                               string " I agree to the "
>                               a ! href "/terms-of-use" $ "Terms of Use"
>                               string ".") (HF.checkbox Nothing)

We don't currently do any validation on email addresses other than to check if
the address is already in use. We need to check both the @member@ and
@member_confirmation@ relations so that we catch unconfirmed email addresses as
well.

> emailAddress :: AppForm String
> emailAddress = plug (tabularInput "Email address") (HF.input Nothing) `check`
>   ensures
>     [  ((/= ""), "Enter an email address."),
>        ((<= 320) . length, "Your email address must be \
>                            \320 characters or shorter.") ]

> uniqueEmailAddress :: AppForm String
> uniqueEmailAddress = emailAddress `checkM` ensureM valid err where
>   valid email = isNothing <$> $(queryTuple' "(SELECT email FROM member \
>                                              \WHERE email = {email}) \
>                                             \UNION \
>                                             \(SELECT email FROM member_confirmation \
>                                              \WHERE email = {email})")
>   err = "That email address is unavailable."

The registration process consists of a single form. Once the user has
registered, we log them in and redirect them to the front page.

> registerMember :: App CGIResult
> registerMember = do
>   res <- runForm register $ Right mempty
>   key <- fromJust <$> getOption "authtokenkey"
>   case res of
>     Left html -> simplePage "Sign Up for Vocabulink" mempty html
>     Right reg -> do
>       memberNo <- fromJust <$> $(queryTuple'
>         "INSERT INTO member (username, password_hash) \
>                     \VALUES ({regUser reg}, crypt({regPass reg}, gen_salt('bf'))) \
>         \RETURNING member_no")
>       ip <- remoteAddr
>       authTok <- liftIO $ authToken memberNo (regUser reg) Nothing ip key
>       setAuthCookie authTok
>       res' <- sendConfirmationEmail memberNo reg
>       case res' of
>         Nothing -> error "Registration failure (this is not your fault)."
>         Just _  -> redirect "/"

Once a user registers, they can log in. However, they won't be able to use most
member-specific functions until they've confirmed their email address. This is
to make sure that people cannot impersonate or spam others.

Email confirmation consists of generating a unique random string and emailing
it to the member as a hyperlink. Once they click the hyperlink we consider the
email address confirmed.

> sendConfirmationEmail :: Integer -> Registration -> App (Maybe ())
> sendConfirmationEmail memberNo reg = do
>   hash <- fromJust <$> $(queryTuple'
>     "INSERT INTO member_confirmation (member_no, hash, email) \
>                              \VALUES ({memberNo}, md5(random()::text), {regEmail reg}) \
>     \RETURNING hash")
>   let email = unlines [
>                "Welcome to Vocabulink.",
>                "",
>                "Click http://www.vocabulink.com/member/confirmation/" ++
>                hash ++ " to confirm your email address." ]
>   res <- liftIO $ sendMail (regEmail reg) "Welcome to Vocabulink" email
>   maybe (return Nothing) (\_ -> do $(execute' "UPDATE member_confirmation \
>                                               \SET email_sent = current_timestamp \
>                                               \WHERE member_no = {memberNo}")
>                                    return $ Just ()) res

This is the place that the dispatcher will send the client to if they click the
hyperlink in the email. If confirmation is successful it redirects them to some
hopefully useful page.

Once we have confirmed the member's email, we need to set a new auth token
cookie for them that contains their gravatar hash.

> confirmEmail :: String -> App CGIResult
> confirmEmail hash = do
>   memberNo <- asks appMemberNo
>   case memberNo of
>     Nothing  -> redirect =<< reversibleRedirect "/member/login"
>     Just n   -> do
>       match <- maybe False fromJust <$> $(queryTuple'
>         "SELECT hash = {hash} FROM member_confirmation \
>         \WHERE member_no = {n}")
>       if match
>         then do h <- asks appDB
>                 liftIO $ withTransaction h $ do
>                   $(execute "UPDATE member SET email = \
>                              \(SELECT email FROM member_confirmation \
>                               \WHERE member_no = {n}) \
>                             \WHERE member_no = {n}") h
>                   $(execute "DELETE FROM member_confirmation \
>                             \WHERE member_no = {n}") h
>                 email <- getMemberEmail n
>                 memberName <- fromJust <$> asks appMemberName
>                 ip <- remoteAddr
>                 key <- fromJust <$> getOption "authtokenkey"
>                 authTok <- liftIO $ authToken n memberName email ip key
>                 setAuthCookie authTok
>                 redirect =<< referrerOrVocabulink
>         else confirmEmailPage

This is the page we redirect unconfirmed members to when they try to interact
with the site in a way that requires a confirmed email address.

> confirmEmailPage :: App CGIResult
> confirmEmailPage = do
>   ref <- referrerOrVocabulink
>   redirect' <- getInputDefault ref "redirect"
>   support <- getSupportForm $ Just redirect'
>   simplePage "Email Confirmation Required" mempty $ do
>     div ! id "central-column" $ do
>       p $ "In order to interact with Vocabulink, \
>           \you need to confirm your email address."
>       p $ "If you haven't received a confirmation email \
>           \or are having trouble, let us know."
>       support
>       p $ do
>         a ! href (stringValue redirect') $ "Click here to go back"
>         string " to where you came from."

At some point members (or non-members) may have difficulties with the site. I
debated about giving out an email address, but I suspect that filtering for
spam among support requests might be pretty difficult. I also don't want to go
dead to support request email. So this uses the tried and true contact form
method.

> supportForm :: Maybe String -> App (AppForm (String, String, String))
> supportForm redirect' = do
>   ref <- referrerOrVocabulink
>   email <- asks appMemberEmail
>   let  redirect'' = fromMaybe ref redirect'
>        emailInput = case email of
>                       Nothing  -> plug (tabularInput "Email Address") (HF.input Nothing) `check`
>                                     ensures
>                                       [((/= ""), "We need an email address to contact you at.")]
>                       Just _   -> HF.hidden email
>   return $ plug (\html -> table $ do
>                             html
>                             tfoot $ tabularSubmit "Get Support")
>              ((,,)  <$>  emailInput
>                     <*>  plug (tabularInput "Problem") (HF.textarea Nothing Nothing Nothing) `check` ensures
>                            [((/= ""),  "It would help us to know \
>                                        \what the problem you're experiencing is ;).")]
>                     <*>  HF.hidden (Just redirect''))

Get a fresh support form (don't attempt to run it).

> getSupportForm :: Maybe String -> App Html
> getSupportForm redirect' = do
>   (_, html) <- runForm' =<< supportForm redirect'
>   return $ form ! action "/member/support" ! method "post" $ html

And finally, here is the actual support page. It's not just for member support.
If the client isn't logged in it will ask for a contact email address.

Because support is so critical, in case there's an error submitting the support
form we fall back to a secondary (disposable) support address.

> memberSupport :: App CGIResult
> memberSupport = do
>   form' <- supportForm =<< getInput "redirect"
>   res <- runForm form' $ Right mempty
>   case res of
>     Left html -> simplePage "Need Help?" mempty $ do
>       div ! id "central-column" $ do
>         p ! style "text-align: center" $ do
>           string "Have you checked the "
>           a ! href "/forum/help" $ "help forum"
>           string "?"
>       html
>     Right (email, problem, redirect') -> do
>       supportAddress <- fromJust <$> getOption "supportaddress"
>       res' <- liftIO $ sendMail supportAddress "Support Request" $
>                 unlines [  "Email: " ++ email,
>                            "Problem: " ++ problem ]
>       case res' of
>         Nothing  -> error "Error sending support request. \
>                           \Please contact support@vocabulink.com for support."
>         Just _   -> simplePage "Support Request Sent" mempty $ do
>                       div ! id "central-column" $ do
>                         p $ "Your support request was sent successfully."
>                         p $ do
>                           a ! href (stringValue redirect') $ "Click here to go back"
>                           string " to where you came from."

\subsection{Permissions}

> class UserContent u where
>   canView    :: u -> App Bool
>   canEdit    :: u -> App Bool
>   canDelete  :: u -> App Bool
