> module Vocabulink.Member (  login, logout, registerMember,
>                             getMemberNumber, getMemberName,
>                             confirmMembership, emailConfirmationPage,
>                             memberSupport ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member.AuthToken
> import Vocabulink.Utils

> import Control.Exception (try)
> import qualified Text.XHtml.Strict.Formlets as F
> import System.Cmd (system)
> import System.Exit (ExitCode(..))

\section{Authentication}

To authenticate a member, we need their username and password.

> loginForm :: String -> AppForm (String, String)
> loginForm ref = plug (\xhtml -> hidden "redirect" ref +++
>                                 table <<
>                                   (xhtml +++ tfoot << tabularSubmit "Login"))
>                  ((,) <$> username <*> passwd "Password") `checkM`
>                    ensureM passMatch err
>   where passMatch (u, p) = do
>           valid <- queryValue'  "SELECT password_hash = crypt(?, password_hash) \
>                                 \FROM member WHERE username = ?"
>                                 [toSql p, toSql u]
>           return $ case valid of
>             Nothing  -> False
>             Just v   -> fromSql v
>         err = "Username and password do not match (or don't exist)."

If a member authenticates correctly, we redirect them to either the frontpage
or whatever redirect parameter we've been given. This allows
|withRequiredMemberNumber| to function properly: getting the client to login
and then continuing where it left off.

> login :: App CGIResult
> login = do
>   ref    <- referrerOrVocabulink
>   redir  <- getInputDefault ref "redirect"
>   res    <- runForm (loginForm redir) (Right noHtml)
>   case res of
>     Left xhtml -> simplePage "Login" []
>       [  paragraph ! [thestyle "text-align: center"] <<
>            [  stringToHtml "Not a member? ",
>               anchor ! [href "/member/signup"] << "Sign Up for free!" ],
>          xhtml ]
>     Right (user, _) -> do
>       ip         <- remoteAddr
>       memberNo   <- getMemberNumber user
>       case memberNo of
>         Nothing  -> redirect redir
>         Just n   -> do
>           setAuthCookie n user ip
>           redirect redir

At the time of authentication, we have to fetch the member's number from the
database before it can be packed into their auth token. There may be a way to
put this step into password verification so that we don't need 2 queries.

> getMemberNumber :: String -> App (Maybe Integer)
> getMemberNumber user = do
>   n <- queryValue' "SELECT member_no FROM member \
>                    \WHERE username = ?" [toSql user]
>   return $ maybe Nothing fromSql n

> getMemberName :: Integer -> App (Maybe String)
> getMemberName number = do
>   maybe Nothing fromSql <$> queryValue' "SELECT username FROM member \
>                                         \WHERE member_no = ?" [toSql number]

To logout the member, we simply clear their auth cookie and redirect them
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

> register :: AppForm Registration
> register = plug (\xhtml -> table << (xhtml +++ tfoot << tabularSubmit "Sign Up"))
>                 (Registration  <$> uniqueUser
>                                <*> uniqueEmailAddress
>                                <*> passConfirmed)

We're very permissive with usernames. They just need to be between 3 and 32
characters long.

> username :: AppForm String
> username = (plug (tabularInput "Username") $ F.input Nothing) `check` ensures
>   [  ((>= 3)   . length  , "Your username must be 3 characters or longer."),
>      ((<= 32)  . length  , "Your username must be 32 characters or shorter.") ]

During registration, we want to make sure that the username the client is
trying to register with isn't already in use.

> uniqueUser :: AppForm String
> uniqueUser = username `checkM` ensureM valid err where
>   valid user = do vs <- queryTuples'  "SELECT username FROM member \
>                                       \WHERE username ILIKE ?" [toSql user]
>                   return $ maybe False (== []) vs
>   err = "That username is unavailable."

Our password input is as permissive as our username input.

> passwd :: String -> AppForm String
> passwd l = (plug (tabularInput l) $ F.password Nothing) `check` ensures
>   [  ((>=  6)   . length  , "Your password must be 6 characters or longer."),
>      ((<=  72)  . length  , "Your password must be 72 characters or shorter.") ]

During registration, we want the client to confirm their password, if for no
other reason than that it's common practice.

> passConfirmed :: AppForm String
> passConfirmed = fst <$> (passwords `check` ensure equal err) where
>   passwords = (,) <$> passwd "Password" <*> passwd "Password (confirm)"
>   equal (a,b) = a == b
>   err = "Passwords do not match."

We don't currently do any validation on email addresses other than to check if
the address is already in use.

> uniqueEmailAddress :: AppForm String
> uniqueEmailAddress = emailAddress `checkM` ensureM valid err where
>   valid email = do  vs <- queryTuples'  "(SELECT email FROM member \
>                                          \WHERE email = ?) \
>                                         \UNION \
>                                         \(SELECT email FROM member_confirmation \
>                                          \WHERE email = ?)"
>                                         [toSql email, toSql email]
>                     return $ maybe False (== []) vs
>   err = "That email address is unavailable."

> emailAddress :: AppForm String
> emailAddress = (plug (tabularInput "Email address") $ F.input Nothing) `check` ensures
>   [  ((/= ""), "Enter an email address.") ]

Create a page with a new user form or register the user and redirect them to
the front page.

> registerMember :: App CGIResult
> registerMember = do
>   res <- runForm register $ Right noHtml
>   case res of
>     Left xhtml  -> simplePage "Sign Up for Vocabulink" [] [xhtml]
>     Right reg   -> do
>       memberNo <- quickInsertNo'
>         "INSERT INTO member (username, password_hash) \
>         \VALUES (?, crypt(?, gen_salt('bf')))"
>         [toSql (regUser reg), toSql (regPass reg)]
>         "member_member_no_seq"
>       case memberNo of
>         Nothing  -> error "Registration failure (this is not your fault)."
>         Just n   -> do
>           ip <- remoteAddr
>           setAuthCookie n (regUser reg) ip
>           sendConfirmationEmail n reg
>           redirect "/"

> sendConfirmationEmail :: Integer -> Registration -> App (Maybe ())
> sendConfirmationEmail memberNo r = do
>   quickStmt'  "INSERT INTO member_confirmation \
>               \(member_no, hash, email) VALUES \
>               \(?, md5(random()::text), ?)"
>               [toSql memberNo, toSql (regEmail r)]
>   hash <- queryValue'  "SELECT hash FROM member_confirmation \
>                        \WHERE member_no = ?" [toSql memberNo]
>   case hash of
>     Nothing  -> return Nothing
>     Just h   -> do
>       let email = unlines [
>                     "From: vocabulink@vocabulink.com",
>                     "To: <" ++ (regEmail r) ++ ">",
>                     "Subject: Welcome to Vocabulink",
>                     "",
>                     "Welcome to Vocabulink.",
>                     "",
>                     "Click http://www.vocabulink.com/member/confirmation/" ++
>                     (fromSql h) ++ " to confirm your email address." ]
>       res <- sendMail (regEmail r) "Welcome to Vocabulink" email
>       maybe (return Nothing) (\_ -> quickStmt'
>         "UPDATE member_confirmation \
>         \SET email_sent = current_timestamp \
>         \WHERE member_no = ?" [toSql memberNo]) res

> sendMail :: String -> String -> String -> App (Maybe ())
> sendMail to subject body = do
>   let body' = unlines [  "From: vocabulink@vocabulink.com",
>                          "To: <" ++ to ++ ">",
>                          "Subject: " ++ subject,
>                          "",
>                          body ]
>   res <- liftIO $ try $ system $
>            (  "echo -e \""   ++ body'  ++ "\" | \
>               \sendmail \""  ++ to     ++ "\"" )
>   case res of
>     Right ExitSuccess  -> return $ Just ()
>     _                  -> return Nothing

> confirmMembership :: String -> App CGIResult
> confirmMembership hash = do
>   memberNo <- asks appMemberNo
>   case memberNo of
>     Nothing  -> redirect =<< reversibleRedirect "/member/login"
>     Just n   -> do
>       match <- queryValue'  "SELECT ? = hash FROM member_confirmation \
>                             \WHERE member_no = ?" [toSql hash, toSql n]
>       let match' = maybe False fromSql match
>       case match' of
>         False  -> emailConfirmationPage
>         True   -> do
>           res <- withTransaction' $ do
>             runStmt'  "UPDATE member SET email = \
>                          \(SELECT email FROM member_confirmation \
>                           \WHERE member_no = ?) \
>                       \WHERE member_no = ?" [toSql n, toSql n]
>             runStmt'  "DELETE FROM member_confirmation \
>                       \WHERE member_no = ?" [toSql n]
>           case res of
>             Nothing  -> emailConfirmationPage
>             Just _   -> redirect =<< referrerOrVocabulink

> emailConfirmationPage :: App CGIResult
> emailConfirmationPage = do
>   ref <- referrerOrVocabulink
>   redirect' <- getInputDefault ref "redirect"
>   support <- getSupportForm $ Just redirect'
>   simplePage "Email Confirmation Required" [] [
>     thediv ! [identifier "central-column"] << [
>       paragraph << "In order to interact with Vocabulink, \
>                    \you need to confirm your email address.",
>       paragraph << "If you haven't received a confirmation email \
>                    \or are having trouble, let us know.",
>       support,
>       paragraph << [
>         anchor ! [href redirect'] << "Click here to go back",
>         stringToHtml " to where you came from." ] ] ]

At some point members (or clients) may have difficulties with the site. I
debated about giving out an email address, but I suspect that filtering for
spam among support requests might be pretty difficult. I also don't want to go
dead to support request email.

So, this uses the tried-and-true contact form method.

> supportForm :: Maybe String -> App (AppForm (String, String, String))
> supportForm redirect' = do
>   ref <- referrerOrVocabulink
>   email <- asks appMemberEmail
>   let  redirect'' = fromMaybe ref redirect'
>        emailInput = case email of
>                       Nothing  -> plug (tabularInput "Email Address") $ F.input Nothing `check` ensures
>                                           [((/= ""), "We need an email address to contact you at.")]
>                       Just _   -> F.hidden email
>   return $ plug (\xhtml -> table << [
>                    xhtml, tfoot << tabularSubmit "Get Support" ])
>              ((,,)  <$>  emailInput
>                    <*>  (plug (tabularInput "Problem") $ F.textarea Nothing) `check` ensures
>                            [((/= ""), "It would help us to know what the problem you're experiencing is ;).")]
>                    <*>  F.hidden (Just redirect''))

Get a fresh support form (don't attempt to run it).

> getSupportForm :: Maybe String -> App Html
> getSupportForm redirect' = do
>   (_, xhtml) <- runForm' =<< supportForm redirect'
>   return $ form ! [action "/member/support", method "POST"] << xhtml

> memberSupport :: App CGIResult
> memberSupport = do
>   form' <- supportForm =<< getInput "redirect"
>   res <- runForm form' (Right noHtml)
>   case res of
>     Left xhtml -> simplePage "Need Help?" []
>       [  thediv ! [identifier "central-column"] <<
>            xhtml ]
>     Right (email, problem, redirect') -> do
>       supportAddress <- fromJust <$> getOption "staticDir"
>       res' <- sendMail supportAddress "Support Request" $
>                 unlines [  "Email: " ++ email,
>                            "Problem: " ++ problem ]
>       case res' of
>         Nothing  -> error "Error sending support request. \
>                           \Please contact vocabulink.jekor@spamgourmet.com for support."
>         Just _   -> simplePage "Support Request Sent" []
>                       [  thediv ! [identifier "central-column"] << [
>                            paragraph << "Your support request was sent successfully.",
>                            paragraph << [
>                              anchor ! [href redirect'] << "Click here to go back",
>                              stringToHtml " to where you came from." ] ] ]
