> module Vocabulink.Member (login, logout, registerMember) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member.AuthToken
> import Vocabulink.Utils

> import qualified Text.XHtml.Strict.Formlets as F

\section{Authentication}

To authenticate a member, we need their username and password.

> loginForm :: AppForm (String, String)
> loginForm = ((,) <$> username <*> passwd "Password") `checkM`
>             ensureM passMatch err
>   where passMatch (u, p) = do
>           valid <- queryValue'  "SELECT password_hash = crypt(?, password_hash) \
>                                 \FROM member WHERE username = ?"
>                                 [toSql p, toSql u]
>           case valid of
>             Nothing  -> error "Internal Authentication Error \
>                               \(This is not your fault.)"
>             Just v   -> return $ maybe False fromSql v
>         err = "Username and password do not match (or don't exist)."

If a member authenticates correctly, we redirect them to either the frontpage
or whatever redirect parameter we've been given. This allows
|withRequiredMemberNumber| to function properly: getting the client to login
and then continuing where it left off.

> login :: App CGIResult
> login = do
>   res <- runForm loginForm "Log In"
>   case res of
>     Left xhtml -> simplePage "Login" []
>       [  xhtml,
>          paragraph << "Not a member? " +++
>                           anchor ! [href "/member/join"] << "Join!" ]
>     Right (user, _) -> do
>       redirect'  <- getInputDefault "/" "redirect"
>       ip         <- remoteAddr
>       memberNo   <- getMemberNumber user
>       case memberNo of
>         Nothing  -> redirect redirect'
>         Just n   -> do
>           setAuthCookie n user ip
>           redirect redirect'

At the time of authentication, we have to fetch the member's number from the
database before it can be packed into their auth token. There may be a way to
put this step into password verification so that we don't need 2 queries.

> getMemberNumber :: String -> App (Maybe Integer)
> getMemberNumber user = do
>   n <- queryValue' "SELECT member_no FROM member \
>                    \WHERE username = ?" [toSql user]
>   case n of
>     Nothing  -> error "Failed to retrieve member number from username."
>     Just n'  -> return $ fromSql <$> n'

To logout the member, we simply clear their auth cookie and redirect them
somewhere sensible. If you want to send a client somewhere other than the front
page after logout, add a @redirect@ query string or POST parameter.

> logout :: App CGIResult
> logout = do
>   deleteAuthCookie
>   redirect =<< getInputDefault "/" "redirect"

\subsection{New Members}

To register a new member we need their desired username, a password, and
optionally an email address.

> data Registration = Registration {  regUser   :: String,
>                                     regEmail  :: Maybe String,
>                                     regPass   :: String }

> register :: AppForm Registration
> register = Registration <$> uniqueUser <*> nothingIfNull emailAddress <*> passConfirmed

We're very permissive with usernames. They just need to be between 3 and 32
characters long.

> username :: AppForm String
> username = ("Username" `formLabel` F.input Nothing) `check` ensures
>   [  ((>= 3)   . length  , "Your username must be 3 characters or longer."),
>      ((<= 32)  . length  , "Your username must be 32 characters or shorter.") ]

During registration, we want to make sure that the username the client is
trying to register with isn't already in use.

> uniqueUser :: AppForm String
> uniqueUser = username `checkM` ensureM valid err where
>   valid user = (== Just Nothing) <$>
>                   queryValue' "SELECT username FROM member \
>                               \WHERE username = ?" [toSql user]
>   err = "That username is unavailable."

Our password input is as permissive as our username input.

TODO: Where does the 72-character password limit come from?
TODO: Add strong password test?

> passwd :: String -> AppForm String
> passwd l = (l `formLabel` F.password Nothing) `check` ensures
>   [  ((>=  6)   . length  , "Your password must be 6 characters or longer."),
>      ((<=  72)  . length  , "Your password must be 72 characters or shorter.") ]

During registration, we want the client to confirm their password, if for no
other reason than that it's common practice.

> passConfirmed :: AppForm String
> passConfirmed = fst <$> (passwords `check` ensure equal err) where
>   passwords = (,) <$> passwd "Password" <*> passwd "Password (confirm)"
>   equal (a,b) = a == b
>   err = "Passwords do not match."

We don't currently do any validation on email addresses.

> emailAddress :: AppForm String
> emailAddress = "Email address" `formLabel` F.input Nothing

Create a page with a new user form or register the user and redirect them to
the front page.

> registerMember :: App CGIResult
> registerMember = do
>   res <- runForm register "Join"
>   case res of
>     Left xhtml  -> simplePage "Join Vocabulink" [] [xhtml]
>     Right reg   -> do
>       memberNo <- quickInsertNo'
>         "INSERT INTO member (username, email, password_hash) \
>         \VALUES (?, ?, crypt(?, gen_salt('bf')))"
>         [toSql (regUser reg), toSql (regEmail reg), toSql (regPass reg)]
>         "member_member_no_seq"
>       case memberNo of
>         Nothing  -> error "Registration failure (this is not your fault)."
>         Just n   -> do
>           ip <- remoteAddr
>           setAuthCookie n (regUser reg) ip
>           redirect "/"
