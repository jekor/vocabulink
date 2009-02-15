> module Vocabulink.Member (  registerMember, addMember, loginPage,
>                             login, logout) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member.AuthToken
> import Vocabulink.Utils

> import Control.Applicative (Applicative)
> import Control.Applicative.Error
> import Control.Arrow (second)
> import Text.Formlets (  runFormState, plug, nothingIfNull,
>                         check, ensure, ensures, checkM, ensureM)
> import qualified Text.XHtml.Strict.Formlets as F

\section{Authentication}

> loginPage :: App CGIResult
> loginPage = do
>   referer'   <- refererOrVocabulink
>   redirect'  <- getInputDefault referer' "redirect"
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

To authenticate a member, we just need their username and password. We also
check to see if anyone has passed a @redirect@ parameter so that we can adjust
the redirect following successful authentication correctly.

> login :: App CGIResult
> login = do
>   username   <- getRequiredInput "username"
>   memberNo   <- getMemberNumber username
>   passwd     <- getRequiredInput "password"
>   ref        <- refererOrVocabulink
>   redirect'  <- getInputDefault ref "redirect"
>   ip         <- remoteAddr
>   valid <- validPassword username passwd
>   if not valid -- TODO: We need to add an error indicator.
>     then redirect "/member/login"
>     else do
>       case memberNo of
>         Nothing  -> redirect redirect'
>         Just n   -> do
>           setAuthCookie n username ip
>           redirect redirect'

Login attempts to match the username and password supplied against the
information in the database.

> validPassword :: String -> String -> App (Bool)
> validPassword username passwd = do
>   valid <- queryValue'  "SELECT password_hash = crypt(?, password_hash) \
>                         \FROM member WHERE username = ?"
>                         [toSql passwd, toSql username]
>   case valid of
>     Nothing  -> error "Internal authentication error (this is not your fault)"
>     Just v   -> return $ maybe False fromSql v

At the time of authentication, we do not have any member information because
we've set no authentication cookie yet. This is where we get it from before we
put it into the auth token.

> getMemberNumber :: String -> App (Maybe Integer)
> getMemberNumber username = do
>   n <- queryValue' "SELECT member_no FROM member \
>                    \WHERE username = ?" [toSql username]
>   case n of
>     Nothing  -> error "Failed to retrieve member number from username."
>     Just n'  -> return $ fromSql <$> n'

To logout the member, we simply clear their auth cookie and redirect them
somewhere sensible. If you want to send a client somewhere other than the front
page after logout, add a @redirect@ query string or POST parameter.

> logout :: App CGIResult
> logout = do
>   redirect' <- getInputDefault "/" "redirect"
>   deleteAuthCookie
>   redirect redirect'

\subsection{New Members}

> data Registration = Registration {  regUsername  :: String,
>                                     regEmail     :: Maybe String,
>                                     regPassword  :: String }

> label' :: Monad m => String -> F.XHtmlForm m a -> F.XHtmlForm m a
> label' l = plug (\xhtml -> paragraph << (label << (l ++ ": ") +++ xhtml))

> register :: F.XHtmlForm (AppT IO) Registration
> register = Registration <$> uniqueUser <*> nothingIfNull emailAddress <*> passConfirmed

> user :: F.XHtmlForm (AppT IO) String
> user = ("Username" `label'` F.input Nothing) `check` ensures
>   [  ((>= 3)   . length, "Your username must be 3 characters or longer."),
>      ((<= 32)  . length, "Your username must be 32 characters or shorter.") ]

> uniqueUser :: F.XHtmlForm (AppT IO) String
> uniqueUser = user `checkM` ensureM valid err where
>   valid name' = (== Just Nothing) <$>
>                   queryValue' "SELECT username FROM member \
>                               \WHERE username = ?" [toSql name']
>   err = "That username is unavailable."

> emailAddress :: F.XHtmlForm (AppT IO) String
> emailAddress = "Email address" `label'` F.input Nothing

TODO: Where does the 72-character password limit come from?

> pass :: String -> F.XHtmlForm (AppT IO) String
> pass l = (l `label'` F.password Nothing) `check` ensures
>   [  ((>= 6)   . length, "Your password must be 6 characters or longer."),
>      ((<= 72)  . length, "Your password must be 72 characters or shorter.") ]

> passConfirmed :: F.XHtmlForm (AppT IO) String
> passConfirmed = fst <$> (passwords `check` ensure equal err) where
>   passwords = (,) <$> pass "Password" <*> pass "Password (confirm)"
>   equal (a,b) = a == b
>   err = "Passwords do not match."

> registerMember :: App CGIResult
> registerMember = do
>   env <- map (second Left) <$> getInputs
>   let (res, markup, _) = runFormState env "" register
>   status <- res
>   xhtml <- markup
>   meth <- requestMethod
>   case status of
>     Failure f    -> stdPage "Join Vocabulink" [] $
>                       [  h1 << "Join Vocabulink",
>                          case meth of
>                            "GET" ->  noHtml
>                            _     ->  unordList f,
>                          form ! [method "POST"] <<
>                            [  xhtml,
>                               submit "" "Join"] ]
>     Success reg  -> do
>       memberNo <- addMember (regUsername reg) (regPassword reg) (regEmail reg)
>       case memberNo of
>         Nothing  -> error "Registration failure (this is not your fault)."
>         Just n   -> do
>           ip <- remoteAddr
>           setAuthCookie n (regUsername reg) ip
>           redirect "/"

> addMember :: String -> String -> Maybe String -> App (Maybe Integer)
> addMember username passwd email =
>   quickInsertNo' "INSERT INTO member (username, email, password_hash) \
>                  \VALUES (?, ?, crypt(?, gen_salt('bf')))"
>                  [toSql username, toSql email, toSql passwd]
>                  "member_member_no_seq"
