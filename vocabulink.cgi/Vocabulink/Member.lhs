> module Vocabulink.Member (  newMemberPage, addMember, loginPage,
>                             login, logout) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member.AuthToken
> import Vocabulink.Utils

\subsection{Helpers}


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
>     Just n'  -> return $ fmap fromSql n'

To logout the member, we simply clear their auth cookie and redirect them
somewhere sensible. If you want to send a client somewhere other than the front
page after logout, add a @redirect@ query string or POST parameter.

> logout :: App CGIResult
> logout = do
>   redirect' <- getInputDefault "/" "redirect"
>   deleteAuthCookie
>   redirect redirect'

\subsection{New Members}

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
>   username  <- getRequiredInput "username"
>   passwd    <- getRequiredInput "password"
>   email     <- getInput "email"
>   memberNo  <- addMember' username passwd email
>   case memberNo of
>     Nothing  -> error "Failed to add member."
>     Just no  -> do
>       ip <- remoteAddr
>       setAuthCookie no username ip
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
