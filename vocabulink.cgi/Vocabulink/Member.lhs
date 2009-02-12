> module Vocabulink.Member (  withMemberNumber, withRequiredMemberNumber,
>                             newMemberPage, addMember, loginPage,
>                             login, logout) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member.Auth
> import Vocabulink.Utils

> import Network.URI (escapeURIString, isUnescapedInURI)

\subsection{Helpers}

Here are a couple functions that most pages on the site will use for
determining the identity for a member.

|withMemberNumber| accepts a default value (for if the client isn't logged in)
and a function to carry out with the member's number otherwise.

> withMemberNumber :: a -> (Integer -> App a) -> App a
> withMemberNumber d f = asks memberNumber >>= maybe (return d) f

|withRequiredMemberNumber| is like |withMemberNumber|, but it provides a
``logged out default'' of redirecting the client to the login page.

> withRequiredMemberNumber :: (Integer -> App CGIResult) -> App CGIResult
> withRequiredMemberNumber f =  asks memberNumber >>=
>                               maybe (loginRedirectPage >>= redirect) f

When we direct a user to the login page, we want to make sure that they can
find their way back to where they were. To do so, we get the current URI and
append it to the login page in the query string. The login page will know what
to do with it.

> loginRedirectPage :: App String
> loginRedirectPage = do
>   request <- fromMaybe "/" `liftM` getVar "REQUEST_URI"
>   return $ "/member/login?redirect=" ++ escapeURIString isUnescapedInURI request

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

> getMemberNumber :: String -> App (Maybe Integer)
> getMemberNumber username = do
>   c <- asks db
>   n <- liftIO $ queryValue c "SELECT member_no FROM member \
>                              \WHERE username = ?" [toSql username]
>                   `catchSqlE` "Failed to retrieve member number from username."
>   return $ maybe Nothing fromSql n

Login attempts to match the username and password supplied against the
information in the database.

> validPassword :: String -> String -> App (Bool)
> validPassword username passwd = do
>   c <- asks db
>   n <- liftIO $ queryValue c "SELECT password_hash = crypt(?, password_hash) \
>                              \FROM member WHERE username = ?"
>                              [toSql passwd, toSql username]
>                   `catchSqlE` "Internal authentication failure (this is not your fault)."
>   return $ maybe False fromSql n

> login :: App CGIResult
> login = do
>   join <- getInputDefault "" "join"
>   if join /= ""
>      then do -- The user clicked the Join button in the login box.
>        -- Carry over the username if they entered it.
>        username <- getInputDefault "" "username"
>        if username /= ""
>           then redirect $ "/member/join?username=" ++ encodeString username
>           else redirect "/member/join"
>      else do
>        username  <- getRequiredInput "username"
>        memberNo  <- getMemberNumber username
>        passwd    <- getRequiredInput "password"
>        ref       <- refererOrVocabulink
>        redirect' <- getInputDefault ref "redirect"
>        ip <- remoteAddr
>        valid <- validPassword username passwd
>        not valid ? error "Login failed." $ do
>          case memberNo of
>            Nothing  -> redirect redirect'
>            Just n   -> do
>              setAuthCookie n username ip
>              redirect redirect'

> logout :: App CGIResult
> logout = do
>   redirect' <- getInputDefault "/" "redirect"
>   deleteAuthCookie
>   redirect redirect'

> loginPage :: App CGIResult
> loginPage = do
>   referer'  <- refererOrVocabulink
>   redirect' <- getInputDefault referer' "redirect"
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