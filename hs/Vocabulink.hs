-- Introduction

-- This is Vocabulink, the SCGI program that handles all web requests for
-- http://www.vocabulink.com/. The site helps people learn languages through
-- fiction. It provides a mnemonics database and spaced repetion (review)
-- tools.

-- Architecture

-- Requests arrive via a webserver. (I'm currently using lighttpd, but it
-- should work with any server that supports SCGI.) They are passed to the
-- vocabulink.cgi process (this program) on TCP port 10033 of the local
-- loopback interface.

-- Upon receiving a request (connection), we immediately fork a new thread. In
-- this thread, we establish a connection to a PostgreSQL server (for each
-- request). We then examine the request for an authentication cookie. If it
-- exists and is valid, we consider the request to have originated from an
-- authenticated member. We pack both the database handle and the authenticated
-- member information into our App monad and then pass control to a function
-- based on the request method and URI.

module Main where

import Vocabulink.API
import Vocabulink.Article
import Vocabulink.CGI
import Vocabulink.Comment
import Vocabulink.Env
import Vocabulink.Html hiding (method, dl)
import Vocabulink.Link
import Vocabulink.Member
import Vocabulink.Member.Html
import Vocabulink.Member.Registration
import Vocabulink.Page
import Vocabulink.Reader
import Vocabulink.Review
import Vocabulink.Utils

import Prelude hiding (div, span, id, words)

import Control.Exception (bracket, SomeException, Handler(..), catches, try)
import Data.Bits ((.|.))
import Data.String.Conv (toS)
import Database.PostgreSQL.Typed (PGDatabase(pgDBAddr, pgDBName, pgDBUser), pgConnect, pgDisconnect, defaultPGDatabase)
import Network.HTTP.Types (status500, status302)
import Network.Socket (socket, bind, listen, close, SockAddr(SockAddrUnix), Family(AF_UNIX), SocketType(Stream))
import Network.Wai (requestHeaders, requestHeaderReferer, responseLBS)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings, setServerName)
import Servant (Proxy(Proxy), serve, (:<|>) ((:<|>)), NoContent(NoContent))
import System.Directory (removeFile)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode, otherReadMode, otherWriteMode)
import Web.Cookie (parseCookies)

main = do
  args <- getArgs
  case args of
    [staticPath, sendmail, tokenKey] -> do
      bracket
        (socket AF_UNIX Stream 0)
        (\ sock -> do
            close sock
            try' (removeFile "vocabulink.sock"))
        (\ sock -> do
            bind sock (SockAddrUnix "vocabulink.sock")
            setFileMode "vocabulink.sock" (ownerReadMode .|. ownerWriteMode .|. groupReadMode .|. groupWriteMode .|. otherReadMode .|. otherWriteMode)
            listen sock 1024
            runSettingsSocket (setServerName "" defaultSettings) sock (handleRequest staticPath sendmail tokenKey))
    _ -> do
      progName <- getProgName
      hPutStrLn stderr ("Usage: " ++ progName ++ " static-dir sendmail token-key")
 where try' :: IO a -> IO (Either SomeException a)
       try' = try

handleRequest staticPath sendmail tokenKey req sendResponse = do
  bracket
    (pgConnect defaultPGDatabase {pgDBAddr = Right (SockAddrUnix "/var/run/postgresql/.s.PGSQL.5432"), pgDBName = "vocabulink", pgDBUser = "vocabulink"})
    pgDisconnect
    (\ db -> do
        member <- loggedIn db
        let ?db = db
            ?static = staticPath
            ?sendmail = sendmail
            ?tokenKey = tokenKey
            ?member = member
            ?referrer = (toS `fmap` requestHeaderReferer req) :: Maybe String
        vocabulinkApp req sendResponse `catches` [Handler (vocabulinkHandler sendResponse), Handler defaultHandler])
 where loggedIn db = do
         let auth = (lookup "auth" . parseCookies) =<< (lookup "Cookie" (requestHeaders req))
         token' <- maybe (return Nothing) (validAuth tokenKey . toS) auth
         case token' of
           Nothing -> return Nothing
           Just token -> do
             row <- $(queryTuple
                      "SELECT username, email FROM member \
                      \WHERE member_no = {authMemberNumber token}") db
             return ((\(username, email) -> Member { memberNumber = authMemberNumber token
                                                   , memberName   = username
                                                   , memberEmail  = email
                                                   }) `fmap` row)
       defaultHandler :: SomeException -> _
       defaultHandler _ = sendResponse $ responseLBS status500 [] "An error occurred."

vocabulinkHandler sendResponse (VError msg) =
  let (headers, body) = redirectWithMsg' referrerOrVocabulink [] MsgError msg
  in sendResponse (responseLBS status302 headers body)

vocabulinkApp = serve (Proxy :: Proxy VocabulinkAPI) app
 where app = liftIO frontPage
        :<|> redirect "https://github.com/jekor/vocabulink/tarball/master"

-- Some permanent URIs are essentially static files. To display them, we make
-- use of the article system (formatting, metadata, etc). You could call these
-- elevated articles. We use articles because the system for managing them
-- exists already (revision control, etc)

-- Each @.html@ file is actually an HTML fragment. These happen to be generated
-- from Muse Mode files by Emacs, but we don't really care where they come
-- from.

-- Other articles are dynamic and can be created without recompilation. We just
-- have to rescan the filesystem for them. They also live in the @/article@
-- namespace (specifically at @/article/title@).

        :<|> ((maybeNotFound . liftIO . articlePage)

-- We have 1 page for getting a listing of all published articles.

         :<|> liftIO articlesPage
         :<|> maybeNotFound (liftIO (articlePage "help"))
         :<|> maybeNotFound (liftIO (articlePage "privacy"))
         :<|> maybeNotFound (liftIO (articlePage "terms-of-use"))
         :<|> maybeNotFound (liftIO (articlePage "source"))
         :<|> maybeNotFound (liftIO (articlePage "api")))

-- Link Pages

-- Vocabulink revolves around links---the associations between words or ideas. As
-- with articles, we have different functions for retrieving a single link or a
-- listing of links. However, the dispatching is complicated by the fact that
-- members can operate upon links (we need to handle the @POST@ method).

-- For clarity, this dispatches:

-- GET    /link/10               → link page
-- POST   /link/10/stories       → add a linkword story

        :<|> ((\ n -> maybeNotFound (liftIO (getStory n))
                 :<|> (\ story -> liftIO (editStory n story) >> return NoContent)
                 :<|> (\ data' -> do
                           let story = bodyVarRequired "story" data'
                           liftIO $ editStory n story
                           -- TODO: This redirect masks the result of editStory
                           redirect (toS referrerOrVocabulink)))

         -- TODO: Find a way to negotiate content types without always rendering the link HTML
         --       (which is not needed for the JSON case).
         :<|> (\ n -> maybeNotFound (liftIO ((\ l -> case l of Nothing -> return Nothing; Just l' -> (Just . LinkOutput l') `fmap` linkPage l') =<< linkDetails n))
                 :<|> maybeNotFound (liftIO ((\ l -> case l of Nothing -> return Nothing; Just l' -> Just `fmap` compactLinkPage l') =<< linkDetails n))
                -- TODO: 404 when link number doesn't exist
                 :<|> liftIO (linkStories n)
                 :<|> (\ data' -> do
                           let story = bodyVarRequired "story" data'
                           liftIO $ addStory n story
                           -- TODO: This redirect masks the result of editStory
                           redirect (toS ("/link/" ++ show n)))))

-- Retrieving a listing of links is easier.

        :<|> (\ (Language ol) (Language dl) -> do
                  links <- liftIO (languagePairLinks ol dl)
                  liftIO (linksPage ("Links from " ++ ol ++ " to " ++ dl) links))

-- Readers

        :<|> (\ lang name' page -> maybeNotFound (liftIO (readerPage lang name' page)))

-- Searching

        :<|> (\ q -> do
                links <- if q == ""
                           then return []
                           else liftIO (linksContaining q)
                liftIO (linksPage ("Search Results for \"" <> q <> "\"") links))

-- Review

-- Members review their links by interacting with the site in a vaguely
-- REST-ish way. The intent behind this is that in the future they will be able
-- to review their links through different means such as a desktop program or a
-- phone application.

-- GET  /review       → review page (app)
-- PUT  /review/n     → add a link for review
-- GET  /review/next  → retrieve the next links for review
-- POST /review/n     → mark link as reviewed

-- (where n is the link number)

-- Reviewing links is one of the only things that logged-in-but-unverified
-- members are allowed to do.

        :<|> ((\ (Language learn) (Language known) -> liftIO (reviewPage learn known))
         :<|> (\ n -> withLoggedInMember (\ m -> liftIO (newReview m n) >> return NoContent))
         :<|> withLoggedInMember (liftIO . reviewStats)
         :<|> (\ statType start end tzOffset ->
                 withLoggedInMember (\ m -> liftIO ((case statType of StatTypeDaily -> dailyReviewStats; StatTypeDetailed -> detailedReviewStats) m start end tzOffset)))
         :<|> (\ n data' ->
                 do let grade = read (bodyVarRequired "grade" data')
                        recallTime = read (bodyVarRequired "time" data')
                        reviewedAt' = maybe Nothing readMaybe (bodyVar "when" data')
                    reviewedAt <- case reviewedAt' of
                                    Nothing -> liftIO epochTime
                                    Just ra -> return ra
                    -- TODO: Sanity-check this time. It should at least not be in the future.
                    withLoggedInMember (\ m -> liftIO $ scheduleNextReview m n grade recallTime reviewedAt >> return NoContent)))

-- Dashboard

        :<|> withLoggedInMember (const (liftIO dashboardPage))

-- Membership

-- Becoming a member is simply a matter of filling out a form.

-- Note that in some places where I would use a PUT I've had to append a verb
-- to the URL and use a POST instead because these requests are often made to
-- HTTPS pages from HTTP pages and can't be done in JavaScript without a lot of
-- not-well-supported cross-domain policy hacking.

        :<|> ((\ data' ->
                 let username = bodyVarRequired "username" data'
                     email = bodyVarRequired "email" data'
                     password = bodyVarRequired "password" data'
                     learned = bodyVar "learned" data'
                 in liftIO (signup username email password learned) >>= \ c -> cookieBounce [c] MsgSuccess "Welcome! Please check your email to confirm your account.")

-- But to use most of the site, we require email confirmation.

         :<|> (\ hash -> liftIO (confirmEmail hash) >>= \case
                           Left pg -> return pg
                           Right c -> cookieBounce [c] MsgSuccess "Congratulations! You've confirmed your account.")
         :<|> (case ?member of
                 Nothing -> do
                   liftIO (simplePage "Please Login to Resend Your Confirmation Email" [ReadyJS "V.loginPopup();"] mempty)
                 Just m  -> do
                   liftIO $ resendConfirmEmail m
                   bounce MsgSuccess "Your confirmation email has been sent.")

-- Logging in is a similar process.

         :<|> (\ data' -> do
                 let userid = bodyVarRequired "userid" data'
                     password = bodyVarRequired "password" data'
                 liftIO (login userid password) >>= \ c -> cookieBounce [c] MsgSuccess "Login successful.")

-- Logging out can be done without a form.

         :<|> cookieRedirect [emptyAuthCookie {setCookieMaxAge = Just (secondsToDiffTime 0)}] "https://www.vocabulink.com/"
         :<|> (\ data' -> do
                 let password = bodyVarRequired "password" data'
                 liftIO (deleteAccount password)
                 cookieBounce [emptyAuthCookie {setCookieMaxAge = Just $ secondsToDiffTime 0}] MsgSuccess "Your account was successfully deleted.")
         :<|> ((\ data' -> do
                  let email = bodyVarRequired "email" data'
                  liftIO (sendPasswordReset email)
                  bounce MsgSuccess "Password reset instructions have been sent to your email.")
          :<|> (liftIO . passwordResetPage)
          :<|> (\ hash data' -> do
                  let password = bodyVarRequired "password" data'
                  liftIO (passwordReset hash password) >>= \ c -> cookieBounce [c] MsgSuccess "Password reset successfully.")
          :<|> (\ data' -> do
                  let oldPassword = bodyVarRequired "old-password" data'
                      newPassword = bodyVarRequired "new-password" data'
                  liftIO (changePassword oldPassword newPassword)
                  bounce MsgSuccess "Password changed successfully."))

         :<|> (\ data' -> do
                let email = bodyVarRequired "email" data'
                    password = bodyVarRequired "password" data'
                liftIO (changeEmail email password)
                bounce MsgSuccess "Email address changed successfully. Please check your email to confirm the change."))

-- Member Pages

         :<|> (\ username ->
                 maybeNotFound (liftIO (maybe (return Nothing) (\ m -> memberPage m >>= \ m' -> return (Just m')) =<< memberByName username))
            :<|> liftIO (usernameAvailable username))

         :<|> (liftIO . emailAvailable)

-- ``reply'' is used here as a noun.

         :<|> (\ n data' -> do
                 let body = bodyVarRequired "body" data'
                 liftIO $ withVerifiedMember (\m -> storeComment (memberNumber m) body (Just n))
                 redirect (toS referrerOrVocabulink))

-- Everything Else

-- For Google Webmaster Tools, we need to respond to a certain URI that acts as
-- a kind of ``yes, we really do run this site''.

         :<|> return (unlines [ "User-agent: *"
                              , "Disallow:" ])

         :<|> return "google-site-verification: google1e7c25c4bdfc5be7.html"

-- Finally, we get to an actual page of the site: the front page.

frontPage = do
  mainButton <- case ?member of
                  Nothing -> return buyButton
                  Just m -> do
                    row <- $(queryTuple "SELECT page_no \
                                        \FROM member_reader \
                                        \INNER JOIN reader USING (reader_no) \
                                        \WHERE short_name = 'para-bailar' AND lang = 'es' AND member_no = {memberNumber m}") ?db
                    return $ case row of
                      Nothing -> buyButton
                      Just pageNo -> div ! class_ "button_buy" $ do
                                       a ! href (toValue $ "/reader/es/para-bailar/" ++ show (pageNo :: Int32)) ! class_ "gradient" ! title "Continue Reading" $ do
                                         span ! class_ "button_price" $ "Continue"
                                         span ! class_ "button_text" $ "Reading"
  stdPage ("Learn Spanish Through Fiction - Vocabulink") [] (do
    meta ! name "viewport" ! content "width=device-width, initial-scale=1.0, user-scalable=no"
    preEscapedToMarkup ("<link rel=\"stylesheet\" href=\"//s.vocabulink.com/css/off-the-shelf.css\" media=\"screen, projection\"> \
                        \<link href=\"//netdna.bootstrapcdn.com/font-awesome/3.2.0/css/font-awesome.css\" rel=\"stylesheet\">"::String)) $ do
      section ! id "banner" $ do
        div ! class_ "row" $ do
          div ! id "shelf" ! class_ "one_half" $ do
            img ! alt "book cover" ! width "331" ! height "497" ! src "//s.vocabulink.com/img/reader/es/para-bailar.jpg"
          div ! class_ "one_half last" $ do
            hgroup $ do
              h1 $ "Learn Spanish Through Fiction"
              h2 ! class_ "subheader" $ "Read real everyday Spanish. Learn new words in context. Designed for beginners, written for adults."
            p $ "Learning doesn't have to be boring. Stories have entertained and educated for centuries. This book is a collection of stories that will teach you some of the basics of Spanish."
            p $ "This isn't like any language reader you've seen before. We've designed it by mixing 4 modern accelerated language learning techniques."
            mainButton
      article $ do
        div ! id "main_content" $ do
          section ! id "features" $ do
            div ! class_ "row" $ do
              h2 ! class_ "section_title" $ do
                span $ "Accelerated Learning Features"
              ul $ do
                li ! class_ "one_half" $ do
                  i ! class_ "icon-road icon-4x" $ mempty
                  h4 "Gradual Progression"
                  p "Have you ever tried reading Spanish but just go frustrated and gave up? Our stories start with very simple sentences. Then, gradually over time we introduce new words and new grammar."
                li ! class_ "one_half last" $ do
                  i ! class_ "icon-magic icon-4x" $ mempty
                  h4 "Linkword Mnemonics"
                  p "Instead of staring at a list of vocabulary or writing words out 100 times, wouldn't it be nice to have a trick to remember each word by? We thought so too. That's why we've included mnemonics for each word we introduce you to."
                li ! class_ "one_half" $ do
                  i ! class_ "icon-sort-by-attributes-alt icon-4x" $ mempty
                  h4 "Words Selected by Frequency"
                  p "Why waste time learning words you're rarely, if ever, going to use? We analyzed how frequently words occur in written and spoken Spanish and use only words among the most common 3,000."
                li ! class_ "one_half last" $ do
                  i ! class_ "icon-bar-chart icon-4x" $ mempty
                  h4 "Spaced Repetition"
                  p "Flashcards are great, but we have better technology today. We use a special algorithm to schedule optimal times for you to review each word that you learn. This algorithm adapts to your memory and the difficulty of each word."
 where buyButton = div ! class_ "button_buy" $ do
                     a ! href "/reader/es/para-bailar/1" ! class_ "gradient" ! title "Preview for Free" $ do
                       span ! class_ "button_price" $ "Start"
                       span ! class_ "button_text" $ "Reading"
