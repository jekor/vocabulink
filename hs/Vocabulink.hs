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

import Control.Concurrent (forkIO)
import Control.Exception (finally, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (find)
import Database.TemplatePG (pgConnect)
import Network (PortID(..), accept)
import Network.Socket (listen, Socket(..), getAddrInfo, socket, Family(..), SocketType(..), defaultProtocol, bindSocket, addrAddress, SocketOption(..), setSocketOption)
import qualified Network.SCGI as SCGI
import Network.URI (unEscapeString)
import System.Environment (getArgs, getProgName)
import System.IO (hClose, hPutStrLn, stderr)
import Web.Cookie (parseCookies)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [staticPath, sendmail, tokenKey] -> do
      s <- listenLocal "10033"
      forever $ do
        (handle, _, _) <- accept s
        _ <- forkIO $ finally (SCGI.runRequest handle (catch (handleRequest staticPath sendmail tokenKey) handleError))
                          (hClose handle)
        return ()
    _ -> do
      progName <- getProgName
      hPutStrLn stderr ("Usage: " ++ progName ++ " static-dir sendmail token-key")
 where listenLocal :: String -> IO Socket
       listenLocal port = do
         addrs <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
         s <- socket AF_INET Stream defaultProtocol
         setSocketOption s ReuseAddr 1
         bindSocket s $ addrAddress $ head addrs
         listen s 5
         return s
       handleError :: SomeException -> SCGI Response
       handleError e = do
         path <- SCGI.path
         case path of
           -- We can't bounce on the front page or we'll get an infinite loop.
           (Just "/") -> return (SCGI.Response "500 Internal Server Error" (BLU.fromString (show e)))
           _ -> bounce MsgError (show e)

handleRequest :: FilePath -> FilePath -> String -> SCGI Response
handleRequest staticPath sendmail tokenKey = do
  db <- liftIO $ pgConnect "localhost" (PortNumber 5432) "vocabulink" "vocabulink" ""
  member <- loggedIn db
  method' <- SCGI.method
  path' <- SCGI.path
  case (method', path') of
    (Just method, Just path) -> let ?db = db
                                    ?static = staticPath
                                    ?sendmail = sendmail
                                    ?tokenKey = tokenKey
                                    ?member = member in dispatch' (BU.toString method) (pathList $ BU.toString path)
    _ -> error "Missing request method or path."
 where loggedIn db = do
         auth <- (lookup "auth" . parseCookies =<<) `liftM` SCGI.header "HTTP_COOKIE"
         token' <- liftIO $ maybe (return Nothing) (validAuth tokenKey . BU.toString) auth
         case token' of
           Nothing -> return Nothing
           Just token -> do
             -- We want to renew the expiry of the auth token if the user
             -- remains active. But we don't want to be sending out a new
             -- cookie with every request. Instead, we check to see that one
             -- day has passed since the auth cookie was set. If so, we refresh
             -- it.
             now <- liftIO epochTime
             when (authExpiry token - now < authShelfLife - (24 * 60 * 60)) $ do
               setCookie =<< liftIO (authCookie (authMemberNumber token) tokenKey)
             row <- liftIO $ $(queryTuple
                      "SELECT username, email FROM member \
                      \WHERE member_no = {authMemberNumber token}") db
             return $ liftM (\(username, email) -> Member { memberNumber = authMemberNumber token
                                                          , memberName   = username
                                                          , memberEmail  = email
                                                          }) row

-- We extract the path part of the URI, ``unescape it'' (convert % codes back
-- to characters), decode it (convert UTF-8 characters to Unicode Chars), and
-- finally parse it into directory and filename components. For example,

-- /some/directory/and/a/filename

-- becomes

-- ["some","directory","and","a","filename"]

-- Note that the parser does not have to deal with query strings or fragments
-- because |uriPath| has already stripped them.

-- The one case this doesn't handle correctly is @//something@, because it's
-- handled differently by |Network.CGI|.

pathList :: String -> [String]
pathList = splitOn "/" . unEscapeString

-- Before we actually dispatch the request, we use the opportunity to clean up
-- the URI and redirect the client if necessary. This handles cases like
-- trailing slashes. We want only one URI to point to a resource.

dispatch' :: E (String -> [String] -> SCGI Response)
dispatch' meth path =
  case path of
    ["",""] -> toResponse $ frontPage -- "/"
    ("":xs) -> case find (== "") xs of
                 Nothing -> dispatch meth xs
                 Just _  -> redirect $ '/' : intercalate "/" (filter (/= "") xs)
    _       -> return notFound

-- Here is where we dispatch each request to a function. We can match the
-- request on method and path components. This means that we can dispatch a
-- @GET@ request to one function and a @POST@ request to another.

dispatch :: E (String -> [String] -> SCGI Response)

-- Articles

-- Some permanent URIs are essentially static files. To display them, we make
-- use of the article system (formatting, metadata, etc). You could call these
-- elevated articles. We use articles because the system for managing them
-- exists already (revision control, etc)

-- Each @.html@ file is actually an HTML fragment. These happen to be generated
-- from Muse Mode files by Emacs, but we don't really care where they come
-- from.

dispatch "GET" ["help"]         = bounce MsgSuccess "Just testing, escaping." -- toResponse $ articlePage "help"
dispatch "GET" ["privacy"]      = toResponse $ articlePage "privacy"
dispatch "GET" ["terms-of-use"] = toResponse $ articlePage "terms-of-use"
dispatch "GET" ["source"]       = toResponse $ articlePage "source"
dispatch "GET" ["api"]          = toResponse $ articlePage "api"
dispatch "GET" ["download"]     = redirect "https://github.com/jekor/vocabulink/tarball/master"

-- Other articles are dynamic and can be created without recompilation. We just
-- have to rescan the filesystem for them. They also live in the @/article@
-- namespace (specifically at @/article/title@).

dispatch "GET" ["article",x] = toResponse $ articlePage x

-- We have 1 page for getting a listing of all published articles.

dispatch "GET" ["articles"] = toResponse $ articlesPage

-- Link Pages

-- Vocabulink revolves around links---the associations between words or ideas. As
-- with articles, we have different functions for retrieving a single link or a
-- listing of links. However, the dispatching is complicated by the fact that
-- members can operate upon links (we need to handle the @POST@ method).

-- For clarity, this dispatches:

-- GET    /link/10               → link page
-- POST   /link/10/stories       → add a linkword story

dispatch meth ["link","story",x] =
  case readMaybe x of
    Nothing -> return notFound
    Just n  -> case meth of
                 "GET" -> toResponse $ getStory n
                 "PUT" -> do
                   story <- SCGI.body
                   liftIO $ editStory n (BLU.toString story)
                   return emptyResponse
                 -- temporarily allow POST until AJAX forms are better
                 "POST" -> do
                   story <- bodyVarRequired "story"
                   liftIO $ editStory n story
                   redirect =<< referrerOrVocabulink -- TODO: This redirect masks the result of editStory
                 _ -> return notAllowed

dispatch meth ("link":x:part) =
  case readMaybe x of
    Nothing -> return notFound
    Just n  -> case (meth, part) of
                 ("GET", []) -> do
                   link' <- liftIO $ linkDetails n
                   reps <- SCGI.negotiate ["application/json", "text/html"]
                   case (reps, link') of
                     (("application/json":_), Just link) -> toResponse $ toJSON link
                     (("text/html":_), Just link) -> toResponse $ linkPage link
                     _ -> return notFound
                 ("GET", ["compact"]) -> do
                   link' <- liftIO $ linkDetails n
                   case link' of
                     Just link -> toResponse $ compactLinkPage link
                     Nothing -> return notFound
                 ("GET", ["stories"]) -> do
                   -- TODO: Support HTML/JSON output based on content-type negotiation.
                   stories <- liftIO $ linkStories n
                   toResponse $ mconcat $ map toMarkup stories
                 ("POST", ["stories"]) -> do
                   story <- bodyVarRequired "story"
                   liftIO $ addStory n story
                   redirect $ "/link/" ++ show n -- TODO: This redirect masks the result of addStory
                 _ -> return notAllowed

-- Retrieving a listing of links is easier.

dispatch "GET" ["links"] = do
  ol' <- queryVar "ol"
  dl' <- queryVar "dl"
  case (ol', dl') of
    (Just ol, Just dl)  -> do
      case (lookup ol languages, lookup dl languages) of
        (Just olang, Just dlang) -> do links <- liftIO $ languagePairLinks ol dl
                                       toResponse $ linksPage ("Links from " ++ olang ++ " to " ++ dlang) links
        _                        -> return notFound
    _ -> return notFound

-- Readers

dispatch "GET" ["reader", lang, name', pg] = case readMaybe pg of
                                               Nothing -> return notFound
                                               Just n  -> toResponse $ readerPage lang name' n

-- Searching

dispatch "GET" ["search"] = do
  q <- queryVarRequired "q"
  links <- if q == ""
              then return []
              else liftIO $ linksContaining q
  toResponse $ linksPage ("Search Results for \"" ++ q ++ "\"") links

-- Languages

-- Browsing through every link on the site doesn't work with a significant
-- number of links. A languages page shows what's available and contains
-- hyperlinks to language-specific browsing.

dispatch "GET" ["languages"] = permRedirect "/links"

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

dispatch "GET" ["review"] = do
  learn <- queryVarRequired "learn"
  known <- queryVarRequired "known"
  case (lookup learn languages, lookup known languages) of
    (Just _, Just _) -> toResponse $ reviewPage learn known
    _ -> return notFound

dispatch meth ("review":rpath) = do
  case ?member of
    Nothing -> return notFound
    Just m ->
      case (meth, rpath) of
        ("GET",  ["stats"])    -> toResponse . toJSON =<< liftIO (reviewStats m)
        ("GET",  ["stats",x])  -> do
          start <- read `liftM` queryVarRequired "start"
          end   <- read `liftM` queryVarRequired "end"
          tzOffset <- queryVarRequired "tzoffset"
          case x of
            "daily"    -> toResponse . toJSON =<< liftIO (dailyReviewStats m start end tzOffset)
            "detailed" -> toResponse . toJSON =<< liftIO (detailedReviewStats m start end tzOffset)
            _          -> return notFound
        ("PUT",  [x]) ->
          case readMaybe x of
            Nothing -> error "Link number must be an integer"
            Just n  -> liftIO (newReview m n) >> return emptyResponse
        ("POST", ["sync"]) -> do
          clientSync <- bodyJSON
          case clientSync of
            Just retained -> toResponse . toJSON =<< liftIO (syncLinks m retained)
            _ -> error "Invalid sync object."
        ("POST", [x]) ->
          case readMaybe x of
            Nothing -> error "Link number must be an integer"
            Just n  -> do
              grade <- read `liftM` bodyVarRequired "grade"
              recallTime <- read `liftM` bodyVarRequired "time"
              reviewedAt' <- maybe Nothing readMaybe `liftM` bodyVar "when"
              reviewedAt <- case reviewedAt' of
                              Nothing -> liftIO epochTime
                              Just ra -> return ra
              -- TODO: Sanity-check this time. It should at least not be in the future.
              liftIO $ scheduleNextReview m n grade recallTime reviewedAt
              return emptyResponse
        _ -> return notFound

-- Dashboard

dispatch "GET" ["dashboard"] = withLoggedInMember $ const $ toResponse dashboardPage

-- Membership

-- Becoming a member is simply a matter of filling out a form.

-- Note that in some places where I would use a PUT I've had to append a verb
-- to the URL and use a POST instead because these requests are often made to
-- HTTPS pages from HTTP pages and can't be done in JavaScript without a lot of
-- not-well-supported cross-domain policy hacking.

dispatch "POST" ["member","signup"] = signup

-- But to use most of the site, we require email confirmation.

dispatch "GET" ["member","confirmation",x] = confirmEmail x
dispatch "POST" ["member","confirmation"] =
  case ?member of
    Nothing -> do
      toResponse $ simplePage "Please Login to Resend Your Confirmation Email" [ReadyJS "V.loginPopup();"] mempty
    Just m  -> do
      liftIO $ resendConfirmEmail m
      bounce MsgSuccess "Your confirmation email has been sent."

-- Logging in is a similar process.

dispatch "POST" ["member","login"] = login

-- Logging out can be done without a form.

dispatch "POST" ["member","logout"] = logout

dispatch "POST" ["member","delete"] = deleteAccount

dispatch "POST" ["member","password","reset"] = do
  email <- bodyVarRequired "email"
  liftIO $ sendPasswordReset email
  return emptyResponse
dispatch "GET"  ["member","password","reset",x] = toResponse $ passwordResetPage x
dispatch "POST" ["member","password","reset",x] = passwordReset x
dispatch "POST" ["member","password","change"] = changePassword
dispatch "POST" ["member","email","change"] = changeEmail

-- Member Pages

dispatch "GET" ["user", username] = toResponse $ memberPage <$$> memberByName username
dispatch "GET" ["user", username, "available"] = toResponse . toJSON =<< liftIO (usernameAvailable username)
dispatch "GET" ["email", email, "available"] = toResponse . toJSON =<< liftIO (emailAvailable email)

-- ``reply'' is used here as a noun.

dispatch meth ("comment":x:meth') =
  case readMaybe x of
    Nothing -> return notFound
    Just n  -> case (meth, meth') of
                 -- Every comment posted is actually a reply thanks to the fake root comment.
                 ("POST", ["reply"]) -> do
                   body <- bodyVarRequired "body"
                   withVerifiedMember (\m -> liftIO $ storeComment (memberNumber m) body (Just n))
                   redirect =<< referrerOrVocabulink
                 _ -> return notFound

-- Everything Else

-- For Google Webmaster Tools, we need to respond to a certain URI that acts as
-- a kind of ``yes, we really do run this site''.

dispatch "GET" ["google1e7c25c4bdfc5be7.html"] = toResponse ("google-site-verification: google1e7c25c4bdfc5be7.html"::String)

dispatch "GET" ["robots.txt"] = toResponse $ unlines [ "User-agent: *"
                                                     , "Disallow:"
                                                     ]

-- It would be nice to automatically respond with ``Method Not Allowed'' on
-- URIs that exist but don't make sense for the requested method (presumably
-- @POST@). However, we need to take a simpler approach because of how the
-- dispatch method was designed (pattern matching is limited). We output a
-- qualified 404 error.

dispatch _ _ = return notFound

-- Finally, we get to an actual page of the site: the front page.

frontPage :: E (IO Html)
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
                                       a ! href (toValue $ "/reader/es/para-bailar/" ++ show pageNo) ! class_ "gradient" ! title "Continue Reading" $ do
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
