-- Copyright 2008, 2009, 2010, 2011, 2012 Chris Forno

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

import Vocabulink.App
import Vocabulink.Article
import Vocabulink.Article.Html
import Vocabulink.CGI
import Vocabulink.Comment
import Vocabulink.Config
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Link.Frequency
import Vocabulink.Link.Html
import Vocabulink.Link.Story
import Vocabulink.Member
import Vocabulink.Member.Page
import Vocabulink.Member.Registration
import Vocabulink.Page
import Vocabulink.Review
import Vocabulink.Utils

import Prelude hiding (div, span, id, words)

import Control.Concurrent (forkIO)
import Data.ConfigFile (get)
import Data.List (find)
import Database.TemplatePG (pgConnect)
import Network (PortID(..))
import Network.SCGI (runSCGIConcurrent')
import Network.URI (URI(..), unEscapeString)

-- Entry and Dispatch

-- When the program starts, it immediately begin listening for connections.
-- |runSCGIConcurrent'| spawns up to some number of threads. This matches the
-- number that lighttpd, running in front of vocabulink.cgi, is configured for.

-- Before forking, we read a configuration file. We pass this to runApp so that
-- all threads have access to global configuration information.

-- The first thing we do after forking is establish a database connection. The
-- database connection might be used immediately in order to log errors. It'll
-- eventually be passed to the App monad where it'll be packed into a reader
-- environment.

main :: IO ()
main = do
  cp <- liftM forceEither getConfig
  let threads = forceEither $ get cp "DEFAULT" "threads"
      pw      = forceEither $ get cp "DEFAULT" "dbpassword"
  sd <- staticDeps cp
  runSCGIConcurrent' forkIO threads (PortNumber 10033) (do
    h <- liftIO $ pgConnect "localhost" (PortNumber 5432) "vocabulink" "vocabulink" pw
    handleErrors h (runApp h cp sd handleRequest))

-- |handleRequest| ``digests'' the requested URI before passing it to the
--  dispatcher.

handleRequest :: App CGIResult
handleRequest = do
  uri  <- requestURI
  meth <- requestMethod
  dispatch' meth (pathList uri)

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

pathList :: URI -> [String]
pathList = splitOn "/" . decodeString . unEscapeString . uriPath

-- Before we actually dispatch the request, we use the opportunity to clean up
-- the URI and redirect the client if necessary. This handles cases like
-- trailing slashes. We want only one URI to point to a resource.

dispatch' :: String -> [String] -> App CGIResult
dispatch' meth path =
  case path of
    ["",""] -> frontPage -- "/"
    ("":xs) -> case find (== "") xs of
                 Nothing -> dispatch meth xs
                 Just _  -> redirect $ '/' : intercalate "/" (filter (/= "") xs)
    _       -> outputNotFound

-- Here is where we dispatch each request to a function. We can match the
-- request on method and path components. This means that we can dispatch a
-- @GET@ request to one function and a @POST@ request to another.

dispatch :: String -> [String] -> App CGIResult

-- Articles

-- Some permanent URIs are essentially static files. To display them, we make
-- use of the article system (formatting, metadata, etc). You could call these
-- elevated articles. We use articles because the system for managing them
-- exists already (revision control, etc)

-- Each @.html@ file is actually an HTML fragment. These happen to be generated
-- from Muse Mode files by Emacs, but we don't really care where they come
-- from.

dispatch "GET" ["help"]         = articlePage "help"
dispatch "GET" ["privacy"]      = articlePage "privacy"
dispatch "GET" ["terms-of-use"] = articlePage "terms-of-use"
dispatch "GET" ["source"]       = articlePage "source"
dispatch "GET" ["api"]          = articlePage "api"
dispatch "GET" ["download"]     = redirect "https://github.com/jekor/vocabulink/tarball/master"

dispatch "POST" ["contact"]     = contactUs

-- Other articles are dynamic and can be created without recompilation. We just
-- have to rescan the filesystem for them. They also live in the @/article@
-- namespace (specifically at @/article/title@).

dispatch "GET" ["article",x] = articlePage x

-- We have 1 page for getting a listing of all published articles.

dispatch "GET" ["articles"] = articlesPage

-- And this is a method used by the web-based administrative interface to
-- reload the articles from the filesystem. (Articles are transmitted to the
-- server via rsync using the filesystem, not through the web.)

dispatch "POST" ["articles"] = refreshArticles

-- Link Pages

-- Vocabulink revolves around links---the associations between words or ideas. As
-- with articles, we have different functions for retrieving a single link or a
-- listing of links. However, the dispatching is complicated by the fact that
-- members can operate upon links (we need to handle the @POST@ method).

-- For clarity, this dispatches:

-- GET    /link/10               → link page
-- POST   /link/10/stories       → add a linkword story

dispatch meth ["link","story",x] =
  case maybeRead x of
    Nothing -> outputNotFound
    Just n  -> case meth of
                 "GET" -> maybe outputNotFound outputText =<< getStory n
                 "PUT" -> getBody >>= editStory n
                 -- temporarily allow POST until AJAX forms are better
                 "POST" -> do
                   getRequiredInput "story" >>= editStory n
                   referrerOrVocabulink >>= redirect
                 _     -> outputNotFound

dispatch meth ("link":x:part) =
  case maybeRead x of
    Nothing -> outputNotFound
    Just n  -> case (meth, part) of
                 ("GET"   , [])                -> linkPage n
                 ("GET"   , ["stories"])       -> do
                   -- TODO: Support HTML/JSON output based on content-type negotiation.
                   stories <- linkWordStories n
                   outputHtml $ mconcat $ map (\(a', b', c', d') -> renderStory a' b' c' d') stories
                 ("POST"  , ["stories"])       -> do
                   story <- getRequiredInput "story"
                   addStory n story
                   redirect $ "/link/" ++ show n
                 (_       , _)                 -> outputNotFound

-- Retrieving a listing of links is easier.

dispatch "GET" ["links"] = do
  ol <- getInput "ol"
  dl <- getInput "dl"
  case (ol, dl) of
    (Just ol', Just dl')  -> do
      ol'' <- langNameFromAbbr ol'
      dl'' <- langNameFromAbbr dl'
      case (ol'', dl'') of
        (Just ol''', Just dl''') -> do db <- asks appDB
                                       mn <- memberNumber <$$> asks appMember
                                       links <- liftIO $ languagePairLinks mn ol' dl' db
                                       linksPage ("Links from " ++ ol''' ++ " to " ++ dl''') links
        _                        -> outputNotFound
    _                        -> languagePairsPage

-- Searching

dispatch "GET" ["search"] = do
  q <- getRequiredInput "q"
  db <- asks appDB
  links <- if q == ""
              then return []
              else liftIO $ linksContaining q db
  linksPage ("Search Results for \"" ++ q ++ "\"") links

-- Languages

-- Browsing through every link on the site doesn't work with a significant
-- number of links. A languages page shows what's available and contains
-- hyperlinks to language-specific browsing.

dispatch "GET" ["languages"] = permRedirect "/links"

-- Frequency Lists

dispatch "GET"  ["list","frequency",lang] = frequencyLists lang
dispatch "POST" ["list","frequency",lang] = addFrequencyList lang

-- Learning

dispatch "GET" ["learn"] = learnPage
dispatch "GET" ["learn", "upcoming"] = upcomingLinks

-- Link Review

-- Members review their links by interacting with the site in a vaguely
-- REST-ish way. The intent behind this is that in the future they will be able
-- to review their links through different means such as a desktop program or a
-- phone application.

-- PUT  /review/n     → add a link for review
-- GET  /review/next  → retrieve the next links for review
-- GET  /review/upcoming?until=timestamp → retrieve upcoming links for review
-- POST /review/n     → mark link as reviewed

-- (where n is the link number)

-- Reviewing links is one of the only things that logged-in-but-unverified
-- members are allowed to do.

dispatch meth ("review":rpath) = do
  member' <- asks appMember
  case member' of
    Nothing     -> outputNotFound
    Just member ->
      case (meth,rpath) of
        ("GET",  ["stats"])    -> reviewStats member
        ("GET",  ["stats",x])  -> do
          start <- readRequiredInput "start"
          end   <- readRequiredInput "end"
          tzOffset <- getRequiredInput "tzoffset"
          case x of
            "daily"    -> dailyReviewStats member start end tzOffset
            "detailed" -> detailedReviewStats member start end tzOffset
            _          -> outputNotFound
        ("PUT",  [x]) ->
          case maybeRead x of
            Nothing -> error "Link number must be an integer"
            Just n  -> newReview member n >> outputNothing
        ("POST", [x]) ->
          case maybeRead x of
            Nothing -> error "Link number must be an integer"
            Just n  -> do
              grade <- readRequiredInput "grade"
              recallTime <- readRequiredInput "time"
              reviewedAt' <- readInput "when"
              reviewedAt <- case reviewedAt' of
                              Nothing -> liftIO getCurrentTime
                              Just ra -> return $ utcEpoch ra
              -- TODO: Sanity-check this time. It should at least not be in the future.
              scheduleNextReview member n grade recallTime reviewedAt
              outputNothing

        (_       ,_)  -> outputNotFound

-- Dashboard

dispatch "GET" ["dashboard"] = dashboardPage

-- Membership

-- Becoming a member is simply a matter of filling out a form.

dispatch "POST" ["member","signup"] = signup

-- But to use most of the site, we require email confirmation.

dispatch "GET" ["member","confirmation",x] = confirmEmail x
dispatch "POST" ["member","confirmation"]  = resendConfirmEmail

-- Logging in is a similar process.

dispatch "POST" ["member","login"] = login

-- Logging out can be done without a form.

dispatch "POST" ["member","logout"] = logout

dispatch "POST" ["member","password","reset"] = sendPasswordReset
dispatch "GET"  ["member","password","reset",x] = passwordResetPage x
dispatch "POST" ["member","password","reset",x] = passwordReset x

-- Member Pages

dispatch "GET" ["user", username] = memberPage username
dispatch "GET" ["user", username, "available"] = outputJSON =<< usernameAvailable username
dispatch "GET" ["email", email, "available"] = outputJSON =<< emailAvailable email

-- ``reply'' is used here as a noun.

dispatch meth ("comment":x:meth') =
  case maybeRead x of
    Nothing -> outputNotFound
    Just n  -> case (meth, meth') of
                 ("POST", ["reply"]) -> replyToComment n
                 (_     , _)         -> outputNotFound

-- Everything Else

-- For Google Webmaster Tools, we need to respond to a certain URI that acts as
-- a kind of ``yes, we really do run this site''.

dispatch "GET" ["google1e7c25c4bdfc5be7.html"] = outputText "google-site-verification: google1e7c25c4bdfc5be7.html"

dispatch "GET" ["robots.txt"] = outputText $ unlines [ "User-agent: *"
                                                     , "Disallow:"
                                                     ]

-- It would be nice to automatically respond with ``Method Not Allowed'' on
-- URIs that exist but don't make sense for the requested method (presumably
-- @POST@). However, we need to take a simpler approach because of how the
-- dispatch method was designed (pattern matching is limited). We output a
-- qualified 404 error.

dispatch _ _ = outputNotFound

-- Finally, we get to an actual page of the site: the front page.

frontPage :: App CGIResult
frontPage = do
  m <- asks appMember
  let limit = 40
  words <- case m of
    -- Logged in? Use the words the person has learned.
    Just m'  -> $(queryTuples'
      "SELECT learn, link_no FROM link \
      \WHERE NOT deleted AND link_no IN \
       \(SELECT DISTINCT link_no FROM link_to_review \
        \WHERE member_no = {memberNumber m'}) \
        \ORDER BY random() LIMIT {limit}")
    -- Not logged in? Use words with stories.
    Nothing -> $(queryTuples'
      "SELECT learn, link_no FROM link \
      \WHERE NOT deleted AND link_no IN \
       \(SELECT DISTINCT link_no FROM linkword_story) \
        \ORDER BY random() LIMIT {limit}")
  cloud <- wordCloud words 261 248 12 32 6
  nEsLinks <- fromJust . fromJust <$> $(queryTuple' "SELECT COUNT(*) FROM link WHERE learn_lang = 'es' AND known_lang = 'en' AND NOT deleted")
  nReviews <- fromJust . fromJust <$> $(queryTuple' "SELECT COUNT(*) FROM link_review")
  nLinkwords <- fromJust . fromJust <$> $(queryTuple' "SELECT COUNT(*) FROM link_linkword ll INNER JOIN link l ON (l.link_no = ll.link_no AND NOT deleted)")
  nStories <- fromJust . fromJust <$> $(queryTuple' "SELECT COUNT(*) FROM linkword_story INNER JOIN link USING (link_no) WHERE NOT deleted")
  stdPage "Learn Vocabulary Fast with Linkword Mnemonics" [CSS "front"] mempty $
    mconcat [
      div ! class_ "top" $ do
        div ! id "word-cloud" $ do
          cloud
        div ! id "intro" $ do
          h1 "Learn Vocabulary—Fast"
          p $ do
            toHtml $ "Learn foreign words with " ++ prettyPrint (nLinkwords::Integer) ++ " "
            a ! href "article/how-do-linkword-mnemonics-work" $ "linkword mnemonics"
            toHtml $ " and " ++ prettyPrint (nStories::Integer) ++ " accompanying stories."
          p $ do
            "Retain the words through "
            a ! href "article/how-does-spaced-repetition-work" $ "spaced repetition"
            toHtml $ " (" ++ prettyPrint (nReviews::Integer) ++ " reviews to-date)."
          p $ do
            toHtml $ prettyPrint (nEsLinks::Integer) ++ " of the "
            a ! href "article/why-study-words-in-order-of-frequency" $ "most common"
            " Spanish words await you. More mnemonics and stories are being added daily. The service is free."
          p ! id "try-now" $ do
            a ! href "/learn?learn=es&known=en" ! class_ "faint-gradient-button green" $ do
              "Get Started"
              br
              "with Spanish" ]

