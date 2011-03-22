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

-- Introduction

-- This is Vocabulink, the SCGI program that handles all web requests for\\*
-- \url{http://www.vocabulink.com/}. The site helps people learn languages
-- through fiction. It provides a mnemonics database, spaced repition (review)
-- tools, and a forum for learners to collaborate.

-- Architecture

-- Requests arrive via a webserver.\footnote{I'm currently using cherokee, but
-- it should work with any server that supports SCGI.} They are passed to the
-- vocabulink.cgi process (this program) on TCP port 10033 of the local
-- loopback interface.

-- Upon receiving a request (connection), we immediately fork a new thread. In
-- this thread, we establish a connection to a PostgreSQL server (for each
-- request). We then examine the request for an authentication cookie. If it
-- exists and is valid, we consider the request to have originated from an
-- authenticated member. We pack both the database handle and the authenticated
-- member information into our ``App'' monad (\autoref{App}) and then pass
-- control to a function based on the request method and URI.

module Main where

import Vocabulink.App
import Vocabulink.Article
import Vocabulink.Article.Html
import Vocabulink.CGI
import Vocabulink.Comment
import Vocabulink.Config
import Vocabulink.Forum
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Link.Html
import Vocabulink.Link.Pronunciation
import Vocabulink.Link.Story
import Vocabulink.Member
import Vocabulink.Member.Page
import Vocabulink.Member.Registration
import Vocabulink.Metrics
import Vocabulink.Page
import Vocabulink.Review
import Vocabulink.Search
import Vocabulink.Support
import Vocabulink.Utils

import Prelude hiding (div, span, id)

import Control.Concurrent (forkIO)
import Data.ConfigFile (get)
import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Database.TemplatePG (pgConnect)
import Network (PortID(..))
import Network.SCGI (runSCGIConcurrent')
import Network.URI (URI(..), unEscapeString)

-- Entry and Dispatch

-- When the program starts, it immediately begin listening for connections.
-- |runSCGIConcurrent'| spawns up to some number of threads. This matches the
-- number that cherokee, running in front of vocabulink.cgi, is configured for.

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
    ls <- liftIO $ languagesFromDB h
    handleErrors h (runApp h cp sd ls handleRequest))

-- |handleRequest| ``digests'' the requested URI before passing it to the
--  dispatcher.

handleRequest :: App CGIResult
handleRequest = do
  uri  <- requestURI
  meth <- requestMethod
  dispatch' meth (pathList uri)

-- We extract the path part of the URI, ``unescape it'' (convert % codes back
-- to characters), decode it (convert \mbox{UTF-8} characters to Unicode
-- Chars), and finally parse it into directory and filename components. For
-- example,

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
-- trailing slashes. We want only one URI to point to a resource.\footnote{I'm
-- not sure that this is the right thing to do. Would it be better just to give
-- the client a 404?}

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

-- If we could rely on the @DELETE@ method being supported by all browsers, this
-- would be a little less ugly. However, I've decided to only use @GET@ and
-- @POST@. All other methods are appended as an extra path component (here, as
-- |method'|).\footnote{I'm not 100\% satisfied with this design decision, but I
-- haven't thought of a better way yet.}

-- For clarity, this dispatches:

-- GET    /link/new              → form to create a new link
-- TODO: Change to POST /links
-- POST   /link/new              → create a new link
-- GET    /link/10               → link page
-- POST   /link/10/stories       → add a linkword story
-- POST   /link/10/pronunciation → add a pronunciation
-- DELETE /link/10               → delete link

-- Creating a new link is a 2-step process. First, the member requests a page
-- on which to enter information about the link. Then they @POST@ the details
-- to establish the link. (Previewing is done through the @GET@ as well.)

dispatch "GET"  ["link","new"] = withRequiredMember $ \ _ -> newLinkPage
dispatch "POST" ["link","new"] = createLink

dispatch meth ["link","story",x] =
  case maybeRead x of
    Nothing -> outputNotFound
    Just n  -> case meth of
                 "GET" -> maybe outputNotFound outputText =<< getStory n
                 "PUT" -> getBody >>= editStory n >> outputNothing
                 -- temporarily allow POST until AJAX forms are better
                 "POST" -> getRequiredInput "story" >>= editStory n >> referrerOrVocabulink >>= redirect
                 _     -> outputNotFound

dispatch meth ("link":x:meth') =
  case maybeRead x of
    Nothing -> outputNotFound
    Just n  -> case (meth, meth') of
                 ("GET"   , [])                -> linkPage n
                 ("DELETE", [])                -> deleteLink n
                 ("POST"  , ["stories"])       -> do
                   story <- getRequiredInput "story"
                   addStory n story
                   redirect $ "/link/" ++ show n
                 (_       , _)                 -> outputNotFound

dispatch "GET" ["pronunciations",lang,word] = getPronunciations lang word

-- Searching

-- Retrieving a listing of links is easier.

dispatch "GET" ["links"] = do
  ol <- getInput "ol"
  dl <- getInput "dl"
  case (ol, dl) of
    (Just ol', Just dl')  -> do
      ol'' <- languageNameFromAbbreviation ol'
      dl'' <- languageNameFromAbbreviation dl'
      case (ol'', dl'') of
        (Just ol''', Just dl''') -> linksPage ("Links from " ++ ol''' ++ " to " ++ dl''')
                                              (languagePairLinks ol' dl')
        _                        -> outputNotFound
    _                        -> linksPage "Latest Links" latestLinks

-- Site-wide search is done separately for now.

dispatch "GET" ["search"] = searchPage

-- Languages

-- Browsing through every link on the site doesn't work with a significant
-- number of links. A languages page shows what's available and contains
-- hyperlinks to language-specific browsing.

dispatch "GET" ["languages"] = languagePairsPage

-- Link Review

-- Members review their links by interacting with the site in a vaguely
-- REST-ish way. The intent behind this is that in the future they will be able
-- to review their links through different means such as a desktop program or a
-- phone application.

-- GET  /review/next  → retrieve the next link for review
-- POST /review/n     → mark link as reviewed
-- POST /review/n/add → add a link for review

-- (where n is the link number)

-- TODO: Change POST /review/n/add to something better (don't like action names
-- in the URI)

-- Reviewing links is one of the only things that logged-in-but-unverified
-- members are allowed to do.

dispatch meth ("review":rpath) = do
  memberNo' <- memberNumber <$$> asks appMember
  case memberNo' of
    Nothing       -> redirect =<< reversibleRedirect "/member/login"
    Just memberNo ->
      case (meth,rpath) of
        ("GET",  ["next"]) -> nextReview memberNo
        ("POST", x:xs)   ->
           case maybeRead x of
             Nothing -> outputNotFound
             Just n  -> case xs of
                          ["add"] -> newReview memberNo n
                          []      -> linkReviewed memberNo n
                          _       -> outputNotFound
        (_       ,_)       -> outputNotFound

-- Membership

-- Becoming a member is simply a matter of filling out a form.

dispatch "GET"  ["member","signup"] = registerMember
dispatch "POST" ["member","signup"] = registerMember

-- But to use most of the site, we require email confirmation.

dispatch "GET" ["member","confirmation"]   = confirmEmailPage
dispatch "GET" ["member","confirmation",x] = confirmEmail x

-- Logging in is a similar process.

dispatch "GET"  ["member","login"] = login
dispatch "POST" ["member","login"] = login

-- Logging out can be done without a form.

dispatch "POST" ["member","logout"] = logout

-- Members can also request support, if for some reason they can't or don't
-- want to use the forums.

dispatch "GET"  ["member","support"] = memberSupport
dispatch "POST" ["member","support"] = memberSupport

-- Member Pages

dispatch "GET" ("member":[username]) = memberPage username

-- Forums

-- While Vocabulink is still growing (and into the future), it's important to
-- help new members along and to get feedback from them. For this, Vocabulink
-- uses forums.

dispatch "GET"  ["forums"] = forumsPage
dispatch "POST" ["forums"] = forumsPage

-- Forums are uniquely identified by their name. The names are trusted to be
-- unique and reversibly mappable into URI-safe strings because they are
-- created by administrators of the site.

dispatch "POST" ["forum","new"] = createForum
dispatch "GET"  ["forum",x] = forumPage x

-- However, topics can be created by anyone and are identified by numbers. This
-- might seem like a lost opportunity for search engine optimization, but
-- including the forum topic text could lead to some very long URIs.

dispatch "POST" ["forum",x] = createForumTopic x

dispatch "GET" ["forum",x,y] =
  case maybeRead y of
    Nothing -> outputNotFound
    Just n  -> forumTopicPage x n

-- ``reply'' is used here as a noun.

dispatch meth ("comment":x:meth') =
  case maybeRead x of
    Nothing -> outputNotFound
    Just n  -> case (meth, meth') of
                 ("POST", ["reply"]) -> replyToComment n
                 ("POST", ["votes"]) -> voteOnComment n
                 (_     , _)         -> outputNotFound

-- Administrative Pages

dispatch "GET" ("admin":xs) = do
  memberNo <- memberNumber <$$> asks appMember
  case memberNo of
    Just 1 -> case xs of
                ["metrics"] -> metricsPage
                _           -> outputNotFound
    _      -> outputUnauthorized

-- Everything Else

-- For Google Webmaster Tools, we need to respond to a certain URI that acts as
-- a kind of ``yes, we really do run this site''.

dispatch "GET" ["google46b9909165f12901.html"] = outputNothing

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
  cloud <- wordCloud 40 261 248 12 32 6
  let page = $(hamletFile "../hamlet/frontpage.hamlet") hamletUrl
  stdPage "Welcome to Vocabulink" [CSS "front"] mempty page
