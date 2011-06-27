-- Copyright 2010, 2011 Chris Forno

-- This file is part of Vocabulink.

-- Vocabulink is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.

-- Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

module Vocabulink.Page ( stdPage, simplePage
                       , currentPage, pager
                       ) where

import Vocabulink.App
import Vocabulink.Article
import Vocabulink.CGI
import Vocabulink.Html hiding (title)
import Vocabulink.Member
import Vocabulink.Utils

import Data.List (partition)
import Text.Blaze.Html5 (docTypeHtml, head, noscript, link, body, title)
import Text.Blaze.Html5.Attributes (rel)
import Text.Regex (mkRegex, subRegex)
import Text.Regex.TDFA ((=~))

import Prelude hiding (div, span, id, head)

-- |stdPage| takes a title, a list of dependencies, and list of HTML objects to
-- place into the body of the page. It automatically adds a standard header and
-- footer. It also includes @page.css@ and conditionally includes an Internet
-- Explorer-specific stylesheet for the few cases when there's no other way to
-- work around a defect in Internet Explorer that would otherwise seriously impact
-- usability.

-- If any JavaScript files are required, |stdPage| will automatically add a
-- @<noscript>@ warning to the top of the page.

stdPage :: String -> [Dependency] -> Html -> Html -> App CGIResult
stdPage title' deps head' body' = outputHtml =<< (do
  headerB <- headerBar
  footerB <- footerBar
  member <- asks appMember
  let deps' = [CSS "lib.common", JS "common"]
           ++ (isJust member ? [CSS "lib.member", JS "member"] $ [])
           ++ deps
      (cssDeps, jsDeps) = partition (\x -> case x of
                                             (CSS _) -> True
                                             (JS  _) -> False) deps'
  cssDeps' <- mapM includeDep cssDeps
  jsDeps'  <- mapM includeDep jsDeps
  return $ docTypeHtml $ do
    head $ do
      mconcat cssDeps'
      title $ string title'
      link ! rel "icon" ! type_ "image/png" ! href "http://s.vocabulink.com/img/favicon.png"
      head'
    body $ do
      div ! id "head" $ headerB
      when (jsDeps /= []) (noscript $ p "This page requires JavaScript for some functionality.")
      div ! id "body" $ body'
      div ! id "foot" $ footerB
      script ! src "http://www.google-analytics.com/ga.js" $ mempty
      mconcat jsDeps')

-- Often we just need a simple page where the title and header are the same.

simplePage :: String -> [Dependency] -> Html -> App CGIResult
simplePage t deps body' = stdPage t deps mempty $ mappend (h1 $ string t) body'

-- Each dependency is expressed as the path from the root of the static files
-- subdomain (for now, @s.vocabulink.com@) to the file. Do not include the file
-- suffix (@.css@ or @.js@); it will be appended automatically. These are meant
-- for inclusion in the @<head>@ of the page.

-- |includeDep| also needs to check dependency versions for cache busting.

includeDep :: Dependency -> App Html
includeDep d = do
  version <- dependencyVersion d
  return $ case version of
    Nothing -> script ! type_ "text/javascript" $
                  preEscapedString ("alert('Dependency \"" ++ show d ++"\" not found.');")
    Just v  ->
      case d of
        CSS css -> link ! href (stringValue $ "http://s.vocabulink.com/css/" ++ css ++ ".css?" ++ v)
                        ! rel "stylesheet"
                        ! type_ "text/css"
        JS  js  -> script ! src (stringValue $ "http://s.vocabulink.com/js/" ++ js ++ ".js?" ++ v) $
                     mempty

-- The standard header bar shows the Vocabulink logo (currently just some
-- text), a list of hyperlinks, a search box, and either a login/sign up button
-- or a logout button. If the page is being served to a logged-in member it
-- also includes a notice about the number of links that the member has waiting
-- for review.

headerBar :: App Html
headerBar = do
  member <- asks appMember
  review <- case member of
              Nothing -> return mempty
              Just m  -> do box <- reviewBox <$> numLinksToReview m
                            return $ mconcat [box, string " | "]
  let newLink = case member of
                  Nothing -> mempty
                  Just _  -> mconcat [a ! href "/link/new" ! class_ "new-link" $ string "new link", string " | "]
  return $ do
    a ! href "/" ! accesskey "1" $
      img ! class_ "logo" ! alt "Vocabulink: Learn Languages through Fiction"
                          ! src "http://s.vocabulink.com/img/logo-compact.png"
    div ! id "head-decoration" $ mempty
    div ! id "head-bar" $ do
      searchBox
      newLink
      review
      maybe loginBox logoutBox member
    clear

-- The footer displays a number of common (or what we believe to be common)
-- hyperlinks for English speakers.

languageLinks :: App [Html]
languageLinks = do
  rows' <- $(queryTuples' "SELECT abbr, name \
                          \FROM language_frequency_to_english \
                          \ORDER BY freq DESC LIMIT 11")
  return $ map languageLink rows' ++ [a ! href "/languages" $ "more..."]
 where languageLink (abbr', name') =
         a ! href (stringValue $ "/links?ol=" ++ fromJust abbr' ++ "&dl=en") $
           string $ fromJust name'

footerBar :: App Html
footerBar = do
  langLinks <- languageLinks
  articles <- (map articleLinkHtml . take 3) <$> getArticles
  copy <- liftIO copyrightNotice
  return $ do
    div ! id "handy-links-background" $ do
      multiColumn [ do h2 $ a ! href "/languages" $ "Browse Links by Language"
                       multiColumnList 3 langLinks
                  , do h2 $ a ! href "/articles" $ "Latest Articles"
                       multiColumnList 1 (articles ++ [a ! href "/articles" $ "more..."])
                  ] ! id "handy-links"
    unordList [ a ! href "https://getsatisfaction.com/vocabulink" $ "help"
              , a ! href "#" ! class_ "contact-us" $ "contact us"
              , a ! href "/privacy" $ "privacy policy"
              , a ! href "/terms-of-use" $ "terms of use"
              , a ! href "/source" $ "source"
              -- , a ! href "/api" $ "API"
              ] ! class_ "hyperlinks"
    p $ do
      copy
      span ! id "design-attribution" $ do
        string "Design by: "
        a ! href "http://www.designcharisma.com" $ "Design Charisma"
      span ! id "forvo-attribution" $ do
        string "Pronunciations by: "
        a ! href "http://www.forvo.com/" $ "Forvo"
 where articleLinkHtml article =
         a ! href (stringValue $ "/article/" ++ articleFilename article) $
           string $ articleTitle article

-- We want a copyright notice at the bottom of every page. Since this is a
-- copyright notice for dynamic content, we want it to be up-to-date with the
-- generation time (now).

copyrightNotice :: IO Html
copyrightNotice = do
  year <- currentYear
  return $ span ! class_ "copyright" $ do
    string "Copyright 2008–"
    string (show year ++ " ")
    a ! href "http://jekor.com/" $ "Chris Forno"

-- The following are just login and signup buttons.

loginBox :: Html
loginBox = span ! class_ "auth-box login" $ do
  a ! id "login-button" $ "Log in"
  string " | "
  a ! id "signup-button" $ "Sign up"

-- For logged-in members, we provide a logout button (with an indicator of your
-- username to show that you're logged in).

logoutBox :: Member -> Html
logoutBox member = form ! class_ "auth-box logout" ! action "/member/logout" ! method "post" $ do
  a ! href (stringValue $ "/user/" ++ memberName member) $ (string $ memberName member)
  input ! type_ "submit" ! id "logout-button" ! class_ "button dark" ! value "Log Out"

-- Students with a goal in mind will want to search for words they're studying
-- rather than browse randomly. We display a search box for them at the top of the
-- page. This also is currently the only way to create new links (aside from
-- entering in the URL manually), but that might change in the future.

-- Currently, this uses a combination of our search logic and Google Custom Search.

searchBox :: Html
searchBox = form ! id "cse-search-box" ! class_ "search-box" ! action "/search" $ do
  div $ do
    input ! type_ "hidden" ! name "cx" ! value "011479832181784786223:2ibwsl9f6ca"
    input ! type_ "hidden" ! name "cof" ! value "FORID:9"
    input ! type_ "hidden" ! name "ie" ! value "UTF-8"
    input ! type_ "search" ! name "q" ! accesskey "s"
    string " "
    input ! type_ "submit" ! id "sa" ! name "sa" ! class_ "button dark" ! value "Search"
  script ! src "http://www.google.com/cse/brand?form=cse-search-box&lang=en" $ mempty

reviewBox :: Integer -- ^ the number of links due for review (presumably from numLinksToReview)
          -> Html
reviewBox n = a ! href "/review" ! class_ "review-box" $ message n
  where message 1 = strong "1" >> " link to review"
        message _ = strong (string $ prettyPrint n) >> " links to review"

-- This retrievs the number of links that a user has for review right now.

numLinksToReview :: Member -> App Integer
numLinksToReview member = fromJust . fromJust <$> $(queryTuple'
  "SELECT COUNT(*) FROM link_to_review \
  \WHERE member_no = {memberNumber member} AND current_timestamp > target_time")

-- Paging

-- We'd like to have a consistent way of ``paging'' lists that don't fit on a
-- single page. This can be used to page search results, a set of links,
-- articles, etc.

-- This reads the page query parameters and returns them along with the current
-- offset (as a convenience). In the absence of certain parameters, we fall
-- back to reasonable defaults like 10 items per page. We also don't want to
-- chew up resources retrieving too many items, so we cap the max that a client
-- can request at 100.

-- Limiting the paging elements to Int bounds is necessary for the functions
-- that use the pager (they often need to |take| some number of tuples from a
-- list, for instance) and does not limit the design of our HTTP interface
-- much, if at all. I cannot think of an instance where we'd need to go past
-- the 65,000th page of anything.

currentPage :: App (Int, Int, Int)
currentPage = do
  pg  <- readInputDefault 1 "pg"
  n'  <- readInputDefault 10 "n"
  let  n''     = n' > 100 ? 100 $ n'
       n       = n'' < 1 ? 1 $ n''
       offset  = (pg - 1) * n
  return (pg, n, offset)

-- This will handle the query string in the hyperlinks it generates while it
-- replaces the @n@ (number of items per page) and @page@ (the page we're on)
-- parameters. We give it the page we're currently on, the number of items per
-- page, and the total number of items available, and it does the rest.

-- This doesn't actually display clickable numeric hyperlinks such as you'd see on
-- a Google search results page. It only provides the client with ``previous'' and
-- ``next''. We do this because determining the number of pages in a result can be
-- expensive.

pager :: Int -> Int -> Int -> App Html
pager pg n total = do
  q'  <- getVar "QUERY_STRING"
  uri <- requestURI
  let path = uriPath uri
      q    = maybe "" decodeString q'
      prev = pageQueryString n (pg - 1) q
      next = pageQueryString n (pg + 1) q
  return $ p ! class_ "pager" $
    span ! class_ "controls" $ do
      if pg > 1
        then a ! href (stringValue $ path ++ prev) ! class_ "prev" $ "Previous"
        else span ! class_ "prev" $ "Previous"
      string " "
      if pg * n < total
        then a ! href (stringValue $ path ++ next) ! class_ "next" $ "Next"
        else span ! class_ "next" $ "Next"

-- Creating the query string involves keeping the existing query string intact as
-- much as possible. We even want the position of the parameters to stay the same
-- if they're already there.

pageQueryString :: Int -> Int -> String -> String
pageQueryString n pg q =
  let q1 = q  =~ nRegex  ? subRegex (mkRegex nRegex)  q  ("n=" ++ show n)
                         $ q  ++ ("&n=" ++ show n)
      q2 = q1 =~ pgRegex ? subRegex (mkRegex pgRegex) q1 ("pg=" ++ show pg)
                         $ q1 ++ ("&pg=" ++ show pg) in
  '?':q2
 where nRegex  = "n=[^&]+"
       pgRegex = "pg=[^&]+"
