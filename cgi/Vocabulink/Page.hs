-- Copyright 2010, 2011, 2012 Chris Forno

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
import Vocabulink.CGI
import Vocabulink.Html hiding (title, style)
import Vocabulink.Member
import Vocabulink.Utils

import Text.Blaze.Html5 (docTypeHtml, head, noscript, link, body, title, style)
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
  let deps' = [CSS "common", JS "common"]
           ++ (isJust member ? [CSS "member", JS "member"] $ [])
           ++ deps
  cssDeps     <- mapM includeDep [ css | css@(CSS _) <- deps' ]
  jsDeps      <- mapM includeDep [ js  |  js@(JS _)  <- deps' ]
  return $ docTypeHtml $ do
    head $ do
      title $ toHtml title'
      link ! rel "icon" ! type_ "image/png" ! href "http://s.vocabulink.com/img/favicon.png"
      mconcat cssDeps
      inlineCSS $ intercalate "\n" [ css | InlineCSS css <- deps' ]
      head'
    body $ do
      div ! id "page" $ do
        div ! id "head" $ headerB
        when (length jsDeps > 0) (noscript $ p "This page requires JavaScript for some functionality.")
        div ! id "body" $ body'
        div ! id "foot" $ footerB
      inlineJS $ memberJS member
      script ! src "http://www.google-analytics.com/ga.js" $ mempty
      mconcat jsDeps
      inlineJS $ intercalate "\n" [ js | InlineJS js <- deps' ]
      readyJS $ intercalate "\n" [ js | ReadyJS js <- deps' ])
 where memberJS m =
         unlines [ "var V = {" -- the Vocabulink object
                 , "  memberName: " ++ maybe "null" (\m' -> "'" ++ memberName m' ++ "'") m ++ ","
                 , "  gravatarHash: " ++ maybe "null" (\h -> "'" ++ h ++ "'") (gravatarHash =<< memberEmail =<< m)
                 , "};"
                 ]
       inlineCSS = (style ! type_ "text/css") . toHtml
       readyJS js = inlineJS $ "(function ($) {$(function () {" ++ js ++ "})})(jQuery);"

-- Often we just need a simple page where the title and header are the same.

simplePage :: String -> [Dependency] -> Html -> App CGIResult
simplePage t deps body' = stdPage t deps mempty $ mappend (h1 $ toHtml t) body'

-- Each dependency is expressed as the path from the root of the static files
-- subdomain (for now, @s.vocabulink.com@) to the file. Do not include the file
-- suffix (@.css@ or @.js@); it will be appended automatically. These are meant
-- for inclusion in the @<head>@ of the page.

-- |includeDep| also needs to check dependency versions for cache busting.

includeDep :: Dependency -> App Html
includeDep d = do
  version <- dependencyVersion d
  return $ case version of
    Nothing -> inlineJS $ "alert('Dependency \"" ++ show d ++"\" not found.');"
    Just v  ->
      case d of
        CSS css -> link ! href (toValue $ "http://s.vocabulink.com/css/" ++ css ++ ".css?" ++ v)
                        ! rel "stylesheet"
                        ! type_ "text/css"
        JS  js  -> script ! src (toValue $ "http://s.vocabulink.com/js/" ++ js ++ ".js?" ++ v) 
                          $ mempty
        _ -> error "Can only include CSS and JS."

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
                            return $ mconcat [box, " | "]
  dashboard <- case member of
                 Nothing -> return mempty
                 Just _  -> return $ mconcat [a ! href "/dashboard" $ "dashboard", " | "]
  return $ do
    a ! href "/" ! accesskey "1" $ do
      img ! class_ "logo" ! alt "Vocabulink: Learn Languages through Fiction"
                          ! src "http://s.vocabulink.com/img/logo-compact-beta.png"
    div ! id "head-decoration" $ mempty
    div ! id "head-bar" $ do
      searchBox
      review
      dashboard
      maybe loginBox logoutBox member

-- The footer displays a number of common (or what we believe to be common)
-- hyperlinks for English speakers.

footerBar :: App Html
footerBar = do
  copy <- liftIO copyrightNotice
  mailto <- (("mailto:support@vocabulink.com?subject=support%20request" ++) . maybe "" (\m -> "%20from%20" ++ escapeURIString' (memberName m)) <$> asks appMember)::(App String)
  return $ do
    unordList [ a ! href "https://getsatisfaction.com/vocabulink" $ "help"
              , a ! href "/links" $ "languages"
              , a ! href "/articles" $ "articles"
              , a ! href (toValue mailto) ! class_ "contact-us" $ "contact us"
              , a ! href "/privacy" $ "privacy policy"
              , a ! href "/terms-of-use" $ "terms of use"
              , a ! href "/source" $ "source code"
              -- , a ! href "/api" $ "API"
              ] ! class_ "hyperlinks"
    p $ do
      copy
      span ! id "design-attribution" $ do
        "Design by: "
        a ! href "http://www.designcharisma.com" $ "Design Charisma"

-- We want a copyright notice at the bottom of every page. Since this is a
-- copyright notice for dynamic content, we want it to be up-to-date with the
-- generation time (now).

copyrightNotice :: IO Html
copyrightNotice = do
  year <- currentYear
  return $ span ! class_ "copyright" $ do
    "Copyright 2008–"
    toHtml (show year ++ " ")
    a ! href "http://jekor.com/" $ "Chris Forno"

-- The following are just login and signup buttons.

loginBox :: Html
loginBox = span ! class_ "auth-box login" $ do
  a ! id "login-button" ! class_ "faint-gradient-button green" ! href "" $ "Log In"
  a ! id "signup-button" ! class_ "faint-gradient-button blue" ! href "" $ "Sign Up"

-- For logged-in members, we provide a logout button (with an indicator of your
-- username to show that you're logged in).

logoutBox :: Member -> Html
logoutBox member = form ! class_ "auth-box logout" ! action "/member/logout" ! method "post" $ do
  a ! href (toValue $ "/user/" ++ memberName member) $ toHtml (memberName member)
  input ! type_ "submit" ! id "logout-button" ! class_ "faint-gradient-button green" ! value "Log Out"

-- Students with a goal in mind will want to search for words they're studying
-- rather than browse randomly. We display a search box for them at the top of the
-- page.

searchBox :: Html
searchBox = form ! class_ "search-box" ! action "/search" $ do
  div $ do
    input ! type_ "search" ! name "q" ! accesskey "s"
    " "
    input ! type_ "submit" ! class_ "button dark" ! value "Search"

reviewBox :: Integer -- ^ the number of links due for review (presumably from numLinksToReview)
          -> Html
reviewBox n = a ! href "/learn?learn=es&known=en" ! class_ "review-box" $ message n
  where message 1 = strong "1" >> " link to review"
        message _ = strong (toHtml n) >> " links to review"

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
        then a ! href (toValue $ path ++ prev) ! class_ "prev" $ "Previous"
        else span ! class_ "prev" $ "Previous"
      " "
      if pg * n < total
        then a ! href (toValue $ path ++ next) ! class_ "next" $ "Next"
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
