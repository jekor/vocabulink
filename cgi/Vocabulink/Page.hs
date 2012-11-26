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
                       ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html hiding (title, style)
import Vocabulink.Member
import Vocabulink.Utils

import Text.Blaze.Html5 (docTypeHtml, head, noscript, link, body, title, style)
import Text.Blaze.Html5.Attributes (rel)

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
       readyJS [] = mempty
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
                          ! src "http://s.vocabulink.com/img/logo.png"
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
  addr <- fromJust <$> getOption "supportaddress"
  mailto <- ((("mailto:" ++ addr ++ "?subject=support%20request") ++) . maybe "" (\m -> "%20from%20" ++ escapeURIString' (memberName m)) <$> asks appMember)::(App String)
  return $ do
    unordList [ a ! href "https://getsatisfaction.com/vocabulink" $ "Help"
              , a ! href "/articles" $ "Articles"
              , a ! href (toValue mailto) ! class_ "contact-us" $ "Contact Us"
              , a ! href "/privacy" $ "Privacy Policy"
              , a ! href "/terms-of-use" $ "Terms of Use"
              , a ! href "/source" $ "Source Code"
              , a ! href "http://twitter.com/vocabulink" $ do
                  sprite "icon" "twitter"
                  " Twitter"
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
