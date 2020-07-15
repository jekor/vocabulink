module Vocabulink.Page (stdPage, simplePage, Dependency(..)) where

import Vocabulink.Env
import Vocabulink.Html hiding (title, style)
import Vocabulink.Member
import Vocabulink.Utils

import qualified Data.ByteString.Lazy.UTF8 as BLU
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

stdPage :: E (String -> [Dependency] -> Html -> Html -> IO Html)
stdPage title' deps head' body' = do
  review <- reviewBox
  let extraLinks = review : case ?member of
                              Nothing -> []
                              Just m -> [ a ! href "/dashboard" $ "dashboard"
                                        , a ! href (toValue $ "/user/" ++ memberName m) $ toMarkup (memberName m) ]
  return $ docTypeHtml $ do
    head $ do
      title $ toMarkup title'
      link ! rel "icon" ! type_ "image/png" ! href "//s.vocabulink.com/img/favicon.png"
      mconcat cssDeps
      inlineCSS $ intercalate "\n" [ css | InlineCSS css <- deps' ]
      head'
    body $ do
      div ! id "page" $ do
        div ! id "head" $ headerBar extraLinks
        when (length jsDeps > 0) (noscript $ p "This page requires JavaScript for some functionality.")
        div ! id "body" $ body'
        div ! id "foot" $ footerBar
      inlineJS $ memberJS
      mconcat jsDeps
      inlineJS $ intercalate "\n" [ js | InlineJS js <- deps' ]
      readyJS $ intercalate "\n" [ js | ReadyJS js <- deps' ]
 where deps' = [CSS "common", JS "common"] ++ maybe [] (const [CSS "member", JS "member"]) ?member ++ deps
       cssDeps = map includeDep [ css | css@(CSS _) <- deps' ]
       jsDeps  = map includeDep [ js  |  js@(JS _)  <- deps' ]
       memberJS = "var V = {member: " ++ BLU.toString (encode $ toJSON ?member) ++ "};" -- the Vocabulink object
       inlineCSS = (style ! type_ "text/css") . toMarkup
       readyJS [] = mempty
       readyJS js = inlineJS $ "(function ($) {$(function () {" ++ js ++ "})})(jQuery);"

-- Often we just need a simple page where the title and header are the same.
simplePage t deps body' = stdPage t deps mempty $ mappend (h1 $ toMarkup t) body'

data Dependency = CSS FilePath | JS FilePath | InlineCSS String | InlineJS String | ReadyJS String
                  deriving (Eq, Show)

-- Each dependency is expressed as the path from the root of the static files
-- subdomain (for now, @s.vocabulink.com@) to the file. Do not include the file
-- suffix (@.css@ or @.js@); it will be appended automatically. These are meant
-- for inclusion in the @<head>@ of the page.

-- |includeDep| also needs to check dependency versions for cache busting.

includeDep :: E (Dependency -> Html)
includeDep d =
  case lookup (depPath d) staticManifest of
    Nothing -> inlineJS $ "alert('Dependency \"" ++ show d ++"\" not found.');"
    Just checksum ->
      case d of
        CSS _ -> link ! href (toValue (depURL checksum))
                      ! rel "stylesheet"
                      ! type_ "text/css"
        JS  _ -> script ! src (toValue (depURL checksum))
                        $ mempty
        _ -> error "Can only include CSS and JS."
 where depPath (CSS n) = "css/" ++ n ++ ".css"
       depPath (JS n) = "js/" ++ n ++ ".js"
       depPath _ = error "Only CSS and JS dependencies live on the filesystem."
       depURL checksum = "//s.vocabulink.com/" ++ depPath d ++ "?" ++ take 7 checksum

-- The standard header bar shows the Vocabulink logo (currently just some
-- text), a list of hyperlinks, a search box, and either a login/sign up button
-- or a logout button. If the page is being served to a logged-in member it
-- also includes a notice about the number of links that the member has waiting
-- for review.

headerBar :: E ([Html] -> Html)
headerBar extras = do
  a ! href "/" ! accesskey "1" $ do
    img ! class_ "logo" ! alt "Vocabulink: Learn Languages through Fiction"
        ! src "//s.vocabulink.com/img/logo.png"
  div ! id "head-decoration" $ mempty
  div ! id "head-bar" $ do
    searchBox
    mconcat $ intersperse " | " extras
    maybe loginBox (\_ -> logoutBox) ?member

footerBar :: E (Html)
footerBar = do
  unordList [ a ! href "/articles" $ "Articles"
            , a ! href (toValue mailto) ! class_ "contact-us" $ "Contact Us"
            , a ! href "/privacy" $ "Privacy Policy"
            , a ! href "/terms-of-use" $ "Terms of Use"
            , a ! href "https://github.com/jekor/vocabulink" $ "Source Code"
            , a ! href "http://twitter.com/vocabulink" $ do
                sprite "icon" "twitter"
                " Twitter"
            ] ! class_ "hyperlinks"
  p $ do
    copyrightNotice
 where mailto = "mailto:support@vocabulink.com?subject=support%20request" ++ (maybe "" (\m -> "%20from%20" ++ escapeURIString' (memberName m)) ?member)

-- We want a copyright notice at the bottom of every page. Since this is a
-- copyright notice for dynamic content, we want it to be up-to-date with the
-- generation time (now).

copyrightNotice :: Html
copyrightNotice = span ! class_ "copyright" $ do
  "Copyright 2008–"
  toMarkup (show compileYear ++ " ")
  a ! href "http://jekor.com/" $ "Chris Forno"

-- The following are just login and signup buttons.

loginBox :: Html
loginBox = span ! class_ "auth-box login" $ do
  a ! id "login-button" ! class_ "faint-gradient-button green" ! href "" $ "Log In"
  a ! id "signup-button" ! class_ "faint-gradient-button blue" ! href "" $ "Sign Up"

-- For logged-in members, we provide a logout button (with an indicator of your
-- username to show that you're logged in).

logoutBox :: Html
logoutBox = form ! class_ "auth-box logout" ! action "/member/logout" ! method "post" $ do
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

reviewBox :: E (IO Html)
reviewBox = do
  due <- maybe (return 0) numDue ?member
  return $ a ! href "/review?learn=es&known=en" ! class_ "review-box" $ message due
 where numDue m = (fromJust . fromJust) `liftM` $(queryTuple
                    "SELECT COUNT(*) FROM link_to_review \
                    \INNER JOIN link USING (link_no) \
                    \WHERE member_no = {memberNumber m} \
                      \AND learn_lang = 'es' AND known_lang = 'en' \
                      \AND current_timestamp > target_time \
                      \AND NOT deleted") ?db
       message :: Int64 -> Html
       message 1 = strong "1" >> " word to review"
       message n = strong (toMarkup n) >> " words to review"
