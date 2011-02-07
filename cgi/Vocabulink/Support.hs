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

-- At some point members (or non-members) may have difficulties with the site.
-- I debated about giving out an email address, but I suspect that filtering
-- for spam among support requests might be pretty difficult. I also don't want
-- to go dead to support request email. So this uses the tried and true contact
-- form method.

module Vocabulink.Support (memberSupport, getSupportForm) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Form
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import qualified Text.Blaze.Html5.Formlets as HF

import Prelude hiding (div, id, span)

supportForm :: Maybe String -> App (AppForm (String, String, String))
supportForm redirect' = do
  ref <- referrerOrVocabulink
  email <- memberEmail <$$> asks appMember
  let redirect'' = fromMaybe ref redirect'
      emailInput = case email of
                     Nothing -> plug (tabularInput "Email Address") (HF.input Nothing)
                                  `check` ensures
                                    [((/= ""), "We need an email address to contact you at.")]
                     Just em -> HF.hidden em
  return $ plug (\html -> table $ do
                            html
                            tfoot $ tabularSubmit "Get Support")
             ((,,) <$> emailInput
                   <*> plug (tabularInput "Problem") (HF.textarea Nothing Nothing Nothing)
                         `check` ensures
                            [((/= ""), "It would help us to know \
                                       \what the problem you're experiencing is ;).")]
                   <*> HF.hidden (Just redirect''))

-- Get a fresh support form (don't attempt to run it).

getSupportForm :: Maybe String -> App Html
getSupportForm redirect' = do
  (_, html) <- runForm' =<< supportForm redirect'
  return $ form ! action "/member/support" ! method "post" $ html

-- And finally, here is the actual support page. It's not just for member
-- support. If the client isn't logged in it will ask for a contact email
-- address.

-- Because support is so critical, in case there's an error submitting the
-- support form we fall back to a secondary (disposable) support address.

memberSupport :: App CGIResult
memberSupport = do
  form' <- supportForm =<< getInput "redirect"
  res <- runForm form' $ Right mempty
  case res of
    Left html -> simplePage "Need Help?" mempty $ do
      div ! id "central-column" $ do
        p ! style "text-align: center" $ do
          string "Have you checked the "
          a ! href "/forum/help" $ "help forum"
          string "?"
      html
    Right (email, problem, redirect') -> do
      supportAddress <- fromJust <$> getOption "supportaddress"
      res' <- liftIO $ sendMail supportAddress "Support Request" $
                unlines [  "Email: " ++ email,
                           "Problem: " ++ problem ]
      case res' of
        Nothing  -> error "Error sending support request. \
                          \Please contact support@vocabulink.com for support."
        Just _   -> simplePage "Support Request Sent" mempty $ do
                      div ! id "central-column" $ do
                        p $ "Your support request was sent successfully."
                        p $ do
                          a ! href (stringValue redirect') $ "Click here to go back"
                          string " to where you came from."
