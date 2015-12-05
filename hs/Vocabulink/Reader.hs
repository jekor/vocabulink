-- Copyright 2008, 2009, 2010, 2011, 2012, 2013 Chris Forno

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

-- Readers introduce words in context.

module Vocabulink.Reader ( readerPage
                         , readerPurchasePage, purchaseReader
                         ) where

import Vocabulink.CGI
import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Web.Stripe.Charge (chargeTokenById, Charge(..), ChargeId(..))
import Web.Stripe.Client (APIKey(..), defaultConfig, runStripeT)
import Web.Stripe.Token (TokenId(..))
import Web.Stripe.Utils (Amount(..), Currency(..), Description(..))

import Prelude hiding (div, id, span)

data ReaderAccess = ReaderGranted | ReaderPreview | ReaderDenied
  deriving (Eq)

readerPage :: E (String -> String -> Int -> IO (Maybe Html))
readerPage lang name' page = do
  readability <- readable
  if readability /= ReaderDenied
    then do
      row <- $(queryTuple "SELECT title, body, (SELECT MAX(page_no) \
                                               \FROM reader_page \
                                               \INNER JOIN reader USING (reader_no) \
                                               \WHERE short_name = {name'} AND lang = {lang}) \
                          \FROM reader_page \
                          \INNER JOIN reader USING (reader_no) \
                          \WHERE short_name = {name'} AND lang = {lang} \
                            \AND page_no = {page}") ?db
      case row of
        (Just (title', body, Just maxPage)) -> do
          when (readability == ReaderGranted && isJust ?member) $ do
            let memberNo = memberNumber $ fromJust ?member
            liftIO $ $(execute "UPDATE member_reader SET page_no = {page} \
                               \WHERE member_no = {memberNo} \
                                 \AND reader_no = (SELECT reader_no FROM reader \
                                                  \WHERE short_name = {name'} AND lang = {lang})") ?db
          Just `liftM` (stdPage (title' ++ " - Page " ++ show page ++ " - A Vocabulink " ++ language ++ " Reader") [CSS "reader", JS "reader", JS "link"] mempty $ do
            div ! id "book" $ do
              when (page > 1) $ a ! class_ "pager prev sprite sprite-icon-arrow-left" ! title "Previous Page" ! href (toValue (page > 1 ? show (page - 1) $ ".")) $ mempty
              let nextRef = case readability of
                              ReaderPreview -> "purchase"
                              _ -> show (page + 1)
              when (page < maxPage) $ a ! class_ "pager next sprite sprite-icon-arrow-right" ! title "Next Page" ! href (toValue $ nextRef) $ mempty
              div ! class_ "page left" $ do
                div ! class_ "header" $ do
                  span ! class_ "title" $ toMarkup title'
                  span ! class_ "page-number" $ toMarkup (show page)
                fromRight "Failed to parse page." (markdownToHtml body)
              div ! class_ "page right" $ do
                p $ do
                  i ! class_ "sprite sprite-icon-wizard" $ mempty
                  "Click a word on the left to see some information about it here."
              div ! style "clear: both" $ mempty) -- We can't use overflow: hidden here.
        _ -> return Nothing
    else return Nothing -- TODO: Should be access denied. Instead of Maybe we should be using an Either to provide failure types.
 where language = fromMaybe "Unknown Language" $ lookup lang languages
       readable = do
         access <- readerHasAccess lang name'
         return $ if access
                    then ReaderGranted
                    else if page == 1 then ReaderPreview else ReaderDenied

readerHasAccess :: E (String -> String -> IO Bool)
readerHasAccess lang name' =
  case ?member of
    Nothing -> return False
    Just m -> isJust `liftM` $(queryTuple "SELECT 1 \
                                          \FROM reader \
                                          \INNER JOIN member_reader USING (reader_no) \
                                          \WHERE short_name = {name'} AND lang = {lang} AND member_no = {memberNumber m}") ?db

readerPurchasePage :: E (String -> String -> SCGI Response)
readerPurchasePage lang name' = do
  access <- liftIO $ readerHasAccess lang name'
  if access
    then do
      -- TODO: Find the user's position in the reader and send them there.
      redirect $ "/reader/" ++ lang ++ "/" ++ name' ++ "/2"
    else do
      row <- liftIO $ $(queryTuple "SELECT title, reader_no, price FROM reader WHERE short_name = {name'} AND lang = {lang}") ?db
      case row of
        Just (title', readerNo, price) ->
          toResponse $ stdPage ("Purchase \"" ++ title' ++ "\" - A Vocabulink " ++ language ++ " Reader") [JS "purchase"] (do
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0, user-scalable=no"
            preEscapedToMarkup ("<!--Fixes for Internet Explorer CSS3 and HTML5--> \
                                \<!--[if gte IE 9]> \
                                \<style type=\"text/css\"> \
                                \  .gradient { filter: none!important;} \
                                \</style> \
                                \<![endif]--> \
                                \<!--[if lt IE 9]> \
                                \<script> \
                                \  'article aside footer header nav section time'.replace(/\\w+/g,function(n){document.createElement(n)}) \
                                \</script> \
                                \<![endif]--> \
                                \<link rel=\"stylesheet\" href=\"//s.vocabulink.com/css/off-the-shelf/style.css\" media=\"screen, projection\"> \
                                \<!--[if lt IE 9]> \
                                \<link rel=\"stylesheet\" href=\"//s.vocabulink.com/css/off-the-shelf/style_ie8.css\" media=\"screen, projection\"> \
                                \<![endif]--> \
                                \<link href=\"//netdna.bootstrapcdn.com/font-awesome/3.2.0/css/font-awesome.css\" rel=\"stylesheet\">"::String)) $ do
            section ! id "banner" $ do
              div ! class_ "row" $ do
                div ! id "shelf" ! class_ "one_half" $ do
                  img ! alt "book cover" ! width "331" ! height "497" ! src (toValue $ "//s.vocabulink.com/img/reader/" ++ lang ++ "/" ++ name' ++ ".jpg")
                div ! class_ "one_half last" $ do
                  hgroup $ do
                    h1 $ "Continue Learning 300 Spanish Words"
                    h2 ! class_ "subheader" $ do
                      "Get 50% off for a limited time."
                  p $ toMarkup $ "Thanks for previewing \"" ++ title' ++ "\". To continue learning, click below to purchase the book. You'll also get instant access to our scheduled word review system. Your support will help us write and develop the next Vocabulink Spanish reader!"
                  div ! class_ "button_buy" $ do
                    a ! id "checkout" ! class_ "gradient" ! title "Buy eBook" ! customAttribute "description" (toValue $ title' ++ " eBook") ! customAttribute "reader" (toValue $ show readerNo) ! customAttribute "price" (toValue $ show price) $ do
                      span ! class_ "button_price" $ do
                        del "$19.95"
                        " "
                        strong "$9.95"
                      span ! class_ "button_text" $ "Buy eBook"
            article $ do
              div ! id "main_content" $ do
                section ! id "features" $ do
                  div ! class_ "row" $ do
                    h2 ! class_ "section_title" $ do
                      span $ "What You Get Access To"
                    ul $ do
                      li ! class_ "one_half" $ do
                        i ! class_ "icon-book icon-4x" $ mempty
                        h4 "200+ Pages of Content"
                        p "If printed as a trade paperback book, the content would cover more than 200 pages. But instead of having to flip back and forth to look up words, the electronic version keeps track of everything for you."
                      li ! class_ "one_half last" $ do
                        i ! class_ "icon-magic icon-4x" $ mempty
                        h4 "300 Words, 234 Mnemonic Stories"
                        p "You'll learn 300 of the most common Spanish words. 234 of those words have an accompanying mnemonic story to help you remember. The other 66 are so similar to their English equivalents that you don't need one."
                      li ! class_ "one_half" $ do
                        i ! class_ "icon-volume-up icon-4x" $ mempty
                        h4 "Word Pronunciations"
                        p "We had a professional Spanish-speaking voice actor record pronunciations for nearly 3,000 Spanish words. Almost all base words introduced in the book include a clickable pronunciation."
                      li ! class_ "one_half last" $ do
                        i ! class_ "icon-calendar icon-4x" $ mempty
                        h4 "Automatically Scheduled Reviews"
                        p "Learn at your own pace: read as much or as little as you want. We keep track of every new word you learn. Then, we automatically schedule optimal times for you to review the words so that you don't forget them."
            script ! src "https://js.stripe.com/v2/" $ mempty
        _ -> return notFound
 where language = fromMaybe "Unknown Language" $ lookup lang languages

purchaseReader :: E (Integer -> Integer -> String -> IO ())
purchaseReader memberNo readerNo stripeToken = do
  row <- $(queryTuple "SELECT price, title FROM reader WHERE reader_no = {readerNo}") ?db
  case row of
    Nothing -> error "Reader not found."
    Just (price, title') -> do
      res <- runStripeT (defaultConfig $ APIKey stripeAPIKey) $ do
        chargeTokenById (TokenId stripeToken) (Amount price) (Currency "usd") (Just $ Description $ title' ++ " eBook")
      case res of
        Left _ -> error "There was an error charging your card."
        Right chg -> do
          $(execute "INSERT INTO member_stripe_charge (member_no, charge_id) \
                    \VALUES ({memberNo}, {unChargeId $ chargeId chg})") ?db
          $(execute "INSERT INTO member_reader (member_no, reader_no, page_no) \
                    \VALUES ({memberNo}, {readerNo}, 2)") ?db
