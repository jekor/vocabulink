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

-- Readers introduce words in context.

module Vocabulink.Reader ( readerTitlePage, readerPage ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Config
import Vocabulink.Html
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, id, span)

readerTitlePage :: String -> String -> App CGIResult
readerTitlePage lang name' = do
  row <- $(queryTuple' "SELECT title, description FROM reader WHERE short_name = {name'} AND lang = {lang}")
  case row of
    Nothing -> outputNotFound
    Just (title', desc) -> do
      language <- fromMaybe "Unknown Language" <$> langName lang
      stdPage (title' ++ " - A Vocabulink " ++ language ++ " Reader") [CSS "reader", JS "reader", CSS "link", JS "link"] mempty $ do
        div ! id "book" $ do
          a ! class_ "pager next sprite sprite-icon-arrow-right" ! title "Next Page" ! href (toValue $ name' ++ "/1") $ mempty
          div ! class_ "page left" $ do
            h1 $ toHtml title'
            h2 ! style "text-align: center" $ toHtml $ "A Vocabulink " ++ language ++ " Reader"
            markdownToHtml desc
          div ! class_ "page right" $ do
            p $ "As you read the story, don't worry about translating or understanding everything perfectly. The main purpose of the story is to introduce you to new words gently and in context."
            p $ "Unlike some readers, there is no translation of the story included. However, you can click on any of the words in the story to see its definition along with any mnemonics that might be available to help you remember it."
            p $ "Each page of the story will introduce you to a handful of new words. We recommend learning all the new words on each page before proceeding to the next." -- Any words you click on will be automatically added to a list that we will review you on later.
            p $ "To begin, click the \"Next Page\" button to the right."
          div ! style "clear: both" $ mempty -- We can't use overflow: hidden here.

readerPage :: String -> String -> Int -> App CGIResult
readerPage lang name' page = do
  row <- $(queryTuple' "SELECT title, body \
                       \FROM reader_page \
                       \INNER JOIN reader USING (reader_no) \
                       \WHERE short_name = {name'} AND lang = {lang} \
                         \AND page_no = {page}")
  maxPage' <- $(queryTuple' "SELECT MAX(page_no) \
                            \FROM reader_page \
                            \INNER JOIN reader USING (reader_no) \
                            \WHERE short_name = {name'} AND lang = {lang}")
  case (row, maxPage') of
    (Just (title', body), Just (Just maxPage)) -> do
      language <- fromMaybe "Unknown Language" <$> langName lang
      stdPage (title' ++ " - Page " ++ show page ++ " - A Vocabulink " ++ language ++ " Reader") [CSS "reader", JS "reader", CSS "link", JS "link"] mempty $ do
        div ! id "book" $ do
          a ! class_ "pager prev sprite sprite-icon-arrow-left" ! title "Previous Page" ! href (toValue (page > 1 ? show (page - 1) $ ".")) $ mempty
          when (page < maxPage) $ a ! class_ "pager next sprite sprite-icon-arrow-right" ! title "Next Page" ! href (toValue $ show (page + 1)) $ mempty
          div ! class_ "page left" $ do
            div ! class_ "header" $ do
              span ! class_ "title" $ toHtml title'
              span ! class_ "page-number" $ toHtml (show page)
            markdownToHtml body
          div ! class_ "page right" $ do
            p $ "Click any of the words on the left page to see their definitions and any related mnemonics here."
          div ! style "clear: both" $ mempty -- We can't use overflow: hidden here.
    _ -> outputNotFound
