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

module Vocabulink.Reader ( readerPage ) where

import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, id, span)

readerPage :: E (String -> String -> Int -> IO (Maybe Html))
readerPage lang name' page = do
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
      when (isJust ?member) $ do
        let memberNo = memberNumber $ fromJust ?member
        liftIO $ $(execute "UPDATE member_reader SET page_no = {page} \
                           \WHERE member_no = {memberNo} \
                             \AND reader_no = (SELECT reader_no FROM reader \
                                              \WHERE short_name = {name'} AND lang = {lang})") ?db
      Just `liftM` (stdPage (title' ++ " - Page " ++ show page ++ " - A Vocabulink " ++ languageName lang ++ " Reader") [CSS "reader", JS "reader", JS "link"] mempty $ do
        div ! id "book" $ do
          when (page > 1) $ a ! class_ "pager prev sprite sprite-icon-arrow-left" ! title "Previous Page" ! href (toValue (page > 1 ? show (page - 1) $ ".")) $ mempty
          let nextRef = show (page + 1)
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
