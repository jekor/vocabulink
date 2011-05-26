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

module Vocabulink.Article.Html ( articlePage, articlesPage
                               , articleLinkHtml
                               ) where

import Vocabulink.App
import Vocabulink.Article
import Vocabulink.CGI
import Vocabulink.Comment
import Vocabulink.Html
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils hiding (readFile)

import Data.ByteString (readFile)
import Text.Blaze (unsafeByteString)

import Prelude hiding (div, span, id, readFile)

-- We don't need the article body until we go to actually display the article,
-- so there's no point in storing it in the article record.

articleBody :: Article -> App Html
articleBody article = do
  path <- (</> articleFilename article <.> "html") <$> (</> "articles") <$> asks appDir
  liftIO $ unsafeByteString <$> readFile path

-- Article Pages

-- Here's how to display an article to the client. We don't check here to see
-- whether or not it's published (past its publication date) as unpublished
-- articles presumably don't have links to them.

articlePage :: String -> App CGIResult
articlePage path = do
  article <- getArticle path
  case article of
    Nothing -> outputNotFound
    Just a' -> do
      row <- $(queryTuple' "SELECT root_comment \
                           \FROM article_comment \
                           \WHERE filename = {path}")
      comments <- case row of
                    Just root -> renderComments root
                    Nothing   -> return mempty
      body <- articleBody a'
      stdPage (articleTitle a') [CSS "article"] mempty $ do
        div ! class_ "article" $ body
        div ! id "comments" $ do
          h3 "Comments"
          comments

-- This page is for displaying a listing of published articles.

articlesPage :: App CGIResult
articlesPage = do
  articles <- getArticles
  memberNo <- memberNumber <$$> asks appMember
  let refresh = case memberNo of
                  Just 1 -> form ! action "/articles" ! method "post" $ do
                              input ! type_ "submit" ! name "" ! value "Refresh from filesystem."
                  _      -> mempty
  simplePage "Articles" [CSS "article"] $ do
    div ! class_ "article" $ do
      unordList $ map articleLinkHtml articles
      refresh

articleLinkHtml :: Article -> Html
articleLinkHtml article =
  a ! href (stringValue $ "/article/" ++ articleFilename article) $ string $ articleTitle article
