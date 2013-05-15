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

module Vocabulink.Article (Article(..), articlePage, articlesPage) where

import Vocabulink.Comment
import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Page
import Vocabulink.Utils hiding ((<$$>), readFile)

import Data.ByteString (readFile)
import Text.Blaze (unsafeByteString)
import qualified Text.Blaze.Html5 as Html5

import Prelude hiding (div, span, id, readFile)

-- All articles have some common metadata.

-- The article file path is not strictly necessary to store, but it does allow
-- an article to have a different representation on the filesystem than some
-- translation of its title. This avoids potential problems with automatic
-- title-to-path translation, such as characters that the filesystem doesn't
-- like or articles with very long titles. The filepath however is just the
-- relative filename in the articles directory, not an absolute path.

data Article = Article { articleFilename    :: FilePath
                       , articleAuthor      :: Integer
                       , articlePublishTime :: UTCTime
                       , articleUpdateTime  :: UTCTime
                       , articleSection     :: Maybe String
                       , articleTitle       :: String
                       }

-- Retrieving Articles

-- To retrieve an article we need its (relative) filename (or path, or whatever
-- you want to call it).

getArticle :: E (String -> IO (Maybe Article))
getArticle filename = liftM articleFromTuple <$> $(queryTuple
  "SELECT filename, author, publish_time, \
         \update_time, section, title \
  \FROM article WHERE filename = {filename}") ?db

-- As with links, we use a helper function to convert a raw SQL tuple.

articleFromTuple :: (FilePath, Integer, UTCTime, UTCTime, Maybe String, String) -> Article
articleFromTuple (f, a', p', u, s, t) =
  Article { articleFilename     = f
          , articleAuthor       = a'
          , articlePublishTime  = p'
          , articleUpdateTime   = u
          , articleSection      = s
          , articleTitle        = t
          }

-- To get a list of articles for display on the main articles page, we look for
-- published articles in the database that are in the ``main'' section.

getArticles :: E (IO [Article])
getArticles = map articleFromTuple <$> $(queryTuples
  "SELECT filename, author, publish_time, \
         \update_time, section, title \
  \FROM article \
  \WHERE publish_time < CURRENT_TIMESTAMP \
    \AND section = 'main' \
  \ORDER BY publish_time DESC") ?db

-- We don't need the article body until we go to actually display the article,
-- so there's no point in storing it in the article record.

articleBody :: Article -> IO Html
articleBody article = do
  let path = mainDir </> "articles" </> articleFilename article <.> "html"
  unsafeByteString <$> readFile path

-- Article Pages

-- Here's how to display an article to the client. We don't check here to see
-- whether or not it's published (past its publication date) as unpublished
-- articles presumably don't have links to them.

articlePage :: E (String -> IO (Maybe Html))
articlePage = maybeM articleHtml <=< getArticle
 where articleHtml article' = do
         row <- $(queryTuple "SELECT root_comment \
                             \FROM article_comment \
                             \WHERE filename = {articleFilename article'}") ?db
         comments <- maybe (return mempty) renderComments row
         body <- articleBody article'
         return $ stdPage (articleTitle article') [CSS "article"] mempty $ do
           Html5.article body
           div ! id "comments" $ do
             h3 "Comments"
             comments

-- This page is for displaying a listing of published articles.

articlesPage :: E (IO Html)
articlesPage = do
  articles <- getArticles
  return $ simplePage "Articles" [CSS "article"] $ do
    div ! id "central-column" $ do
      unordList $ map articleLinkHtml articles

articleLinkHtml :: Article -> Html
articleLinkHtml article =
  a ! href (toValue $ "/article/" ++ articleFilename article) $ toMarkup $ articleTitle article
