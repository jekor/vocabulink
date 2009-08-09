% Copyright 2008, 2009 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\section{Articles}
\label{Article}

All of Vocabulink's @www@ subdomain is served by this program. As such, if we
want to publish static data there, we need some outlet for it. The only form of
static page we currently display is an article.

> module Vocabulink.Article (  articlePage, articlesPage, refreshArticles,
>                              getArticle, articleBody, getArticles,
>                              articleLinkHtml ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Comment
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils hiding ((<$$>))

> import Control.Monad (filterM)
> import Data.Time.Format (parseTime)
> import System.Directory (  getDirectoryContents, getPermissions, doesFileExist,
>                            readable)
> import qualified System.IO.UTF8 as IO.UTF8
> import System.Locale (defaultTimeLocale)
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec.Perm ((<$$>), (<||>), (<|?>), permute)

All articles have some common metadata.

The article file path is not strictly necessary to store, but it does allow an
article to have a different representation on the filesystem than some
translation of its title. This avoids potential problems with automatic
title-to-path translation, such as characters that the filesystem doesn't like
or articles with very long titles. The filepath however is just the relative
filename in the articles directory, not an absolute path.

> data Article = Article {  articleFilename     :: FilePath,
>                           articleAuthor       :: Maybe String,
>                           articlePublishTime  :: UTCTime,
>                           articleUpdateTime   :: Maybe UTCTime,
>                           articleSection      :: Maybe String,
>                           articleTitle        :: String }

For now, article storage is very simple. That may need to change in the future
for efficiency and to allow people without access to the article repository to
contribute.

\subsection{Retrieving Articles}

Creating articles is done outside of this program. They are currently generated
from Muse-mode files (\url{http://mwolson.org/projects/EmacsMuse.html}) using
Emacs. However, we can reconstruct the metadata for an article by parsing the
file ourselves.

Keeping the body of the article outside of the database gives us some of the
advantages of the filesystem such as revision control.

> articleDir :: App String
> articleDir = fromJust <$> getOption "articledir"

We need a way of keeping the database consistent with the filesystem. This is
it. For a logged in administrator, the articles page displays a ``Refresh from
Filesystem'' button that POSTs here.

> refreshArticles :: App CGIResult
> refreshArticles = do
>   articles <- publishedArticles
>   c <- asks appDB
>   insert  <- liftIO $ prepare c
>                "INSERT INTO article (author, publish_time, update_time, \
>                                     \section, title, filename) \
>                             \VALUES ((SELECT member_no FROM member \
>                                      \WHERE username = ?), ?, ?, ?, ?, ?)"
>   liftIO $ withTransaction c (\_ ->
>     mapM_ (execute insert . rec) articles)
>   redirect =<< referrerOrVocabulink
>    where rec a = [  toSql $ articleAuthor       a,
>                     toSql $ articlePublishTime  a,
>                     toSql $ articleUpdateTime   a,
>                     toSql $ articleSection      a,
>                     toSql $ articleTitle        a,
>                     toSql $ articleFilename     a ]

Articles are created and updated via the filesystem, but we need to have the
metadata on articles available in the database before we can display them via
the web. One option is to actually submit articles through the web. But I
consider that to be needlessly complex for now. Instead, we can just look over
a directory of articles, parse the header of each for metadata, and then update
the database.

We list the contents of the given directory (non-recursively) and figure out
which files are published articles. Finally, we call |getArticle| for each of
them. Then we return a list of Articles with undefined numbers and bodies.

> publishedArticles :: App [Article]
> publishedArticles = do
>   dir <- articleDir
>   ls <- liftIO $ getDirectoryContents dir
>   let fullPaths = map (\l -> dir </> l) ls
>   paths <- liftIO $ map takeBaseName <$> filterM isPublished fullPaths
>   catMaybes <$> mapM articleFromFile paths

We consider an article (file) published if it:

\begin{enumerate}
\item ends with @.muse@
\item is readable
\item has a corresponding readable @.html@ file
\end{enumerate}

> isPublished :: FilePath -> IO Bool
> isPublished f =
>   if takeExtension f == ".muse"
>      then do
>        r1 <- isReadable f
>        r2 <- isReadable $ replaceExtension f ".html"
>        return $ r1 && r2
>      else return False

> isReadable :: FilePath -> IO Bool
> isReadable f = do
>   exists' <- doesFileExist f
>   if exists'
>      then do
>        perms <- getPermissions f
>        return $ readable perms
>      else return False

To retrieve an article from the filesystem we just need the path to the @.muse@
file. We don't generate the article HTML (that's done by Emacs), so we read
that separately from a corresponding @.html@ file. Both files must exist for
this to succeed.

> articleFromFile :: String -> App (Maybe Article)
> articleFromFile path = do
>   dir   <- articleDir
>   muse  <- liftIO $ IO.UTF8.readFile $ dir </> path ++ ".muse"
>   case P.parse articleHeader "" muse of
>     Left e     -> logApp "parse error" (show e) >> return Nothing
>     Right hdr  -> return $ Just $ hdr {  articleFilename  = path }

Each article is expected to have a particular structure. The structure is based
off of a required subset of the Muse-mode directives.

An accepted article's first lines will consist of something like:

\begin{verbatim}
 #title Why Learn with Vocabulink?
 #author jekor
 #date 2009-01-11 16:04 -0800
 #update 2009-02-28 14:55 -0800
 #section main
\end{verbatim}

\begin{description}
\item[@#title@] is a freeform title.
\item[@#author@] must be a username from the members table.
\item[@#date@] is the date in ISO-8601 format (with spaces between the date,
               time, and timezone).
\item[@#update@] is an optional update time (in the same format as @#date@).
\item[@#section@] is the section of the site in to publish the article. For
                  static content ``articles'' such as the privacy policy, don't
                  include a section. For now, only ``main'' is supported with
                  our simple publishing system.
\end{description}

Parsing an article's metadata is pretty simple with Parsec's permutation
combinators.

We're going to go ahead and use fromJust here because we don't care about how
we're notified of errors (publishing articles is not (yet) member-facing).

> articleHeader :: P.Parser Article
> articleHeader = permute
>   (mkArticle  <$$>  museDirective "title"
>               <|?>  (Nothing, museDir "author" >> authorP)
>               <||>  (museDir "date" >> dateTimeP)
>               <|?>  (Nothing, museDir "update" >> dateTimeP)
>               <|?>  (Nothing, Just <$> museDirective "section"))
>     where mkArticle title author date update section =
>             Article {  articleFilename     = undefined,
>                        articleAuthor       = author,
>                        articlePublishTime  = fromJust date,
>                        articleUpdateTime   = update,
>                        articleSection      = section,
>                        articleTitle        = title }

A muse directive looks sort of like a C preprocessor directive.

> museDirective :: String -> P.Parser String
> museDirective dir = museDir dir >> P.manyTill P.anyChar P.newline

> museDir :: String -> P.Parser ()
> museDir dir = P.try (P.string ('#' : dir)) >> P.spaces

> authorP :: P.Parser (Maybe String)
> authorP = Just <$> P.manyTill P.anyChar P.newline

> dateTimeP :: P.Parser (Maybe UTCTime)
> dateTimeP =  parseTime defaultTimeLocale "%F %T %z" <$>
>              P.manyTill P.anyChar P.newline

We don't need the article body until we go to actually display the article, so
there's no point in storing it in the article record.

> articleBody :: Article -> App Html
> articleBody article = do
>   path <- (</> articleFilename article ++ ".html") <$> articleDir
>   liftIO $ primHtml <$> IO.UTF8.readFile path

\subsection{Retrieving Articles}

To retrieve an article with need its (relative) filename (or path, or whatever
you want to call it).

> getArticle :: String -> App (Maybe Article)
> getArticle filename =
>   (>>= articleFromTuple) <$>
>     queryTuple' "SELECT filename, author, publish_time, \
>                        \section, update_time, title \
>                 \FROM article WHERE filename = ?" [toSql filename]

As with links, we use a helper function to convert a raw SQL tuple.

> articleFromTuple :: [SqlValue] -> Maybe Article
> articleFromTuple [f,a,p,u,s,t]  = Just
>   Article {  articleFilename     = fromSql f,
>              articleAuthor       = fromSql a,
>              articlePublishTime  = fromSql p,
>              articleUpdateTime   = fromSql u,
>              articleSection      = fromSql s,
>              articleTitle        = fromSql t }
> articleFromTuple _            = Nothing

To get a list of articles for display on the main articles page, we look for
published articles in the database that are in the ``main'' section.

> getArticles :: App (Maybe [Article])
> getArticles = do
>   rs <- queryTuples' "SELECT filename, author, publish_time, \
>                             \update_time, section, title \
>                      \FROM article \
>                      \WHERE publish_time < CURRENT_TIMESTAMP \
>                        \AND section = 'main' \
>                      \ORDER BY publish_time DESC" []
>   case rs of
>     Nothing   -> return Nothing
>     Just rs'  -> return $ Just $ mapMaybe articleFromTuple rs'

\subsection{Article Pages}

Here's how to display an article to the client. We don't check here to see
whether or not it's published (past its publication date) as unpublished
articles presumably don't have links to them.

> articlePage :: String -> App CGIResult
> articlePage path = do
>   article <- getArticle path
>   case article of
>     Nothing  -> output404 ["article", path]
>     Just a   -> do
>       r <- queryValue'  "SELECT root_comment \
>                         \FROM article_comments \
>                         \WHERE filename = ?" [toSql path]
>       comments <- case r of
>                     Just root  -> renderComments $ fromSql root
>                     Nothing    -> return noHtml
>       body <- articleBody a
>       stdPage (articleTitle a)
>         [CSS "article", JS "MochiKit", JS "form", JS "comment", CSS "comment"] []
>         [  thediv ! [theclass "article"] << body,
>            hr ! [theclass "clear"], h3 << "Comments", comments ]

This page is for displaying a listing of published articles.

> articlesPage :: App CGIResult
> articlesPage = do
>   articles <- getArticles
>   memberName <- asks appMemberName
>   case articles of
>     Nothing  -> error "Error retrieving articles"
>     Just as  -> simplePage "Articles" [CSS "article"]
>       [  thediv ! [theclass "article"] << [
>            unordList $ map articleLinkHtml as,
>            refresh memberName ] ]
>  where refresh member = case member of
>                           Just "jekor"  ->
>                             form ! [action "/articles", method "POST"] <<
>                               submit "" "Refresh from filesystem."
>                           _               -> noHtml

Create a clickable link HTML fragment for an article.

> articleLinkHtml :: Article -> Html
> articleLinkHtml a = anchor ! [href ("/article/" ++ articleFilename a)] <<
>   articleTitle a