\section{Articles}
\label{Article}

All of Vocabulink's @www@ subdomain is served by this program. As such, if we
want to publish static data there, we need some outlet for it.

It turns out that using our program gives us some additional consistency
(standard header and footer) and abstraction.

> module Vocabulink.Article (articlePage, articlesPage, refreshArticles) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import Control.Monad (filterM)
> import Data.Time.Format (parseTime)
> import System.Directory (  getDirectoryContents, getPermissions, doesFileExist,
>                            readable)
> import System.FilePath (takeExtension, replaceExtension, takeBaseName)
> import System.Locale (defaultTimeLocale)
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec.Perm ((<$$>), (<||>), (<|?>), permute)

All articles have some common metadata.

The article file path is not strictly necessary to store, but it does allow an
article to have a different representation on the filesystem than some
translation of its title. This avoids potential problems with automatic
title-to-path translation such as characters that the filesystem doesn't like
or articles with very long titles. The filepath however is just the relative
filename in the articles directory.

> data Article = Article {  articleFilename     :: FilePath,
>                           articleAuthor       :: Maybe String,
>                           articlePublishTime  :: UTCTime,
>                           articleUpdateTime   :: Maybe UTCTime,
>                           articleTitle        :: String }

Articles are actually used for articles, blog posts, and other static pages of
the site (that allow comments). The idea is that the ``blog'' section of the
site will chronologically list articles and allow interaction in a way similar
to most blogs. But there could be an ``articles'' section of the site that
contains a subset of the articles so that no time-sensitive articles are
displayed. Or an article could be used for a static page such as a
``privacy policy''.

For now, article storage is very simple. That may need to change in the future
for efficiency and to allow people without access to the article repository to
contribute.

\subsection{Retrieving Articles}

Creating articles is done outside of this program. They are currently generated
from Muse-mode (\url{http://mwolson.org/projects/EmacsMuse.html}) files using
Emacs. However, we can reconstruct the metadata for an article by parsing the
file.

Keeping the body of the article outside of the database gives us some of the
advantages of the filesystem such as revision control.

> articleDir :: App String
> articleDir = fromJust <$> getOption "articledir"

We need a way of keeping the database consistent with the filesystem. This is
it.

> refreshArticles :: App CGIResult
> refreshArticles = do
>   articles <- publishedArticles
>   c <- asks appDB
>   insert  <- liftIO $ prepare c
>                "INSERT INTO article (author, publish_time, update_time, \
>                                     \title, filename) \
>                             \VALUES ((SELECT member_no FROM member \
>                                      \WHERE username = ?), ?, ?, ?, ?)"
>   liftIO $ withTransaction c (\_ ->
>     mapM_ (\a -> execute insert (rec a)) articles)
>   redirect "/articles"
>    where rec a = [  toSql $ articleAuthor       a,
>                     toSql $ articlePublishTime  a,
>                     toSql $ articleUpdateTime   a,
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
>   let fullPaths = map (dir ++) ls
>   paths <- liftIO $ (map takeBaseName) <$> filterM isPublished fullPaths
>   catMaybes <$> mapM articleFromFile paths

We consider an article (file) published if it:

\begin{enumerate}
\item ends with @.muse@
\item is readable
\item the corresponding @.html@ file is readable
\end{enumerate}

> isPublished :: FilePath -> IO Bool
> isPublished f = do
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
>   muse  <- liftIO $ readFile $ dir ++ path ++ ".muse"
>   case P.parse articleHeader "" muse of
>     Left e     -> logApp "parse error" (show e) >> return Nothing
>     Right hdr  -> return $ Just $ hdr {  articleFilename  = path }

Each article is expected to have a particular structure. The structure is based
off of a required subset the Muse-mode directives.

An accepted article's first lines will consist of something like:

\begin{verbatim}
 #title Why Learn with Vocabulink?
 #author jekor
 #date 2009-01-11 16:04 -0800
 #update 2009-02-28 14:55 -0800
\end{verbatim}

\begin{description}
\item[@#title@] is a freeform title.
\item[@#author@] must be a username from the members table.
\item[@#date@] is the date in ISO-8601 format (with spaces between the date,
               time, and timezone).
\item[@#update@] is an optional update time (in the same format as @#date@).
\end{description}

We're going to go ahead and use fromJust here because we don't care about how
we're notified of errors. Publishing articles is not (yet) member-facing.

> articleHeader :: P.Parser Article
> articleHeader = permute (mkArticle  <$$>  (museDirective "title")
>                                     <|?>  (Nothing, museDir "author" >> authorP)
>                                     <||>  (museDir "date" >> dateTimeP)
>                                     <|?>  (Nothing, museDir "update" >> dateTimeP))
>     where mkArticle title author date update =
>             Article {  articleFilename     = undefined,
>                        articleAuthor       = author,
>                        articlePublishTime  = fromJust date,
>                        articleUpdateTime   = update,
>                        articleTitle        = title }

A muse directive looks sort of like a C preprocessor directive.

> museDirective :: String -> P.Parser String
> museDirective dir = museDir dir >> P.manyTill P.anyChar P.newline

> museDir :: String -> P.Parser ()
> museDir dir = P.try (P.string ("#" ++ dir)) >> P.spaces

> authorP :: P.Parser (Maybe String)
> authorP = Just <$> P.manyTill P.anyChar P.newline

> dateTimeP :: P.Parser (Maybe UTCTime)
> dateTimeP =  parseTime defaultTimeLocale "%F %T %z" <$>
>              P.manyTill P.anyChar P.newline

We don't need the article body until we go to actually display the article, so
there's no point in storing it in the article record.

> articleBody :: Article -> App Html
> articleBody article = do
>   path <- (++ (articleFilename article) ++ ".html") <$> articleDir
>   liftIO $ primHtml <$> readFile path

\subsection{Retrieving Articles}

Retrieve an article by its filename (path, whatever you want to call it).

> getArticle :: String -> App (Maybe Article)
> getArticle filename = do
>   (>>= articleFromTuple) <$> queryTuple' "SELECT filename, author, publish_time, \
>                                    \update_time, title \
>                             \FROM article WHERE filename = ?" [toSql filename]

> articleFromTuple :: [SqlValue] -> Maybe Article
> articleFromTuple [f,a,p,u,t]  = Just $
>   Article {  articleFilename     = fromSql f,
>              articleAuthor       = fromSql a,
>              articlePublishTime  = fromSql p,
>              articleUpdateTime   = fromSql u,
>              articleTitle        = fromSql t }
> articleFromTuple _            = Nothing

Here's how we get official articles.

> getArticles :: App (Maybe [Article])
> getArticles = do
>   rs <- queryTuples' "SELECT filename, author, publish_time, \
>                             \update_time, title \
>                      \FROM article" []
>   case rs of
>     Nothing   -> return Nothing
>     Just rs'  -> return $ Just $ catMaybes $ map articleFromTuple rs'

\subsection{Article Pages}

Display an article to the client. We don't care whether or not it's published
(past its publication date) as unpublished articles presumably don't have links
to them.

> articlePage :: String -> App CGIResult
> articlePage path = do
>   article <- getArticle path
>   case article of
>     Nothing  -> output404 ["article", path]
>     Just a   -> do
>       body <- articleBody a
>       stdPage (articleTitle a) [CSS "article"] []
>         [thediv ! [theclass "article"] << body]

Display a listing of published articles to the client.

> articlesPage :: App CGIResult
> articlesPage = do
>   articles <- getArticles
>   memberName <- asks appMemberName
>   case articles of
>     Nothing  -> error "Error retrieving articles"
>     Just as  -> simplePage "Articles" []
>       [unordList $ map articleLinkHtml as, refresh memberName]
>  where refresh member = case member of
>                           Just "jekor"  ->
>                             form ! [action "/articles", method "POST"] <<
>                               submit "" "Refresh from filesystem."
>                           _               -> noHtml

Create a clickable link HTML fragment for an article.

> articleLinkHtml :: Article -> Html
> articleLinkHtml a = anchor ! [href ("/article/" ++ articleFilename a)] <<
>   articleTitle a