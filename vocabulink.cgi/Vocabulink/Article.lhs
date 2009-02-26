\section{Articles}
\label{Article}

All of Vocabulink's @www@ subdomain is served by this program. As such, if we
want to publish static data there, we need some outlet for it.

It turns out that using our program gives us some additional consistency
(standard header and footer) and abstraction.

> module Vocabulink.Article (articlePage, articlesPage) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Html
> import Vocabulink.Utils

> import Control.Monad (filterM)
> import System.Directory (  getDirectoryContents, getPermissions, doesFileExist,
>                            readable)
> import System.FilePath (takeExtension, replaceExtension, takeBaseName)
> import System.IO.Error (try)
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec.Perm ((<$$>), (<||>), permute)

All articles have some common metadata.

> data Article = Article {  articleTitle   :: String,
>                           articleAuthor  :: String,
>                           articleDate    :: String,
>                           articlePath    :: FilePath,
>                           articleUrl     :: String,
>                           articleBody    :: Html }

\subsection{Retrieving Articles}

Creating articles is done outside of this program. They are currently generated
from Muse-mode (\url{http://mwolson.org/projects/EmacsMuse.html}) files using
Emacs. However, we can reconstruct the metadata for an article by parsing the
file.

For now, all articles live in my project directory.

> articleDir :: String
> articleDir = "/home/chris/project/vocabulink/articles/"

To retrieve an article from the filesystem we just need the path to the @.muse@
file. We don't generate the article HTML (that's done by Emacs), so we read
that separately from a corresponding @.html@ file. Both files must exist for
this to succeed.

> getArticle :: FilePath -> App (Maybe Article)
> getArticle path = do
>   muse  <- liftIO $ readFile path
>   body  <- liftIO $ readFile $ replaceExtension path ".html"
>   case P.parse articleHeader "" muse of
>     Left e     -> logApp "parse error" (show e) >> return Nothing
>     Right hdr  -> return $ Just $ hdr {  articlePath = path,
>                                          articleUrl  = takeBaseName path,
>                                          articleBody = primHtml body }

Each article is expected to have a particular structure. The structure is based
off of a required subset the Muse-mode directives.

An accepted article's first lines will consist of something like:

\begin{verbatim}
 #title Why Learn with Vocabulink?
 #author Chris Forno (jekor)
 #date 2009-01-11
\end{verbatim}

> articleHeader :: P.Parser Article
> articleHeader = permute (mkArticle  <$$>  (museDirective "title")
>                                     <||>  (museDirective "author")
>                                     <||>  (museDirective "date"))
>     where mkArticle title author date =
>             Article {  articleTitle  = title,
>                        articleAuthor = author,
>                        articleDate   = date,
>                        articlePath   = undefined,
>                        articleUrl    = undefined,
>                        articleBody   = undefined }

A muse directive looks sort of like a C preprocessor directive.

> museDirective :: String -> P.Parser String
> museDirective dir = P.try (P.string ("#" ++ dir)) >> P.spaces >>
>                     P.manyTill P.anyChar P.newline

Retrieving more than one article is slightly more complicated. Instead of
taking the path to a file, we take the path to a directory. We list the
contents of the directory (non-recursively) and figure out which files are
published articles. Finally, we call |getArticle| for each of them. This is
inefficient and should be changed to reading some listing at some point.

> getPublishedArticles :: FilePath -> App (Maybe [Article])
> getPublishedArticles path = do
>   ls <- liftIO $ try $ getDirectoryContents path
>   case ls of
>     Left e     -> logApp "IO exception" (show e) >> return Nothing
>     Right ls'  -> do  let fullPaths = map (path ++) ls'
>                       paths <- liftIO $ filterM isPublished fullPaths
>                       articles <- mapM getArticle paths
>                       return $ Just $ catMaybes articles

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

\subsection{Article Pages}

Display a published article to the client.

> articlePage :: String -> App CGIResult
> articlePage title = do
>   published <- liftIO $ isPublished path
>   case published of
>     False -> output404 ["article", title]
>     True -> do
>       article <- getArticle path
>       case article of
>         Nothing  -> output404 ["article", title]
>         Just a   -> stdPage (articleTitle a) [CSS "article"] []
>           [thediv ! [theclass "article"] << (articleBody a)]
>   where path = articleDir ++ title ++ ".muse"

Display a listing of published articles to the client.

> articlesPage :: App CGIResult
> articlesPage = do
>   articles <- getPublishedArticles articleDir
>   case articles of
>     Nothing  -> error "Error retrieving articles."
>     Just as  -> stdPage "Articles" [] [] [unordList $ map articleLinkHtml as]

Create a clickable link HTML fragment for an article.

> articleLinkHtml :: Article -> Html
> articleLinkHtml a = anchor ! [href ("/article/" ++ articleUrl a)] << articleTitle a