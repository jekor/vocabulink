\section{Articles}
\label{Article}

> module Vocabulink.Article (articlePage, articlesPage) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Html (stdPage)

> import Control.Monad (filterM)
> import Data.Maybe (maybe, catMaybes)
> import System.Directory (getDirectoryContents, getPermissions, doesFileExist, readable)
> import System.FilePath (takeExtension, replaceExtension, takeBaseName)
> import System.IO.Error (try)
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec.Perm ((<$$>), (<||>), permute)
> import Text.XHtml.Strict hiding (title, body)

> data Article = Article { title'  :: String
>                        , author' :: String
>                        , date'   :: String
>                        , path'   :: FilePath
>                        , url'    :: String
>                        , body'   :: Html }
>                deriving Show

> articlePath :: String
> articlePath = "/home/chris/project/vocabulink/articles/"

Publish an article from the filesystem. The article should be an
already-prepared html fragment.

> articlePage :: String -> App CGIResult
> articlePage title = do
>   article <- liftIO $ getArticle (articlePath ++ title ++ ".muse")
>   maybe (output404 ["article", title])
>     (\a -> stdPage (title' a) [] [(body' a)]) article

> getArticles :: FilePath -> IO [Article]
> getArticles path = do
>   ls <- try $ getDirectoryContents path
>   case ls of
>     Left _    -> return []
>     Right ls' -> do let fullPaths = map (path ++) ls'
>                     paths' <- filterM isPublished fullPaths
>                     articles <- mapM getArticle paths'
>                     return $ catMaybes articles

In order for an article to show up in the listing, it needs to be readable and
a readable HTML version must also exist.

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

> articleHeader :: P.Parser Article
> articleHeader = permute (mkArticle <$$> (museDirective "title")
>                                    <||> (museDirective "author")
>                                    <||> (museDirective "date"))
>     where mkArticle title author date =
>             Article { title'  = title
>                     , author' = author
>                     , date'   = date
>                     , path'   = ""
>                     , url'    = ""
>                     , body'   = noHtml }

> museDirective :: String -> P.Parser String
> museDirective dir = P.try (P.string ("#" ++ dir)) >> P.spaces >>
>                     P.manyTill P.anyChar P.newline

> getArticle :: FilePath -> IO (Maybe Article)
> getArticle path = do
>   muse <- readFile path
>   body <- readFile $ replaceExtension path ".html"
>   case P.parse articleHeader "" muse of
>     Left _    -> return Nothing
>     Right hdr -> return $ Just $ hdr { path' = path
>                                      , url'  = takeBaseName path
>                                      , body' = primHtml body }

> articlesPage :: App CGIResult
> articlesPage = do
>   articles <- liftIO $ getArticles articlePath
>   stdPage "Articles" [] $ [unordList $ map showArticle articles]

> showArticle :: Article -> Html
> showArticle a = anchor ! [href ("/article/" ++ url' a)] << title' a