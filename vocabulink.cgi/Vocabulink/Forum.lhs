\section{Forum}

Vocabulink uses a custom forums implementation. It does so for a couple reasons:

\begin{enumerate}

\item We want maximum integration and control as the forums are an integral
part of the site, not just an afterthought (they are here to direct the
evolution of the site).

\item Much of the forum logic is based on comments, which is common to the
forums, articles, and most importantly, links. Once we have comment threads,
the forums are almost free.

\item We reduce a source of security problems, and potentially maintenance
problems as well.

\end{enumerate}

> module Vocabulink.Forum (forumsPage, createForum) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import qualified Data.ByteString.Lazy as BS
> import qualified Text.XHtml.Strict.Formlets as F

The Forums page is a high-level look into Vocabulink's forums. Each forum is
part of a group of like forums. We have a placeholder for an eventual search
function which will search through the text of all comments in all forums.
Also, we include an administrative interface for creating new groups.

> forumsPage :: App CGIResult
> forumsPage = do
>   res <- runForm ("Forum Group Name" `formLabel` F.input Nothing) "Create"
>   memberName <- asks appMemberName
>   case (res, memberName) of
>     (Right s, Just "jekor")  -> createForumGroup s
>     (Left xhtml, _)          -> do
>       groups   <- queryAttribute' "SELECT group_name FROM forum_group \
>                                   \ORDER BY position ASC" []
>       groups'  <- case groups of
>                     Just gs  -> do
>                       gs' <- mapM (renderForumGroup . fromSql) gs
>                       return $ concatHtml gs'
>                     Nothing  -> return $ stringToHtml "Error retrieving forums."
>       simplePage "Forums" [CSS "forum", JS "MochiKit", JS "forum", JS "form"]
>         [  form ! [action "/forum/threads", method "POST"] <<
>              [  textfield "containing", submit "" "Search" ],
>            groups',
>            if memberName == Just "jekor"
>              then button ! [theclass "reveal forum-group-creator"] <<
>                     "New Forum Group" +++
>                   thediv ! [  identifier "forum-group-creator",
>                               theclass "forum-group",
>                               thestyle "display: none" ] <<
>                     xhtml
>              else noHtml ]
>     _                        -> outputError 403 "Access Denied" []

> renderForumGroup :: String -> App Html
> renderForumGroup g = do
>   memberName <- asks appMemberName
>   forums <- queryTuples'  "SELECT title, icon_filename FROM forum \
>                           \WHERE group_name = ? ORDER BY position ASC"
>                           [toSql g]
>   case forums of
>     Nothing  -> return $ paragraph << "Error retrieving forums"
>     Just fs  -> do
>       let fs' = map renderForum fs
>           creator = case memberName of
>                       Just "jekor"  -> [button ! [theclass "reveal forum-creator"] <<
>                                           "New Forum" +++ forumCreator]
>                       _             -> []
>           (left, right) = foldr (\a ~(x,y) -> (a:y,x)) ([],[]) (fs' ++ creator)
>       return $ thediv ! [theclass "forum-group"] <<
>         [  h2 << g,
>            unordList left ! [theclass "first"],
>            unordList right,
>            paragraph ! [theclass "clear"] << noHtml ]
>    where forumCreator =
>            form ! [  identifier "forum-creator",
>                      thestyle "display: none",
>                      action "/forum/new",
>                      method "POST",
>                      enctype "multipart/form-data" ] <<
>              fieldset <<
>                [  legend << "New Forum",
>                   hidden "forum-group" g,
>                   table <<
>                     [  tabularInput "Title" $ textfield "forum-title",
>                        tabularInput "Icon (64x64)" $ afile "forum-icon",
>                        tabularSubmit "Create" ] ]

> renderForum :: [SqlValue] -> Html
> renderForum [t, i]  =
>   anchor ! [href $ "/forum/" ++ urlify (fromSql t)] << [
>     image ! [  width "64", height "64",
>                src ("http://s.vocabulink.com/icons/" ++ fromSql i)],
>     stringToHtml $ fromSql t ]
> renderForum _       = stringToHtml "Error retrieving forum."

Create a forum group. For now, we're not concerned with error handling and
such. Maybe later when bringing on volunteer moderators or someone else I'll be
motivated to make this nicer.

> createForumGroup :: String -> App CGIResult
> createForumGroup s = do
>   c <- asks appDB
>   liftIO $ quickStmt c  "INSERT INTO forum_group (group_name, position) \
>                         \VALUES (?, COALESCE((SELECT MAX(position) \
>                                              \FROM forum_group), 0) + 1)"
>                         [toSql s]
>   redirect =<< referrerOrVocabulink

For now, we just upload to the icons directory in our static directory. Once we
have need for more file uploads we'll generalize this if necessary.

> createForum :: App CGIResult
> createForum = do
>   icons     <- iconDir
>   filename  <- getInputFilename "forum-icon"
>   group     <- getInput "forum-group"
>   title     <- getInput "forum-title"
>   case (filename, group, title) of
>     (Just f, Just g, Just t) -> do
>       icon <- fromJust <$> getInputFPS "forum-icon"
>       let iconfile = icons ++ "/" ++ basename f
>       liftIO $ BS.writeFile iconfile icon
>       c <- asks appDB
>       liftIO $ quickStmt c  "INSERT INTO forum (group_name, title, \
>                                                \position, icon_filename) \
>                             \VALUES (?, ?, \
>                                     \COALESCE((SELECT MAX(position) \
>                                               \FROM forum \
>                                               \WHERE group_name = ?), 0) + 1, ?)"
>                             [toSql g, toSql t, toSql g, toSql $ basename f]
>       redirect =<< referrerOrVocabulink
>     _ -> error "Please fill in the form completely. \
>                \Make sure to include an icon."

> iconDir :: App String
> iconDir = (++ "/icons") . fromJust <$> getOption "staticDir"
