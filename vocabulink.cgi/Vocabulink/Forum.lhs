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

\section{Forum}

Vocabulink uses a custom forums implementation. I created it for a couple
reasons:

\begin{enumerate}

\item We want maximum integration and control as the forums are an integral
part of the site, not just an afterthought (they are here to direct the
evolution of the site).

\item Much of the forum logic is based on comments, which is common to the
forums, articles, and most importantly, links. Once we have comment threads,
the forums come almost for free. (Note that comments on articles and links are
currently supported but will be soon.)

\item We reduce a source of security problems, and potentially maintenance
problems as well. (Most software is implemented in PHP which is notorious for
security problems.)

\end{enumerate}

> module Vocabulink.Forum (  forumsPage, forumPage, createForum,
>                            newTopicPage, forumTopicPage, commentPreview ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Comment
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import qualified Data.ByteString.Lazy as BS
> import qualified Text.XHtml.Strict.Formlets as F

The Forums page is a high-level look into Vocabulink's forums. Each forum is
part of a group of similar forums. Also, we include an administrative interface
for creating new groups.

> forumsPage :: App CGIResult
> forumsPage = do
>   res <- runForm ("Forum Group Name" `formLabel` F.input Nothing) $ Left "Create"
>   memberName <- asks appMemberName
>   case (res, memberName) of
>     (Right s, Just "jekor")  -> createForumGroup s
>     (Left xhtml, _)          -> do
>       groups   <- queryAttribute'  "SELECT group_name FROM forum_group \
>                                    \ORDER BY position ASC" []
>       groups'  <- case groups of
>                     Just gs  -> do
>                       gs' <- mapM (renderForumGroup . fromSql) gs
>                       return $ concatHtml gs'
>                     Nothing  -> return $ stringToHtml "Error retrieving forums."
>       simplePage "Forums" forumDeps
>         [  groups',
>            if memberName == Just "jekor"
>              then button ! [theclass "reveal forum-group-creator"] <<
>                     "New Forum Group" +++
>                   thediv ! [  identifier "forum-group-creator",
>                               theclass "forum-group",
>                               thestyle "display: none" ] << xhtml
>              else noHtml ]
>     _                        -> outputError 403 "Access Denied" []

The dependencies for forum pages are all the same.

> forumDeps :: [Dependency]
> forumDeps = [CSS "forum", JS "MochiKit", JS "forum", JS "form"]

Displaying an individual group of forums is a little bit tougher than it would
seem (we have to also support the administrative interface for creating new
forums within the group).

> renderForumGroup :: String -> App Html
> renderForumGroup g = do
>   memberName <- asks appMemberName
>   forums <- queryTuples'
>     "SELECT t.name, t.title, t.icon_filename, c2.time, m.username FROM \
>      \(SELECT f.name, f.title, f.icon_filename, f.position, \
>              \MAX(c.comment_no) AS latest_comment \
>       \FROM forum f \
>       \LEFT JOIN forum_topic ON (forum_name = name) \
>       \LEFT JOIN comment c ON (comment_no = last_comment) \
>       \WHERE group_name = ? \
>       \GROUP BY f.name, f.title, f.icon_filename, f.position) AS t \
>     \LEFT JOIN comment c2 ON (c2.comment_no = latest_comment) \
>     \LEFT JOIN member m ON (m.member_no = c2.author) \
>     \ORDER BY t.position ASC"
>     [toSql g]
>   case forums of
>     Nothing  -> return $ paragraph << "Error retrieving forums"
>     Just fs  -> do
>       let fs' = map renderForum fs
>           creator = case memberName of
>                       Just "jekor"  -> [button ! [theclass ("reveal forum-creator-" ++ g)] <<
>                                           "New Forum" +++ forumCreator]
>                       _             -> []
>           (left, right) = every2nd $ fs' ++ creator
>       return $ thediv ! [theclass "forum-group"] <<
>         [  h2 << g,
>            unordList left ! [theclass "first"],
>            unordList right,
>            clear ]
>    where forumCreator =
>            form ! [  identifier ("forum-creator-" ++ g),
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

Displaying individual forums within the group is easier. Each forum requires an
icon. This allows faster visual navigation to the forum of interest when first
arriving at the top-level forums page.

> renderForum :: [SqlValue] -> Html
> renderForum [n, t, i, t', u]  =
>   let latest = case t' of
>                  SqlNull  -> noHtml
>                  _        -> paragraph ! [theclass "metadata"] <<
>                                [  stringToHtml $ formatSimpleTime $ fromSql t', br,
>                                   stringToHtml $ fromSql u ] in
>   anchor ! [href $ "/forum/" ++ (fromSql n)] << [
>     image ! [  width "64", height "64",
>                src ("http://s.vocabulink.com/icons/" ++ fromSql i)],
>     h3 << (fromSql t :: String),
>     latest, clear ]
> renderForum _       = stringToHtml "Error retrieving forum."

Creating a forum group is a rare administrative action. For now, we're not
concerned with error handling and such. Maybe later when bringing on volunteer
moderators or someone else I'll be motivated to make this nicer.

> createForumGroup :: String -> App CGIResult
> createForumGroup s = do
>   c <- asks appDB
>   liftIO $ quickStmt c  "INSERT INTO forum_group (group_name, position) \
>                         \VALUES (?, COALESCE((SELECT MAX(position) \
>                                              \FROM forum_group), 0) + 1)"
>                         [toSql s]
>   liftIO memcacheFlush
>   redirect =<< referrerOrVocabulink

For now, we just upload to the icons directory in our static directory.

> createForum :: App CGIResult
> createForum = do
>   icons     <- iconDir
>   filename  <- getInputFilename "forum-icon"
>   group     <- getInput "forum-group"
>   title     <- getInput "forum-title"
>   case (filename, group, title) of
>     (Just f, Just g, Just t) -> do
>       icon <- fromJust <$> getInputFPS "forum-icon"
>       let iconfile = icons </> basename f
>       liftIO $ BS.writeFile iconfile icon
>       c <- asks appDB
>       liftIO $ quickStmt c  "INSERT INTO forum (name, title, group_name, \
>                                                \position, icon_filename) \
>                             \VALUES (?, ?, ?, \
>                                     \COALESCE((SELECT MAX(position) \
>                                               \FROM forum \
>                                               \WHERE group_name = ?), 0) + 1, ?)"
>                             [  toSql $ urlify t, toSql t, toSql g,
>                                toSql g, toSql $ basename f]
>       liftIO memcacheFlush
>       redirect =<< referrerOrVocabulink
>     _ -> error "Please fill in the form completely. \
>                \Make sure to include an icon."

The directory for forum icons is located within the static directory (and hence
served from the static subdomain).

> iconDir :: App String
> iconDir = (++ "/icons") . fromJust <$> getOption "staticdir"

Each forum page is a table of topics. Each topic lists the title (a description
of the topic), how many replies the topic has received, who created the topic,
and the time of the latest topic.

> forumPage :: String -> App CGIResult
> forumPage forum = do
>   title <- queryValue'  "SELECT title FROM forum WHERE name = ?"
>                         [toSql forum]
>   case title of
>     Nothing  -> output404 ["forum", forum]
>     Just t   -> do
>       let tc = concatHtml $ [
>                  td ! [theclass "topic"] <<
>                    form ! [action (forum ++ "/new"), method "GET"]
>                      << submit "" "New Topic",
>                  td << noHtml, td << noHtml, td << noHtml ]
>       topics <- forumTopicRows forum
>       stdPage ("Forum - " ++ forum) forumDeps []
>         [  breadcrumbs [  anchor ! [href "../forums"] << "Forums",
>                           stringToHtml $ fromSql t ],
>            thediv ! [identifier "topics"] << table << [
>              thead << tr << [
>                th ! [theclass "topic"]    << "Topic",
>                th ! [theclass "replies"]  << "Replies",
>                th ! [theclass "author"]   << "Author",
>                th ! [theclass "last"]     << "Last Comment" ],
>              tbody << tableRows (tc:topics),
>              tfoot << tr << noHtml ] ]

This returns a list of forum topics, sorted by creation date, as HTML rows. It
saves us some effort by avoiding any intermediate representation which we don't
yet need.

The structure of the comment relations make sorting by latest comment a little
bit more difficult and a task for later.

> forumTopicRows :: String -> App [Html]
> forumTopicRows t = do
>   topics <- queryTuples'
>     "SELECT t.topic_no, t.title, t.num_replies, \
>            \m1.username, c2.time, m2.username \
>     \FROM forum_topic t, comment c1, comment c2, member m1, member m2 \
>     \WHERE c1.comment_no = t.root_comment \
>       \AND c2.comment_no = t.last_comment \
>       \AND m1.member_no = c1.author \
>       \AND m2.member_no = c2.author \
>       \AND forum_name = ? \
>     \ORDER BY t.topic_no DESC" [toSql t]
>   case topics of
>     Nothing  -> return [td << "Error retrieving topics."]
>     Just ts  -> return $ map topicRow ts
>       where  topicRow :: [SqlValue] -> Html
>              topicRow [tn, tt, nr, ta, lt, la] = 
>                let tn'  :: Integer  = fromSql tn
>                    tt'  :: String   = fromSql tt
>                    nr'  :: Integer  = fromSql nr
>                    ta'  :: String   = fromSql ta
>                    lt'  :: UTCTime  = fromSql lt
>                    la'  :: String   = fromSql la in
>                  concatHtml [
>                  td ! [theclass "topic"] <<
>                    anchor ! [href (t ++ "/" ++ (show tn'))] << tt',
>                  td ! [theclass "replies"] << show nr',
>                  td ! [theclass "author"] << ta',
>                  td ! [theclass "last"] << [
>                    stringToHtml $ formatSimpleTime lt',
>                    br,
>                    stringToHtml la' ] ]
>              topicRow _ = td << "Error retrieving topic."

The page for creating a new topic is very plain. In fact, it doesn't even have
a way to preview the text of the first comment to the topic.

> newTopicPage :: String -> App CGIResult
> newTopicPage n = withRequiredMemberNumber $ \_ -> do
>   email <- asks appMemberEmail
>   memberName <- fromJust <$> asks appMemberName
>   res <- runForm (forumTopicForm memberName email) $ Right noHtml
>   case res of
>     Left xhtml -> simplePage "New Forum Topic" forumDeps
>       [thediv ! [theclass "comments"] << xhtml]
>     Right (title, (body, _)) -> do
>       r <- createTopic (title, body) n
>       case r of
>         Nothing  -> simplePage "Error Creating Forum Topic" forumDeps []
>         Just _   -> redirect $ "../" ++ n

The form for creating the topic is very simple. All we need is a title and the
body of the first comment (topics can't be created without a root comment).

> forumTopicForm ::  String -> Maybe String ->
>                    AppForm (String, (String, Maybe Integer))
> forumTopicForm memberName email =
>   commentBox ((,)  <$> plug (thediv <<) ("Topic Title" `formLabel`
>                           (  plug (\xhtml -> thediv ! [theclass "title"] << xhtml) $
>                              F.input Nothing) `check` ensures
>     [  ((> 0)     . length, "Title must not be empty."),
>        ((<=  80)  . length, "Title must be 80 characters or shorter.") ])
>                    <*> commentForm memberName email Nothing)

This creates the topic in the database given the title and root comment body.

> createTopic :: (String, String) -> String -> App (Maybe Integer)
> createTopic (t, c) fn = do
>   memberNo' <- asks appMemberNo
>   case memberNo' of
>     Nothing        -> return Nothing
>     Just memberNo  -> withTransaction' $ do
>       c' <- asks appDB
>       commentNo' <- storeComment memberNo c Nothing
>       case commentNo' of
>         Nothing  -> liftIO $ rollback c' >> throwDyn ()
>         Just n   -> do
>           r <- quickStmt'  "INSERT INTO forum_topic \
>                            \(forum_name, title, root_comment, last_comment) \
>                            \VALUES (?, ?, ?, ?)"
>                            [toSql fn, toSql t, toSql n, toSql n]
>           case r of
>             Nothing  -> liftIO $ rollback c' >> throwDyn ()
>             Just _   -> (liftIO memcacheFlush) >> return n

We're finally getting to the core of the forum: individual topic pages. The
forum topic page displays the entire comment thread. Eventually it will
probably need paging.

> forumTopicPage :: String -> Integer -> App CGIResult
> forumTopicPage fn i = do
>   r <- queryTuple'  "SELECT t.root_comment, t.title, f.title \
>                     \FROM forum_topic t, forum f \
>                     \WHERE f.name = t.forum_name \
>                       \AND t.topic_no = ?" [toSql i]
>   case r of
>     Just [root,title,fTitle] -> do
>       comments <- queryTuples' "SELECT * FROM comment_tree(?)" [root]
>       case comments of
>         Nothing  -> error "Error retrieving comments."
>         Just cs  -> do
>           commentsHtml <- mapM (displayComment i) cs
>           stdPage (fromSql title) (forumDeps ++ [JS "forum-comment"]) [] [
>             breadcrumbs [
>               anchor ! [href "../../forums"] << "Forums",
>               anchor ! [href $ "../" ++ fn] << (fromSql fTitle :: String),
>               stringToHtml $ fromSql title ],
>             thediv ! [theclass "comments"] << commentsHtml ]
>     _          -> output404 ["forum",fn,show i]
