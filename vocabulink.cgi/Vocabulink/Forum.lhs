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

> module Vocabulink.Forum (  forumsPage, forumPage, createForum,
>                            newTopicPage, forumTopicPage, replyToComment ) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import qualified Data.ByteString.Lazy as BS
> import Network.Gravatar (gravatarWith, size)
> import qualified Text.XHtml.Strict.Formlets as F

The Forums page is a high-level look into Vocabulink's forums. Each forum is
part of a group of like forums. We have a placeholder for an eventual search
function which will search through the text of all comments in all forums.
Also, we include an administrative interface for creating new groups.

> forumsPage :: App CGIResult
> forumsPage = do
>   res <- runForm ("Forum Group Name" `formLabel` F.input Nothing) $ Left "Create"
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
>       simplePage "Forums" forumDeps
>         [  -- Forum search disabled until it works.
>            --form ! [action "/forum/threads", method "POST"] <<
>            --  [  textfield "containing", submit "" "Search" ],
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

> forumDeps :: [Dependency]
> forumDeps = [CSS "forum", JS "MochiKit", JS "forum", JS "form"]

> renderForumGroup :: String -> App Html
> renderForumGroup g = do
>   memberName <- asks appMemberName
>   forums <- queryTuples'  "SELECT name, title, icon_filename FROM forum \
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
> renderForum [n, t, i]  =
>   anchor ! [href $ "/forum/" ++ (fromSql n)] << [
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
>       liftIO $ quickStmt c  "INSERT INTO forum (name, title, group_name, \
>                                                \position, icon_filename) \
>                             \VALUES (?, ?, ?, \
>                                     \COALESCE((SELECT MAX(position) \
>                                               \FROM forum \
>                                               \WHERE group_name = ?), 0) + 1, ?)"
>                             [  toSql $ urlify t, toSql t, toSql g,
>                                toSql g, toSql $ basename f]
>       redirect =<< referrerOrVocabulink
>     _ -> error "Please fill in the form completely. \
>                \Make sure to include an icon."

> iconDir :: App String
> iconDir = (++ "/icons") . fromJust <$> getOption "staticDir"

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

> newTopicPage :: String -> App CGIResult
> newTopicPage n = withRequiredMemberName $ \memberName -> do
>   email <- asks appMemberEmail
>   res <- runForm (forumTopicForm memberName email) $ Right noHtml
>   case res of
>     Left xhtml -> simplePage "New Forum Topic" forumDeps
>       [thediv ! [theclass "comments"] << xhtml]
>     Right (title, (body, _)) -> do
>       r <- createTopic (title, body) n
>       case r of
>         Nothing  -> simplePage "Error Creating Forum Topic" forumDeps []
>         Just _   -> redirect $ "../" ++ n

> forumTopicForm :: String -> Maybe String -> AppForm (String, (String, Maybe Integer))
> forumTopicForm memberName email =
>   commentBox ((,)  <$> plug (thediv <<) ("Topic Title" `formLabel`
>                           (  plug (\xhtml -> thediv ! [theclass "title"] << xhtml) $
>                              F.input Nothing) `check` ensures
>     [  ((> 0)     . length, "Title must not be empty."),
>        ((<=  80)  . length, "Title must be 80 characters or shorter.") ])
>                    <*> commentForm memberName email Nothing "Create")

> createTopic :: (String, String) -> String -> App (Maybe Integer)
> createTopic (t, c) fn = do
>   memberNo' <- asks appMemberNo
>   case memberNo' of
>     Nothing        -> return Nothing
>     Just memberNo  -> withTransaction' $ do
>       c' <- asks appDB
>       commentNo <- storeComment memberNo c Nothing
>       case commentNo of
>         Nothing  -> liftIO $ rollback c' >> throwDyn ()
>         Just n   -> do
>           r <- quickStmt'  "INSERT INTO forum_topic \
>                            \(forum_name, title, root_comment, last_comment) \
>                            \VALUES (?, ?, ?, ?)"
>                            [toSql fn, toSql t, toSql n, toSql n]
>           case r of
>             Nothing  -> liftIO $ rollback c' >> throwDyn ()
>             Just _   -> return n

> commentBox :: Monad m => XHtmlForm m a -> XHtmlForm m a
> commentBox = plug (\xhtml -> thediv ! [theclass "comment toplevel editable"] << xhtml)

> commentForm :: String -> Maybe String -> Maybe String ->
>                String -> AppForm (String, Maybe Integer)
> commentForm memberName email parent submitText = plug (\xhtml -> concatHtml [
>   anchor ! [href "#"] << image ! [  width "50", height "50",
>                                     src $ gravatarWith (fromMaybe "" email)
>                                                        Nothing (size 50) (Just "wavatar") ],
>   thediv ! [theclass "speech"] << xhtml,
>   thediv ! [theclass "signature"] << [
>     anchor ! [href "#"] << ((encodeString "—") ++ memberName),
>     stringToHtml " ",
>     submit "" submitText ] ])
>     ((\a b -> (a, maybeRead =<< b))  <$> (F.textarea Nothing `check` ensures
>                 [  ((> 0)       . length, "Comment must not be empty."),
>                    ((<= 10000)  . length, "Comment must be 10,000 characters or shorter.") ])
>           <*> (nothingIfNull $ F.hidden parent))

Return a list of forum topics as HTML rows. This saves us some effort by
avoiding any intermediate representation which we don't yet need.

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

> formatSimpleTime :: UTCTime -> String
> formatSimpleTime = formatTime' "%a %b %d, %Y %R"

> forumTopicPage :: String -> Integer -> App CGIResult
> forumTopicPage fn i = do
>   r <- queryTuple'  "SELECT t.root_comment, t.title, f.title \
>                     \FROM forum_topic t, forum f \
>                     \WHERE f.name = t.forum_name \
>                       \AND t.topic_no = ?" [toSql i]
>   case r of
>     Just [root,title,fTitle] -> do
>       comments <- queryTuples'
>         "SELECT c.comment_no, t.level, m.username, m.email, \
>                \c.time, c.comment \
>         \FROM comment c, member m, \
>              \connectby('comment', 'comment_no', 'parent_no', ?, 0) \
>              \AS t(comment_no int, parent_no int, level int) \
>         \WHERE c.comment_no = t.comment_no \
>           \AND m.member_no = c.author" [root]
>       case comments of
>         Nothing  -> error "Error retrieving comments."
>         Just cs  -> do
>           commentsHtml <- mapM displayComment cs
>           stdPage (fromSql title) (forumDeps ++ [JS "forum-comment"]) [] [
>             breadcrumbs [
>               anchor ! [href "../../forums"] << "Forums",
>               anchor ! [href $ "../" ++ fn] << (fromSql fTitle :: String),
>               stringToHtml $ fromSql title ],
>             thediv ! [theclass "comments"] << commentsHtml ]
>     _          -> output404 ["forum",fn,show i]

> displayComment :: [SqlValue] -> App Html
> displayComment [n, l, u, e, t, c]  = do
>   memberName <- asks appMemberName
>   email <- asks appMemberEmail
>   let n'  :: Integer  = fromSql n
>       l'  :: Integer  = fromSql l
>       u'  :: String   = fromSql u
>       e'  :: String   = e == SqlNull ? "" $ fromSql e
>       t'  :: UTCTime  = fromSql t
>       c'  :: String   = fromSql c
>       id'             = "reply-" ++ (show n')
>   replyBox <- case memberName of
>     Nothing  -> return noHtml
>     Just mn  -> do
>       let (_,markup,_) = runFormState [] "" $
>                              commentForm mn email (Just $ show n') "Send Reply"
>       xhtml <- markup
>       return $ thediv ! [  identifier id',
>                            theclass "reply",
>                            thestyle "display: none" ] <<
>         thediv ! [theclass "comment editable"] <<
>           form ! [method "POST"] << xhtml
>   return $ thediv ! [  theclass "comment toplevel",
>                        thestyle $ "margin-left:" ++ (show $ l'*2) ++ "em" ] << [
>     paragraph ! [theclass "timestamp"] << formatSimpleTime t',
>     anchor ! [href "#"] << image ! [  width "50", height "50",
>                                       src $ gravatarWith e' Nothing (size 50) (Just "wavatar") ],
>     thediv ! [theclass "speech"] << markdownToHtml c',
>     thediv ! [theclass "signature"] << [
>       anchor ! [href "#"] << ((encodeString "—") ++ u'),
>       stringToHtml " ",
>       button ! [theclass $ "reveal " ++ id'] << "Reply" ],
>     replyBox ]
> displayComment _             = return $ paragraph << "Error retrieving comment."

This returns the new comment number.

> storeComment :: Integer -> String -> Maybe Integer -> App (Maybe Integer)
> storeComment memberNo body parent = do
>   c <- asks appDB
>   liftIO $ insertNo c  "INSERT INTO comment (author, comment, parent_no) \
>                        \VALUES (?, ?, ?)" [toSql memberNo, toSql body, toSql parent]
>                        "comment_comment_no_seq"

> replyToComment :: App CGIResult
> replyToComment = do
>   memberName <- asks appMemberName
>   email <- asks appMemberEmail
>   case memberName of
>     Nothing  -> outputUnauthorized
>     Just mn  -> do
>       parent <- getInput "parent"
>       res <- runForm (commentForm mn email parent "Send Reply") $ Right noHtml
>       case res of
>         Left xhtml           -> outputJSON [  ("html", showHtmlFragment $ thediv ! [theclass "comment editable"] << xhtml),
>                                               ("status", "incomplete") ]
>         Right (body,parent')  -> do
>           memberNo <- fromJust <$> asks appMemberNo
>           commentNo <- storeComment memberNo body parent'
>           res' <- queryTuple'  "SELECT c.comment_no, 0 as level, \
>                                       \m.username, m.email, \
>                                       \c.time, c.comment \
>                                \FROM comment c, member m \
>                                \WHERE m.member_no = c.author \
>                                  \AND comment_no = ?"
>                                [toSql commentNo]
>           case res' of
>             Nothing  -> outputJSON [  ("html", "Error posting comment."),
>                                       ("status", "error") ]
>             Just c   -> do
>               c' <- asks appDB
>               liftIO $ commit c'
>               comment <- displayComment c
>               outputJSON [  ("html", showHtmlFragment comment),
>                             ("status", "accepted") ]
