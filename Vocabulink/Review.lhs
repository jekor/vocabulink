> module Vocabulink.Review where

> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Member
> import Vocabulink.Utils

> import Database.HDBC
> import Network.CGI
> import Text.XHtml.Strict
> import System.Time

> scheduleReview :: IConnection conn => conn -> Integer -> Integer -> IO ()
> scheduleReview c memberNo linkNo = do
>   quickInsert c "INSERT INTO link_to_review (member_no, link_no) \
>                 \VALUES (?, ?)" [toSql memberNo, toSql linkNo]
>     `catchSqlE` "You already have this link scheduled for review or there was an error."

> newReview :: String -> CGI CGIResult
> newReview link = do
>   c <- liftIO db
>   memberNo <- loginNumber
>   no <- liftIO $ intFromString link
>   case no of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       liftIO $ scheduleReview c memberNo n
>       referer >>= redirect

Review the next link in the queue.

> reviewLink :: CGI CGIResult
> reviewLink = do
>   c <- liftIO db
>   memberNo <- loginNumber
>   linkNo <- liftIO $ query1 c "SELECT link_no FROM link_to_review \
>                               \WHERE member_no = ? AND current_timestamp >= target_time \
>                               \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>                        `catchSqlE` "Failed to retrieve next link for review."
>   case linkNo of
>     Nothing -> noLinksToReviewPage c memberNo
>     Just n  -> reviewLinkPage $ fromSql n

> reviewLinkPage :: Integer -> CGI CGIResult
> reviewLinkPage _ = output $ "blah"

> noLinksToReviewPage :: IConnection conn => conn -> Integer -> CGI CGIResult
> noLinksToReviewPage c memberNo = do
>   nextReview <- liftIO $ nextReviewTime c memberNo
>   let next = case nextReview of
>                Nothing   -> noHtml
>                Just diff -> paragraph <<
>                               ("Your next link for review is in " ++
>                                (timeDiffToString diff))
>   output $ renderHtml $ page t ["lexeme"]
>     [ h1 << t,
>       paragraph << "You don't have any links to review.",
>       next ]
>     where t = "No Links to Review"

> nextReviewTime :: IConnection conn => conn -> Integer -> IO (Maybe TimeDiff)
> nextReviewTime c memberNo = do
>   next <- query1 c "SELECT target_time - current_timestamp FROM link_to_review \
>                    \WHERE member_no = ? AND target_time > current_timestamp \
>                    \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>             `catchSqlE` "Failed to determine next review time."
>   case next of
>     Nothing -> return Nothing
>     Just n  -> return $ Just (fromSql n)

> reviewHtml :: IConnection conn => conn -> Integer -> Integer -> IO (Html)
> reviewHtml c memberNo linkNo = do
>   if memberNo == 0
>     then return $ paragraph ! [theclass "review-box login"] <<
>                     anchor ! [href "/member/login"] << "Login to Review" 
>     else do
>       r <- reviewing c memberNo linkNo
>       return $ r ? paragraph ! [theclass "review-box reviewing"] << "Reviewing" $
>                    form ! [action ("/review/" ++ (show linkNo)),
>                            method "post", theclass "review-box review"] <<
>                      input ! [thetype "submit", name "review", value "Review"]

Determine whether or not a member is already reviewing this link. This will be
true only if the member is currently reviewing the link, not if they've
reviewed it in the past but removed it from their review.

> reviewing :: IConnection conn => conn -> Integer -> Integer -> IO (Bool)
> reviewing c memberNo linkNo = do
>   r <- query1 c "SELECT link_no FROM link_to_review \
>                 \WHERE member_no = ? AND link_no = ? LIMIT 1"
>                 [toSql memberNo, toSql linkNo]
>   case r of
>     Nothing -> return False
>     Just _  -> return True