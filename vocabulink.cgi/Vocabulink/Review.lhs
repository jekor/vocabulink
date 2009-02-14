> module Vocabulink.Review where

> import Vocabulink.Review.SM2 (reviewInterval)

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Link (getLink, linkHtml, Link(..))
> import Vocabulink.Utils

> newReview :: Integer -> Integer -> App CGIResult
> newReview memberNo linkNo = do
>   scheduleReview memberNo linkNo
>   refererOrVocabulink >>= redirect

> linkReviewed :: Integer -> Integer -> App CGIResult
> linkReviewed memberNo linkNo = do
>   recall <- readRequiredInput "recall"
>   recallTime <- readRequiredInput "recall-time"
>   linkReviewed' memberNo linkNo recall recallTime
>   redirect "/review/next"

> scheduleReview :: Integer -> Integer -> App ()
> scheduleReview memberNo linkNo = do
>   c <- asks db
>   liftIO $ quickStmt c "INSERT INTO link_to_review (member_no, link_no) \
>                        \VALUES (?, ?)" [toSql memberNo, toSql linkNo]
>              `catchSqlE` "You already have this link scheduled for review or there was an error."

Review the next link in the queue.

> reviewLink :: Integer -> App CGIResult
> reviewLink memberNo = do
>   linkNo <- queryValue'
>     "SELECT link_no FROM link_to_review \
>     \WHERE member_no = ? AND current_timestamp >= target_time \
>     \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>   case linkNo of
>     Just (Just n)  -> reviewLinkPage $ fromSql n
>     Just Nothing   -> noLinksToReviewPage
>     _              -> error "Failed to retrieve next link for review."

> reviewLinkPage :: Integer -> App CGIResult
> reviewLinkPage linkNo = do
>   l <- getLink linkNo
>   case l of
>     Nothing -> stdPage "Unable to retrieve link." [CSS "link"]
>       [ stringToHtml "Unable to retrieve link." ]
>     Just (Link _ _ o d) -> do
>       let origin = encodeString o
>           destination = encodeString d
>       stdPage ("Review: " ++ origin ++ " -> ?")
>               [CSS "link", JS "MochiKit", JS "review"]
>         [ thediv ! [identifier "baseline", theclass "link"] <<
>             linkHtml (stringToHtml origin) (anchor ! [identifier "lexeme-cover", href "#"] << "?"),
>           form ! [action ("/review/" ++ (show linkNo)), method "post"] <<
>             [ hidden "recall-time" "",
>               hidden "hidden-lexeme" destination,
>               fieldset ! [identifier "recall-buttons", thestyle "display: none"] <<
>                 map recallButton [0..5] ] ]

> recallButton :: Integer -> Html
> recallButton i = let q :: Double = (fromIntegral i) / 5 in
>                  button ! [name "recall", value (show q)] << show i

> noLinksToReviewPage :: App CGIResult
> noLinksToReviewPage = do
>   stdPage t [CSS "link"]
>     [ h1 << t,
>       paragraph << "Take a break! You don't have any links to review right now." ]
>         where t = "No Links to Review"

Get the number of links that a user has for review.

> numLinksToReview :: Integer -> App (Maybe Integer)
> numLinksToReview memberNo = do
>   n <- queryValue'  "SELECT COUNT(*) FROM link_to_review \
>                     \WHERE member_no = ? AND current_timestamp > target_time"
>                     [toSql memberNo]
>   return $ fmap (maybe 0 fromSql) n

Note that a link was reviewed and schedule the next review. For testing
purposes, we schedule the review forward an hour.

> linkReviewed' :: Integer -> Integer -> Double -> Integer -> App ()
> linkReviewed' memberNo linkNo recall recallTime = do
>   c' <- asks db
>   liftIO $ withTransaction c' $ \c -> do
>     previous <- previousInterval c memberNo linkNo
>     seconds <- reviewInterval c memberNo linkNo previous recall
>     run c "INSERT INTO link_review (member_no, link_no, recall, \
>                                    \recall_time, target_time) \
>           \VALUES (?, ?, ?, ?, \
>                   \(SELECT target_time FROM link_to_review \
>           \WHERE member_no = ? AND link_no = ?))"
>           [toSql memberNo, toSql linkNo, toSql recall,
>            toSql recallTime, toSql memberNo, toSql linkNo]
>     let s = fromMaybe 0 seconds
>     run c ("UPDATE link_to_review \
>            \SET target_time = current_timestamp + interval \
>            \'" ++ (show s) ++ " seconds" ++ "' \
>            \WHERE member_no = ? AND link_no = ?")
>           [toSql memberNo, toSql linkNo]
>     return ()
>    `catchSqlE` "Failed to record review of link."

Determine the previous interval in seconds.

> previousInterval :: IConnection conn => conn -> Integer -> Integer -> IO (Integer)
> previousInterval c memberNo linkNo = do
>   d <- queryValue c "SELECT extract(epoch from current_timestamp - \
>                                    \(SELECT actual_time FROM link_review \
>                                     \WHERE member_no = ? AND link_no = ? \
>                                     \ORDER BY actual_time DESC LIMIT 1))"
>                     [toSql memberNo, toSql linkNo]
>          `catchSqlE` "Failed to determine previous review interval."
>   return $ maybe 0 fromSql d
