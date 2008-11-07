> module Vocabulink.Review where

> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Utils

> import Vocabulink.Review.SM2

> import Codec.Binary.UTF8.String
> import Control.Monad (liftM)
> import Database.HDBC
> import Data.Maybe (fromMaybe)
> import Network.CGI
> import Text.XHtml.Strict
> import System.Time

> scheduleReview :: IConnection conn => conn -> Integer -> Integer -> String -> IO ()
> scheduleReview c memberNo linkNo _ = do
>   quickInsert c "INSERT INTO link_to_review (member_no, link_no) \
>                 \VALUES (?, ?)" [toSql memberNo, toSql linkNo]
>     `catchSqlE` "You already have this link scheduled for review or there was an error."

> newReview :: Integer -> String -> CGI CGIResult
> newReview memberNo set = do
>   c <- liftIO db
>   link <- getInput' "link"
>   no <- liftIO $ intFromString link
>   case no of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       liftIO $ scheduleReview c memberNo n set
>       referer >>= redirect

Review the next link in the queue.

> reviewLink :: Integer -> CGI CGIResult
> reviewLink memberNo = do
>   c <- liftIO db
>   linkNo <- liftIO $ query1 c "SELECT link_no FROM link_to_review \
>                               \WHERE member_no = ? AND current_timestamp >= target_time \
>                               \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>                        `catchSqlE` "Failed to retrieve next link for review."
>   maybe noLinksToReviewPage reviewLinkPage (fromSql `liftM` linkNo)

> reviewLinkPage :: Integer -> CGI CGIResult
> reviewLinkPage linkNo = do
>   c <- liftIO db
>   (o,d) <- liftIO $ getLink c linkNo
>   let origin = encodeString o
>       destination = encodeString d
>   outputHtml $ page ("Review " ++ origin ++ " -> ?")
>                     [CSS "lexeme", JS "MochiKit", JS "review"]
>     [ thediv ! [identifier "baseline", theclass "link"] <<
>         linkHtml (stringToHtml origin) (anchor ! [identifier "lexeme-cover", href "#"] << "?"),
>       form ! [action ("/review/" ++ (show linkNo)), method "post"] <<
>         [ hidden "recall-time" "",
>           hidden "hidden-lexeme" destination,
>           fieldset ! [identifier "recall-buttons", thestyle "display: none"] <<
>             map recallButton [0..5] ] ]

> recallButton :: Integer -> Html
> recallButton i = let q :: Double = (fromIntegral i) / 5 in
>                  button ! [name "recall", value (show q)] << show i

> noLinksToReviewPage :: CGI CGIResult
> noLinksToReviewPage = do
>   outputHtml $ page t [CSS "lexeme"]
>     [ h1 << t,
>       paragraph << "Take a break! You don't have any links to review right now." ]
>         where t = "No Links to Review"

> nextReviewTime :: IConnection conn => conn -> Integer -> IO (Maybe TimeDiff)
> nextReviewTime c memberNo = do
>   next <- query1 c "SELECT extract(epoch FROM (target_time - current_timestamp)) \
>                    \FROM link_to_review \
>                    \WHERE member_no = ? AND target_time > current_timestamp \
>                    \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>             `catchSqlE` "Failed to determine next review time."
>   return $ fromSql `liftM` next

> linkReviewed' :: Integer -> String -> CGI CGIResult
> linkReviewed' memberNo link = do
>   linkNo <- liftIO $ intFromString link
>   case linkNo of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       c <- liftIO db
>       recall <- getInput' "recall"
>       recallTime <- getInput' "recall-time"
>       liftIO $ linkReviewed c memberNo n recall recallTime
>       redirect "/review/next"

Note that a link was reviewed and schedule the next review. For testing
purposes, we schedule the review forward an hour.

> linkReviewed :: IConnection conn => conn -> Integer -> Integer -> Double -> Integer -> IO ()
> linkReviewed c memberNo linkNo recall recallTime = do
>   previous <- previousInterval c memberNo linkNo
>   seconds <- reviewInterval c memberNo linkNo previous recall
>   withTransaction c $ \c' -> do
>       run c' "INSERT INTO link_review (member_no, link_no, recall, \
>                                       \recall_time, target_time) \
>              \VALUES (?, ?, ?, ?, \
>                      \(SELECT target_time FROM link_to_review \
>              \WHERE member_no = ? AND link_no = ?))"
>              [toSql memberNo, toSql linkNo, toSql recall,
>               toSql recallTime, toSql memberNo, toSql linkNo]
>       let s = fromMaybe 0 seconds
>       run c' ("UPDATE link_to_review \
>               \SET target_time = current_timestamp + interval \
>               \'" ++ (show s) ++ " seconds" ++ "' \
>               \WHERE member_no = ? AND link_no = ?")
>              [toSql memberNo, toSql linkNo]
>       return ()
>     `catchSqlE` "Failed to record review of link."

Determine the previous interval in seconds.

> previousInterval :: IConnection conn => conn -> Integer -> Integer -> IO (Integer)
> previousInterval c memberNo linkNo = do
>   d <- query1 c "SELECT extract(epoch from current_timestamp - \
>                        \(SELECT actual_time FROM link_review \
>                         \WHERE member_no = ? AND link_no = ? \
>                         \ORDER BY actual_time DESC LIMIT 1))"
>                   [toSql memberNo, toSql linkNo]
>     `catchSqlE` "Failed to determine previous review interval."
>   return $ maybe 0 fromSql d
