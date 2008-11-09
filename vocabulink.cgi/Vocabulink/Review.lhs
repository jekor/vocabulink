> module Vocabulink.Review where

> import Vocabulink.Review.SM2 (reviewInterval)

> import Vocabulink.App
> import Vocabulink.CGI (getInput', referer)
> import Vocabulink.DB (query1, quickInsert, catchSqlE)
> import Vocabulink.Html (outputHtml, page, Dependency(..))
> import Vocabulink.Link (getLink, linkHtml)
> import Vocabulink.Utils (intFromString)

> import Codec.Binary.UTF8.String (encodeString)
> import Control.Monad (liftM)
> import Control.Monad.Reader (asks)
> import Database.HDBC (withTransaction, run, toSql, fromSql)
> import Data.Maybe (fromMaybe)
> import Network.FastCGI (CGIResult, liftIO, outputError, redirect)
> import Text.XHtml.Strict
> import System.Time (TimeDiff)

> scheduleReview :: Integer -> Integer -> String -> App ()
> scheduleReview memberNo linkNo _ = do
>   c <- asks db
>   liftIO $ quickInsert c "INSERT INTO link_to_review (member_no, link_no) \
>                          \VALUES (?, ?)" [toSql memberNo, toSql linkNo]
>              `catchSqlE` "You already have this link scheduled for review or there was an error."

> newReview :: Integer -> String -> App CGIResult
> newReview memberNo set = do
>   link <- getInput' "link"
>   no <- liftIO $ intFromString link
>   case no of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       scheduleReview memberNo n set
>       referer >>= redirect

Review the next link in the queue.

> reviewLink :: Integer -> App CGIResult
> reviewLink memberNo = do
>   c <- asks db
>   linkNo <- liftIO $ query1 c "SELECT link_no FROM link_to_review \
>                               \WHERE member_no = ? AND current_timestamp >= target_time \
>                               \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>                        `catchSqlE` "Failed to retrieve next link for review."
>   maybe noLinksToReviewPage reviewLinkPage (fromSql `liftM` linkNo)

> reviewLinkPage :: Integer -> App CGIResult
> reviewLinkPage linkNo = do
>   (o,d) <- getLink linkNo
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

> noLinksToReviewPage :: App CGIResult
> noLinksToReviewPage = do
>   outputHtml $ page t [CSS "lexeme"]
>     [ h1 << t,
>       paragraph << "Take a break! You don't have any links to review right now." ]
>         where t = "No Links to Review"

> nextReviewTime :: Integer -> App (Maybe TimeDiff)
> nextReviewTime memberNo = do
>   c <- asks db
>   next <- liftIO $ query1 c "SELECT extract(epoch FROM (target_time - current_timestamp)) \
>                             \FROM link_to_review \
>                             \WHERE member_no = ? AND target_time > current_timestamp \
>                             \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>                      `catchSqlE` "Failed to determine next review time."
>   return $ fromSql `liftM` next

> linkReviewed' :: Integer -> String -> App CGIResult
> linkReviewed' memberNo link = do
>   linkNo <- liftIO $ intFromString link
>   case linkNo of
>     Left  _ -> outputError 400 "Links are identified by numbers only." []
>     Right n -> do
>       recall <- getInput' "recall"
>       recallTime <- getInput' "recall-time"
>       linkReviewed memberNo n recall recallTime
>       redirect "/review/next"

Note that a link was reviewed and schedule the next review. For testing
purposes, we schedule the review forward an hour.

> linkReviewed :: Integer -> Integer -> Double -> Integer -> App ()
> linkReviewed memberNo linkNo recall recallTime = do
>   previous <- previousInterval memberNo linkNo
>   seconds <- reviewInterval memberNo linkNo previous recall
>   c <- asks db
>   liftIO $ withTransaction c $ \c' -> do
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

> previousInterval :: Integer -> Integer -> App (Integer)
> previousInterval memberNo linkNo = do
>   c <- asks db
>   d <- liftIO $ query1 c "SELECT extract(epoch from current_timestamp - \
>                                 \(SELECT actual_time FROM link_review \
>                                  \WHERE member_no = ? AND link_no = ? \
>                                  \ORDER BY actual_time DESC LIMIT 1))"
>                          [toSql memberNo, toSql linkNo]
>                   `catchSqlE` "Failed to determine previous review interval."
>   return $ maybe 0 fromSql d
