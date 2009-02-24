\section{Review}

Now that we know how to create links, its time to look at how to present them
to a member for review.

> module Vocabulink.Review (  newReview, linkReviewed, reviewLink,
>                             numLinksToReview) where

For now, we have only 1 algorithm.

> import qualified Vocabulink.Review.SM2 as SM2

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Link
> import Vocabulink.Utils

When a member indicates that they want to review a link, we just add it to the
@link_to_review@ table. This may change if we ever support multiple review
sets. The link review time is set to the current time by default so that it
immediately shows up for review (something we want no matter which algorithm
we're using).

We need to give the user feedback that they've successfully added the link to
their review set. For now, we redirect them back to the referrer because we
assume it will be the link page (which will then indicate in some way that
they're reviewing the link now). However, this is a good candidate for an
asynchronous call.

> newReview :: Integer -> Integer -> App CGIResult
> newReview memberNo linkNo = do
>   res <- quickStmt' "INSERT INTO link_to_review (member_no, link_no) \
>                     \VALUES (?, ?)" [toSql memberNo, toSql linkNo]
>   case res of
>     Nothing  -> error "Error scheduling link for review."
>     Just _   -> redirect =<< refererOrVocabulink

The client indicates a successful review with a @POST@ to @/review/linknumber/@
which will be dispatched to |linkReviewed|. Once we schedule the next review
time for the link, we move on to the next in their set.

> linkReviewed :: Integer -> Integer -> App CGIResult
> linkReviewed memberNo linkNo = do
>   recall      <- readRequiredInput "recall"
>   recallTime  <- readRequiredInput "recall-time"
>   res <- scheduleNextReview memberNo linkNo recall recallTime
>   case res of
>     Nothing  -> error "Failed to schedule next review."
>     Just _   -> redirect "/review/next"

Note that a link was reviewed and schedule the next review. For testing
purposes, we schedule the review forward an hour.

> scheduleNextReview :: Integer -> Integer -> Double -> Integer -> App (Maybe ())
> scheduleNextReview memberNo linkNo recall recallTime = do
>   withTransaction' $ do
>     previous <- previousInterval memberNo linkNo
>     seconds <- maybe (return Nothing)
>                (\p -> SM2.reviewInterval memberNo linkNo p recall) previous
>     run' "INSERT INTO link_review (member_no, link_no, recall, \
>                                   \recall_time, target_time) \
>          \VALUES (?, ?, ?, ?, \
>                 \(SELECT target_time FROM link_to_review \
>          \WHERE member_no = ? AND link_no = ?))"
>          [  toSql memberNo, toSql linkNo, toSql recall,
>             toSql recallTime, toSql memberNo, toSql linkNo]
>     let s = fromMaybe 0 seconds
>     run' ("UPDATE link_to_review \
>           \SET target_time = current_timestamp + interval \
>           \'" ++ (show s) ++ " seconds" ++ "' \
>           \WHERE member_no = ? AND link_no = ?")
>          [toSql memberNo, toSql linkNo]
>     return ()

Review the next link in the queue.

> reviewLink :: Integer -> App CGIResult
> reviewLink memberNo = do
>   linkNo <- queryTuples'
>     "SELECT link_no FROM link_to_review \
>     \WHERE member_no = ? AND current_timestamp >= target_time \
>     \ORDER BY target_time ASC LIMIT 1" [toSql memberNo]
>   case linkNo of
>     Just []     -> noLinksToReviewPage
>     Just [[n]]  -> reviewLinkPage $ fromSql n
>     _           -> error "Failed to retrieve next link for review."

> reviewLinkPage :: Integer -> App CGIResult
> reviewLinkPage linkNo = do
>   l <- getLink linkNo
>   case l of
>     Nothing  -> simplePage "Unable to retrieve link." [CSS "link"] []
>     Just l'  -> do
>       let source  = encodeString $ linkOrigin l'
>           dest    = encodeString $ linkDestination l'
>       stdPage ("Review: " ++ source ++ " -> ?")
>               [CSS "link", JS "MochiKit", JS "review"]
>         [ thediv ! [identifier "baseline", theclass "link"] <<
>             linkHtml (stringToHtml source) (anchor ! [identifier "lexeme-cover", href "#"] << "?"),
>           form ! [action ("/review/" ++ (show linkNo)), method "POST"] <<
>             [ hidden "recall-time" "",
>               hidden "hidden-lexeme" dest,
>               fieldset ! [identifier "recall-buttons", thestyle "display: none"] <<
>                 map recallButton [0..5] ] ]

> recallButton :: Integer -> Html
> recallButton i = let q :: Double = (fromIntegral i) / 5 in
>                  button ! [name "recall", value (show q)] << show i

> noLinksToReviewPage :: App CGIResult
> noLinksToReviewPage = do
>   simplePage t [CSS "link"]
>     [ paragraph << "Take a break! You don't have any links to review right now." ]
>         where t = "No Links to Review"

Get the number of links that a user has for review.

> numLinksToReview :: Integer -> App (Maybe Integer)
> numLinksToReview memberNo = do
>   v <- queryValue'  "SELECT COUNT(*) FROM link_to_review \
>                     \WHERE member_no = ? AND current_timestamp > target_time"
>                     [toSql memberNo]
>   return $ fmap fromSql v

Determine the previous interval in seconds.

> previousInterval :: Integer -> Integer -> App (Maybe Integer)
> previousInterval memberNo linkNo = do
>   v <- queryValue'  "SELECT extract(epoch from current_timestamp - \
>                                    \(SELECT actual_time FROM link_review \
>                                     \WHERE member_no = ? AND link_no = ? \
>                                     \ORDER BY actual_time DESC LIMIT 1))"
>                     [toSql memberNo, toSql linkNo]
>   return $ fmap fromSql v
