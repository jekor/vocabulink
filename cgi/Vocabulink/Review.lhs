% Copyright 2008, 2009, 2010 Chris Forno

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

\section{Review}

Creating links is great, but viewing a link doesn't mean that you've learned
it. We need a way to present links to members for regular (scheduled) reviews.

> module Vocabulink.Review (newReview, linkReviewed, nextReview) where

For now, we have only 1 review algorithm (SuperMemo 2).

> import qualified Vocabulink.Review.SM2 as SM2

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Form
> import Vocabulink.Html
> import Vocabulink.Page
> import Vocabulink.Link
> import Vocabulink.Utils

> import Prelude hiding (div, span, id)

\subsection{Review Scheduling}

When a member indicates that they want to review a link, we just add it to the
@link_to_review@ relation. This may change if we ever support multiple review
sets. The link review time is set to the current time by default so that it
immediately shows up for review (something we want no matter which algorithm
we're using).

We need to give the user feedback that they've successfully added the link to
their review set. For now, we redirect them back to the referrer because we
assume it will be the link page (which will then indicate in some way that
they're reviewing the link now). However, this is a good candidate for an
asynchronous JavaScript call.

> newReview :: Integer -> Integer -> App CGIResult
> newReview memberNo linkNo = do
>   $(execute' "INSERT INTO link_to_review (member_no, link_no) \
>                                  \VALUES ({memberNo}, {linkNo})")
>   outputJSON [(""::String, ""::String)]

The client indicates a completed review with a @POST@ to @/review/linknumber@
which will be dispatched to |linkReviewed|. Once we schedule the next review
time for the link, we move on to the next in their set.

> linkReviewed :: Integer -> Integer -> App CGIResult
> linkReviewed memberNo linkNo = do
>   recall      <- readRequiredInput "recall"
>   recallTime  <- readRequiredInput "recall-time"
>   scheduleNextReview memberNo linkNo recall recallTime
>   redirect "/review/next"

We need to schedule the next review based on the review algorithm in use. The
algorithm needs to know how well the item was remembered. Also, we log the
amount of time it took to recall the item. The SM2 algorithm does not use this
information (nor any SuperMemo algorithm that I know of), but we may find it
useful when analyzing data later.

@recall@ is passed as a real number between 0 and 1 to allow for future
variations in recall rating (such as fewer choices than 1 through 5 or less
discrete options like a slider). 0 indicates complete failure while 1 indicates
perfect recall.

@previous@ is passed as well. This is the time in seconds between this and the
last review (the actual time difference, not the scheduled time difference).

All database updates during this process are wrapped in a transaction.

> scheduleNextReview :: Integer -> Integer -> Float -> Integer -> App ()
> scheduleNextReview memberNo linkNo recall recallTime = do
>   previous <- fromJust <$> previousInterval memberNo linkNo
>   diff <- SM2.reviewInterval memberNo linkNo previous recall
>   h <- asks appDB
>   liftIO $ withTransaction h $ do
>     $(execute
>       "INSERT INTO link_review (member_no, link_no, recall, recall_time, \
>                                \target_time) \
>                        \VALUES ({memberNo}, {linkNo}, {recall}, {recallTime}, \
>                                \(SELECT target_time FROM link_to_review \
>                                 \WHERE member_no = {memberNo} AND link_no = {linkNo}))") h
>     $(execute
>       "UPDATE link_to_review \
>       \SET target_time = current_timestamp + {diff} \
>       \WHERE member_no = {memberNo} AND link_no = {linkNo}") h

\subsection{Review Pages}

Here's the entry point for the client to request reviews. It's pretty simple:
we just request the next link from @link_to_review@ by @target_time@. If
there's none, we send the member to a ``congratulations'' page. If there is a
link for review, we send them to the review page.

> nextReview :: Integer -> App CGIResult
> nextReview memberNo = do
>   row <- $(queryTuple'
>     "SELECT link_no FROM link_to_review \
>     \WHERE member_no = {memberNo} AND current_timestamp >= target_time \
>     \ORDER BY target_time ASC LIMIT 1")
>   case row of
>     Nothing -> noLinksToReviewPage memberNo
>     Just n  -> reviewLinkPage n

The review page is pretty simple. It displays a link without the typical
link-type decorations and with the destination node covered by a question mark.
Once the member clicks the question mark or presses the space bar (to find out
what is hidden beneath it) it reveals the lexeme, records the total amount of
recall time taken, and displays a recall feedback form (currently a row of 6
buttons for working with the SM2 algorithm).

Once the member clicks a recall number, it sends the information off to
|linkReviewed| to record the details and schedule the next review. This sends
the client to |nextReview| which begins the process all over again.

> reviewLinkPage :: Integer -> App CGIResult
> reviewLinkPage linkNo = do
>   l <- getLink linkNo
>   case l of
>     Nothing  -> simplePage "Error: Unable to retrieve link." [CSS "link"] mempty
>     Just l'  -> do
>       let source  = linkOrigin l'
>           dest    = linkDestination l'
>       sLang  <- linkOriginLanguage l'
>       dLang  <- linkDestinationLanguage l'
>       fullLink <- renderLink l'
>       stdPage ("Review: " ++ source ++ " → ?") [CSS "link", JS "lib.link"] mempty $ do
>         h1 ! id "review-link" ! class_ "link review" $ do
>           span ! class_ "orig" ! title (stringValue sLang) $ string source
>           span ! class_ "link" $ mempty
>           a ! class_ "dest hidden" ! title (stringValue dLang) $ "?"
>         fullLink ! id "full-link" ! style "display: none"
>         form ! action (stringValue $ "/review/" ++ show linkNo) ! method "post" $ do
>           input ! type_ "hidden" ! id "recall-time" ! name "recall-time"
>           input ! type_ "hidden" ! name "hidden-lexeme" ! value (stringValue dest)
>           fieldset ! id "recall-buttons" ! style "display: none" $ do
>             mconcat $ map (recallButton 5) [0..5]

The next review time can be in the future or in the past.

> nextReviewTime :: Integer -> App (Maybe UTCTime)
> nextReviewTime memberNo = liftM join $(queryTuple'
>   "SELECT MIN(target_time) FROM link_to_review \
>   \WHERE member_no = {memberNo}")

In order to determine the next review interval, the review scheduling algorithm
may need to know how long the last review period was (in fact, any algorithm
based on spaced reptition will). This returns the actual, not scheduled, amount
of time between the current and last review in seconds.

Note that this will not work before the link has been reviewed. We expect that
the review algorithm does not have to be used for determining the first review
(immediate).

> previousInterval :: Integer -> Integer -> App (Maybe DiffTime)
> previousInterval memberNo linkNo = do
>   t <- $(queryTuple'
>     "SELECT COALESCE(extract(epoch from current_timestamp - \
>                             \(SELECT actual_time FROM link_review \
>                              \WHERE member_no = {memberNo} AND link_no = {linkNo} \
>                              \ORDER BY actual_time DESC LIMIT 1))::int, \
>                     \extract(epoch from current_timestamp - \
>                             \(SELECT target_time FROM link_to_review \
>                              \WHERE member_no = {memberNo} AND link_no = {linkNo}))::int)")
>   case t of
>     Nothing -> return Nothing
>     Just t' -> return $ liftM secondsToDiffTime t'

This creates a ``recall button''. It returns a button with a decimal recall
value based on an integral button number. It hopefully allows us to make the
recall options more flexible in the future.

You may get unpleasant results when passing a |total| that doesn't cleanly
divide |i|.

> recallButton :: Integer -> Integer -> Html
> recallButton total i = let q = fromIntegral i / fromIntegral total in
>                        button ! name "recall" ! value (stringValue $ show q) $ string (show i)

When a member has no more links to review for now, let's display a page letting
them know that.

Here's a critical chance to:

\begin{itemize}
\item Give positive feedback to encourage the behavior of getting through the
      review set.
\item Point the member to other places of interest on the site.
\end{itemize}

But for now, the page is pretty plain. We congratulate the member and let them
know when their next review is scheduled.

> noLinksToReviewPage :: Integer -> App CGIResult
> noLinksToReviewPage memberNo = do
>   t <- nextReviewTime memberNo
>   now <- liftIO getCurrentTime
>   simplePage "No Links to Review" [CSS "link", JS "lib.link"] $ do
>     div ! id "central-column" $ do
>       p ! style "text-align: center" $ "Take a break! You don't have any links to review right now."
>       case t of
>         Just t'  -> p ! style "text-align: center" $ do
>                       string "Your next review is due "
>                       span ! id "countdown" $ do
>                         string "in "
>                         span ! class_ "seconds" $ (string $ show (round $ diffUTCTime t' now))
>                         string " seconds"
>                       string "."
>         Nothing  -> mempty
