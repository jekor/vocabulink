\section{Review Html}

This is broken off from the main Review module to break cyclical imports. It
may end up being more work than it's worth.

> module Vocabulink.Review.Html (reviewBox, reviewIndicator) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Utils

> import Text.XHtml.Strict

We display the number of links that are waiting for review for logged in
members in the standard page header. Reviewing is currently the primary
function of Vocabulink, and we want it prominently displayed.

The idea is that a member will go to the site, and we want them to be instantly
reminded that they have links to review. Or, if a link for review becomes due
while they are browsing another part of the site, we want them to be notified.

> reviewBox :: App Html
> reviewBox = withMemberNumber noHtml $ \memberNo -> do
>   n <- numLinksToReview memberNo
>   return $ case n of
>     Just 0   -> anchor ! [href "/links", theclass "review-box"] <<
>                   "No links to review"
>     Just n'  -> anchor ! [href "/review/next", theclass "review-box"] <<
>                   [(strong << show n') +++ " links to review"]
>     Nothing  -> stringToHtml "Error finding links for review."

Get the number of links that a user has for review. We display this number to
the member in the header bar.

> numLinksToReview :: Integer -> App (Maybe Integer)
> numLinksToReview memberNo = do
>   v <- queryValue'  "SELECT COUNT(*) FROM link_to_review \
>                     \WHERE member_no = ? AND current_timestamp > target_time"
>                     [toSql memberNo]
>   return $ fmap fromSql v

When displaying a link, it's useful to show its review status (or a method to
add it to a review set if it's not). This returns an Html element that will do
both based on the link number and the currently logged in member.

> reviewIndicator :: Integer -> App (Html)
> reviewIndicator linkNo = do
>   memberNo <- asks appMemberNo
>   case memberNo of
>     Nothing -> return $ paragraph ! [theclass "review-box login"] <<
>                           anchor ! [href "/member/login"] << "Login to Review" 
>     Just n  -> do
>       r <- reviewing n linkNo
>       case r of
>         Nothing  -> return $ paragraph ! [theclass "review-box"] <<
>                       "Unable to determine review status."
>         Just r'  -> return $ r' ? paragraph ! [theclass "review-box reviewing"] <<
>                       "Reviewing" $
>                         form ! [  action ("/review/" ++ (show linkNo) ++ "/add"),
>                                   method "POST", theclass "review-box review"] <<
>                           [ submit "review" "Review" ]

Determine whether or not a member is already reviewing this link. This will be
true only if the member is currently reviewing the link, not if they've
reviewed it in the past but removed it from their review.

> reviewing :: Integer -> Integer -> App (Maybe Bool)
> reviewing memberNo linkNo = do
>   r <- queryValue'  "SELECT link_no FROM link_to_review \
>                     \WHERE member_no = ? AND link_no = ? LIMIT 1"
>                     [toSql memberNo, toSql linkNo]
>   return $ fmap fromSql r
