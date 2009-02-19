> module Vocabulink.Review.Html where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

For now, all links are added to the default review set.

> reviewHtml :: Integer -> App (Html)
> reviewHtml linkNo = do
>   memberNo <- asks memberNumber
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
>                         form ! [action ("/review/" ++ (show linkNo) ++ "/add"),
>                                method "POST", theclass "review-box review"] <<
>                           [ submit "review" "Review" ]

Determine whether or not a member is already reviewing this link. This will be
true only if the member is currently reviewing the link, not if they've
reviewed it in the past but removed it from their review.

> reviewing :: Integer -> Integer -> App (Maybe Bool)
> reviewing memberNo linkNo = do
>   r <- queryValue' "SELECT link_no FROM link_to_review \
>                    \WHERE member_no = ? AND link_no = ? LIMIT 1"
>                    [toSql memberNo, toSql linkNo]
>   return $ fmap isJust r
