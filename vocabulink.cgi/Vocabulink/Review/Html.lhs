> module Vocabulink.Review.Html where

> import Vocabulink.DB
> import Vocabulink.Html
> import Vocabulink.Utils

> import Database.HDBC
> import Text.XHtml.Strict

For now, all links are added to the default review set.

> reviewHtml :: IConnection conn => conn -> Integer -> Integer -> IO (Html)
> reviewHtml c memberNo linkNo = do
>   if memberNo == 0
>     then return $ paragraph ! [theclass "review-box login"] <<
>                     anchor ! [href "/member/login"] << "Login to Review" 
>     else do
>       r <- reviewing c memberNo linkNo
>       return $ r ? paragraph ! [theclass "review-box reviewing"] << "Reviewing" $
>                    form ! [action ("/review/set" ++ "/"),
>                            method "post", theclass "review-box review"] <<|
>                      [ input ! [thetype "hidden", name "link", value (show linkNo)],
>                        input ! [thetype "submit", name "review", value "Review"] ]

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