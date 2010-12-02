> module Vocabulink.Rating (ratingBar, rateLink) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Form
> import Vocabulink.Html
> import Vocabulink.Utils

> import Prelude hiding (div, span, id)

> barWidth :: Integer
> barWidth = 100

> spriteHeight :: Integer
> spriteHeight = 22

> numColors :: Integer
> numColors = 5

The 5-star system is a little bit confusing. We want to keep our ratings as a
value between 0 and 1 since it's the most logical and flexible for the future.
We can convert the continuous (average) rating into stars, but doing so would
lead to cases where "even" ratings (ratings that should fall exactly on a star
boundary) will shift a little bit. This is especially likely to happen when an
item has 1 rating.

For example, so that someone rates something with 4 stars. In our system,
that's 0.75 (1 star = 0, 2 stars = 0.25 .. 5 stars = 1). But if we go to
display that to the user using the full width of the stars to display the
value, we'll end up with 0.75 * 5 stars = 3.75 stars, which they wouldn't
expect. This would be especially problematic after someone gave something a
1-star rating (which counts as 0) because it would appear that the rating
wasn't taken at all (the stars would be filled up 0 pixels wide).

So instead what we do is we re-align the rating display in the same way as we
re-align the rating scale when taking the rating from the user: we place 0 at 1
full star into the rating system.

> ratingBar :: String -> Integer -> Maybe Float -> Bool -> Html
> ratingBar baseUrl numRatings rating allowRating =
>   let  r = fromMaybe 0 rating
>        starWidth = case rating of
>                      Just r'  -> round $ (0.2 + 0.8 * r') * fromIntegral barWidth :: Integer
>                      Nothing  -> 0
>        starColor = min (floor (r * fromIntegral numColors) + 1) 5
>        starPosition = -1 * spriteHeight * (numColors + 1) + starColor * spriteHeight in
>   div ! class_ (stringValue $ "rating" ++ (allowRating ? " enabled" $ ""))
>       ! title (stringValue $ show numRatings ++ " rating" ++ (numRatings == 1 ? "" $ "s")) $ do
>     string "rate: "
>     div ! class_ "stars-base" $ do
>       form ! class_ "stars" ! action (stringValue baseUrl) ! method "post"
>            ! style (stringValue $ "width: " ++ show starWidth ++ "px; background-position: left " ++ show starPosition ++ "px") $ mempty

> rateLink :: Integer -> App CGIResult
> rateLink ln = withRequiredMemberNumber $ \memberNo -> do
>   rating <- readRequiredInput "rating" :: App Double
>   $(execute' "INSERT INTO link_rating (link_no, member_no, rating) \
>                               \VALUES ({ln}, {memberNo}, {rating})")
>   output' ""
