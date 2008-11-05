> module Vocabulink.Review.SM2 where

SuperMemo algorithm SM-2

http://www.supermemo.com/english/ol/sm2.htm

> import Vocabulink.Review

> interval :: Float -> Int -> Float -> Float
> interval _ 1 _  = 1.0
> interval _ 2 _  = 6.0
> interval p _ eF = p * eF

We have a unique advantage in that it's possible for us to determine EF
(easiness factor) of an item through collaboration. However, I'm not sure if
that's necessary or will pay off as much as moving to a newer algorithm.

For now we'll stick to separate EF for each member.

> eF' :: Float -> Float -> Float
> eF' eF q = min 1.3 $ eF + (0.1 - (5-q) * (0.08 + (5-q) * 0.02))

Return the number of days after the current review time to schedule the
following review.

-- > reviewInterval :: IConnection conn => conn -> Integer -> Integer -> Int -> Float
-- > reviewInterval c memberNo linkNo q = do
-- >   