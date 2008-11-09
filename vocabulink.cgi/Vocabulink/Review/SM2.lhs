> module Vocabulink.Review.SM2 where

SuperMemo algorithm SM-2

http://www.supermemo.com/english/ol/sm2.htm

> import Vocabulink.App
> import Vocabulink.DB (quickInsert, catchSqlE)

> import Control.Monad.Reader (asks)
> import Database.HDBC (quickQuery, run, withTransaction, toSql, fromSql)

> interval :: Double -> Integer -> Double -> Double
> interval _ 1 _  = 1.0
> interval _ 2 _  = 6.0
> interval p _ eF = p * eF

We have a unique advantage in that it's possible for us to determine EF
(easiness factor) of an item through collaboration. However, I'm not sure if
that's necessary or will pay off as much as moving to a newer algorithm.

For now we'll stick to separate EF for each member.

> easinessFactor' :: Double -> Integer -> Double
> easinessFactor' ef q = max 1.3 $ ef + (0.1 - x * (0.08 + x * 0.02))
>     where x :: Double = fromIntegral $ 5 - q

Return the number of seconds after the current review time to schedule the
following review.

This function is responsible for doing any updates to the database that it
needs in order to accurately return the next review interval when that time
comes.

This should return Nothing if the item needs to be repeated immediately.

> reviewInterval :: Integer -> Integer -> Integer -> Double -> App (Maybe Integer)
> reviewInterval memberNo linkNo previous recall = do
>   let p = daysFromSeconds previous
>       q :: Integer = round $ recall * 5 -- The algorithm expects 0-5, not 0-1.
>   c <- asks db
>   stats <- liftIO $ quickQuery c "SELECT n, EF FROM link_sm2 \
>                                  \WHERE member_no = ? AND link_no = ?"
>                                  [toSql memberNo, toSql linkNo]
>                       `catchSqlE` "Failed to determine next review interval."
>   case stats of
>     []          -> if q < 3 -- This item is not yet learned.
>                       then return Nothing
>                       else do let n  = 1
>                                   ef = easinessFactor' 2.5 q
>                               createSM2 memberNo linkNo n ef
>                               return $ Just $ secondsFromDays (interval p n ef)
>     [[n', ef']] -> let ef = fromSql ef'
>                        n :: Integer = fromSql n' in
>                    if q < 3 -- We need to restart the learning process.
>                       then do updateSM2 memberNo linkNo 1 ef
>                               return Nothing
>                       else do let newEF = easinessFactor' ef q
>                                   n''   = n + 1
>                               updateSM2 memberNo linkNo n'' newEF
>                               return $ Just $ secondsFromDays (interval p n'' newEF)
>     _           -> error "Failed to retrieve review data."

> daysFromSeconds :: Integer -> Double
> daysFromSeconds s = (fromIntegral s) / (24 * 60 * 60)

> secondsFromDays :: Double -> Integer
> secondsFromDays d = round $ d * 24 * 60 * 60

> createSM2 :: Integer -> Integer -> Integer -> Double -> App ()
> createSM2 memberNo linkNo n ef = do
>   c <- asks db
>   liftIO $ quickInsert c "INSERT INTO link_sm2 (member_no, link_no, n, EF) \
>                          \VALUES (?, ?, ?, ?)"
>                          [toSql memberNo, toSql linkNo, toSql n, toSql ef]
>              `catchSqlE` "Failed to create an SM-2 record."

> updateSM2 :: Integer -> Integer -> Integer -> Double -> App ()
> updateSM2 memberNo linkNo n ef = do
>   c <- asks db
>   liftIO $ withTransaction c $ \c' ->
>     run c' "UPDATE link_sm2 SET n = ?, EF = ? \
>            \WHERE member_no = ? AND link_no = ?"
>            [toSql n, toSql ef, toSql memberNo, toSql linkNo]
>       `catchSqlE` "Failed to update SM-2 record."
>   return ()