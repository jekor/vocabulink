% Copyright 2008, 2009 Chris Forno

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

\section{Review Algorithm SM2}

This is SuperMemo algorithm SM-2
(\url{http://www.supermemo.com/english/ol/sm2.htm})

> module Vocabulink.Review.SM2 (reviewInterval) where

> import Vocabulink.App
> import Vocabulink.DB

Each algorithm needs to export a |reviewInterval| function that accepts some
basic information about a completed review and returns the time (in seconds
from now) to schedule the next review for. |reviewInterval| will be called with
an active transaction, so it must not commit anything to the database (such as
using quickStmt', etc.).

This function is responsible for doing any updates to the database that it
needs in order to accurately return the next review interval when that time
comes.

This should return Nothing if there was an error or 0 if the item needs to be
repeated immediatey.

> reviewInterval :: Integer -> Integer -> Integer -> Double -> App (Maybe Integer)
> reviewInterval memberNo linkNo previous recall = do
>   let p = daysFromSeconds previous
>       q = round $ recall * 5 -- The algorithm expects 0-5, not 0-1.
>   stats <- queryTuples'  "SELECT n, EF FROM link_sm2 \
>                          \WHERE member_no = ? AND link_no = ?"
>                          [toSql memberNo, toSql linkNo]
>   case stats of
>     Just []           -> if q < 3 -- This item is not yet learned.
>                            then return $ Just 0
>                            else do  let  n   = 1
>                                          ef  = easinessFactor' 2.5 q
>                                     createSM2 memberNo linkNo n ef
>                                     return $ Just $
>                                       secondsFromDays (interval p n ef)
>     Just [[n', ef']]  -> let  ef  = fromSql ef'
>                               n   = fromSql n' in
>                          if q < 3 -- We need to restart the learning process.
>                            then do  updateSM2 memberNo linkNo 1 ef
>                                     return $ Just 0
>                            else do  let  newEF  = easinessFactor' ef q
>                                          n''    = n + 1
>                                     updateSM2 memberNo linkNo n'' newEF
>                                     return $ Just $
>                                       secondsFromDays (interval p n'' newEF)
>     _                 -> return Nothing

The algorithm works with days, but we keep track of seconds.

> daysFromSeconds :: Integer -> Double
> daysFromSeconds s = fromIntegral s / (24 * 60 * 60)

> secondsFromDays :: Double -> Integer
> secondsFromDays d = round $ d * 24 * 60 * 60

The following are based on a direct translation of the published algorithm. The
variable names are taken directly from the document.

The first 2 review intervals are fixed. The following are based on the duration
of the previous interval (|p|) and the easiness factor (|ef|).

> interval :: Double -> Integer -> Double -> Double
> interval _ 1 _   = 1.0
> interval _ 2 _   = 6.0
> interval p _ ef  = p * ef

The easiness factor is the core calculation of the SM-2 algorithm.

We have a unique advantage in that it's possible for us to determine the EF
(easiness factor) of an item through collaboration. However, I'm not sure if
that's necessary or will pay off as much as moving to a newer algorithm. But
for now we'll stick to a separate EF for each member.

> easinessFactor' :: Double -> Integer -> Double
> easinessFactor' ef q = max 1.3 $ ef + (0.1 - x * (0.08 + x * 0.02))
>     where x = fromIntegral (5 - q)

For the SM-2 algorithm to work, we need to keep track of a couple variables for
each link. This establishes the variable record in the database the first time
a link is reviewed.

> createSM2 :: Integer -> Integer -> Integer -> Double -> App ()
> createSM2 memberNo linkNo n ef = do
>   run'  "INSERT INTO link_sm2 (member_no, link_no, n, EF) \
>         \VALUES (?, ?, ?, ?)"
>         [toSql memberNo, toSql linkNo, toSql n, toSql ef]
>   return ()

When a link is already being reviewed, this updates the SM2 variables.

> updateSM2 :: Integer -> Integer -> Integer -> Double -> App ()
> updateSM2 memberNo linkNo n ef = do
>   run'  "UPDATE link_sm2 SET n = ?, EF = ? \
>         \WHERE member_no = ? AND link_no = ?"
>         [toSql n, toSql ef, toSql memberNo, toSql linkNo]
>   return ()
