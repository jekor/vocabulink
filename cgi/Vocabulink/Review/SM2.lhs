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

\section{Review Algorithm SM2}

This is SuperMemo algorithm SM-2
(\url{http://www.supermemo.com/english/ol/sm2.htm})

> module Vocabulink.Review.SM2 (reviewInterval) where

> import Vocabulink.App
> import Vocabulink.Utils

Each algorithm needs to export a |reviewInterval| function that accepts some
basic information about a completed review and returns the time from now to
schedule the next review for. |reviewInterval| will be called with an active
transaction, so it must not commit anything to the database (such as using
quickStmt', etc.).

This function is responsible for doing any updates to the database that it
needs in order to accurately return the next review interval when that time
comes.

This should return Nothing if there was an error or 0 if the item needs to be
repeated immediatey.

> reviewInterval :: Integer -> Integer -> DiffTime -> Float -> App DiffTime
> reviewInterval memberNo linkNo previous recall = do
>   let p = daysFromSeconds $ diffTimeToSeconds previous
>       q = round $ recall * 5 -- The algorithm expects 0-5, not 0-1.
>   stats <- $(queryTuple' "SELECT n, EF FROM link_sm2 \
>                          \WHERE member_no = {memberNo} AND link_no = {linkNo}")
>   secondsToDiffTime <$> case stats of
>     Nothing      -> if q < 3 -- This item is not yet learned.
>                       then return 0
>                       else do let n  = 1
>                                   ef = easinessFactor' 2.5 q
>                               createSM2 memberNo linkNo n ef
>                               return $ secondsFromDays (interval p n ef)
>     Just (n, ef) -> if q < 3 -- We need to restart the learning process.
>                       then do updateSM2 memberNo linkNo 1 ef
>                               return 0
>                       else do let newEF = easinessFactor' ef q
>                                   n'    = n + 1
>                               updateSM2 memberNo linkNo n' newEF
>                               return $ secondsFromDays (interval p n' newEF)

The algorithm works with days, but we keep track of seconds.

> daysFromSeconds :: Integer -> Float
> daysFromSeconds s = fromIntegral s / (24 * 60 * 60)

> secondsFromDays :: Float -> Integer
> secondsFromDays d = round $ d * 24 * 60 * 60

The following are based on a direct translation of the published algorithm. The
variable names are taken directly from the document.

The first 2 review intervals are fixed. The following are based on the duration
of the previous interval (|p|) and the easiness factor (|ef|).

> interval :: Float -> Integer -> Float -> Float
> interval _ 1 _   = 1.0
> interval _ 2 _   = 6.0
> interval p _ ef  = p * ef

The easiness factor is the core calculation of the SM-2 algorithm.

We have a unique advantage in that it's possible for us to determine the EF
(easiness factor) of an item through collaboration. However, I'm not sure if
that's necessary or will pay off as much as moving to a newer algorithm. But
for now we'll stick to a separate EF for each member.

> easinessFactor' :: Float -> Integer -> Float
> easinessFactor' ef q = max 1.3 $ ef + (0.1 - x * (0.08 + x * 0.02))
>     where x = fromIntegral (5 - q)

For the SM-2 algorithm to work, we need to keep track of a couple variables for
each link. This establishes the variable record in the database the first time
a link is reviewed.

> createSM2 :: Integer -> Integer -> Integer -> Float -> App ()
> createSM2 memberNo linkNo n ef = $(execute'
>   "INSERT INTO link_sm2 (member_no, link_no, n, EF) \
>                 \VALUES ({memberNo}, {linkNo}, {n}, {ef})")

When a link is already being reviewed, this updates the SM2 variables.

> updateSM2 :: Integer -> Integer -> Integer -> Float -> App ()
> updateSM2 memberNo linkNo n ef = $(execute'
>   "UPDATE link_sm2 SET n = {n}, EF = {ef} \
>   \WHERE member_no = {memberNo} AND link_no = {linkNo}")
