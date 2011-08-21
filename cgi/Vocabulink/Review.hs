-- Copyright 2008, 2009, 2010, 2011 Chris Forno

-- This file is part of Vocabulink.

-- Vocabulink is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- Vocabulink is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

-- Creating links is great, but viewing a link doesn't mean that you've learned
-- it. We need a way to present links to members for regular (scheduled)
-- reviews.

module Vocabulink.Review (newReview, linkReviewed, nextReview, reviewStats, reviewPage) where

-- For now, we have only 1 review algorithm (SuperMemo 2).

import qualified Vocabulink.Review.SM2 as SM2

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Page
import Vocabulink.Link
import Vocabulink.Link.Pronunciation
import Vocabulink.Member.Auth
import Vocabulink.Utils

import qualified Data.Aeson.Generic
import qualified Data.Aeson.Types
import qualified Data.Text

import Prelude hiding (div, span, id)

-- Review Scheduling

-- When a member indicates that they want to review a link, we just add it to
-- the @link_to_review@ relation. This may change if we ever support multiple
-- review sets. The link review time is set to the current time by default so
-- that it immediately shows up for review (something we want no matter which
-- algorithm we're using).

-- We need to give the user feedback that they've successfully added the link
-- to their review set. For now, we redirect them back to the referrer because
-- we assume it will be the link page (which will then indicate in some way
-- that they're reviewing the link now). However, this is a good candidate for
-- an asynchronous JavaScript call.

newReview :: Member -> Integer -> App ()
newReview member linkNo = do
  $(execute' "INSERT INTO link_to_review (member_no, link_no) \
                                 \VALUES ({memberNumber member}, {linkNo})")

-- The client indicates a completed review with a @POST@ to
-- @/review/linknumber@ which will be dispatched to |linkReviewed|. Once we
-- schedule the next review time for the link, we move on to the next in their
-- set.

linkReviewed :: Member -> Integer -> App ()
linkReviewed member linkNo = do
  grade <- readRequiredInput "grade"
  time  <- readRequiredInput "time"
  scheduleNextReview member linkNo grade time

-- We need to schedule the next review based on the review algorithm in use.
-- The algorithm needs to know how well the item was remembered. Also, we log
-- the amount of time it took to recall the item. The SM2 algorithm does not
-- use this information (nor any SuperMemo algorithm that I know of), but we
-- may find it useful when analyzing data later.

-- @recallGrade@ is passed as a real number between 0 and 1 to allow for future
-- variations in recall rating (such as fewer choices than 1 through 5 or less
-- discrete options like a slider). 0 indicates complete failure while 1
-- indicates perfect recall.

-- All database updates during this process are wrapped in a transaction.

scheduleNextReview :: Member -> Integer -> Float -> Integer -> App ()
scheduleNextReview member linkNo recallGrade recallTime = do
  previous <- fromJust <$> previousInterval member linkNo
  diff <- SM2.reviewInterval (memberNumber member) linkNo previous recallGrade
  h <- asks appDB
  liftIO $ withTransaction h $ do
    $(execute
      "INSERT INTO link_review (member_no, link_no, recall_grade, recall_time, \
                               \target_time) \
                       \VALUES ({memberNumber member}, {linkNo}, {recallGrade}, {recallTime}, \
                               \(SELECT target_time FROM link_to_review \
                                \WHERE member_no = {memberNumber member} AND link_no = {linkNo}))") h
    $(execute
      "UPDATE link_to_review \
      \SET target_time = current_timestamp + {diff} \
      \WHERE member_no = {memberNumber member} AND link_no = {linkNo}") h

-- Review Pages

-- Here's the entry point for the client to request reviews. It's pretty
-- simple: we just request the next link from @link_to_review@ by
-- @target_time@. If there's none, we send the member to a ``congratulations''
-- page. If there is a link for review, we send them to the review page.

nextReview :: Member -> Integer -> App CGIResult
nextReview member n = do
  rows <- $(queryTuples'
    "SELECT link_no FROM link_to_review \
    \WHERE member_no = {memberNumber member} AND current_timestamp >= target_time \
    \ORDER BY target_time ASC LIMIT {n}")
  case rows of
    [] -> outputNotFound
    -- TODO: Make a version of getLink that can retrieve multiple links at once.
    _  -> outputJSON =<< mapM linkJSON =<< (catMaybes <$> mapM getLink rows)
 where linkJSON l = do
         ol <- linkOriginLanguage l
         dl <- linkDestinationLanguage l
         pr <- pronounceable $ linkNumber l
         return [aesonQQ| {"linkNumber": <| linkNumber l |>
                          ,"foreign": <| linkOrigin l |>
                          ,"foreignLang": <| linkOriginLang l |>
                          ,"foreignLanguage": <| ol |>
                          ,"familiar": <| linkDestination l |>
                          ,"familiarLang": <| linkDestinationLang l |>
                          ,"familiarLanguage": <| dl |>
                          ,"linkType": <| linkTypeNameFromType $ linkType l |>
                          ,"linkword": <| toJSON $ linkWord l |>
                          ,"pronunciation": <| pr |>
                          } |]

-- In order to determine the next review interval, the review scheduling
-- algorithm may need to know how long the last review period was (in fact, any
-- algorithm based on spaced reptition will). This returns the actual, not
-- scheduled, amount of time between the current and last review in seconds.

-- Note that this will not work before the link has been reviewed. We expect
-- that the review algorithm does not have to be used for determining the first
-- review (immediate).

previousInterval :: Member -> Integer -> App (Maybe DiffTime)
previousInterval member linkNo = do
  t <- $(queryTuple'
    "SELECT COALESCE(extract(epoch from current_timestamp - \
                            \(SELECT actual_time FROM link_review \
                             \WHERE member_no = {memberNumber member} AND link_no = {linkNo} \
                             \ORDER BY actual_time DESC LIMIT 1))::int, \
                    \extract(epoch from current_timestamp - \
                            \(SELECT target_time FROM link_to_review \
                             \WHERE member_no = {memberNumber member} AND link_no = {linkNo}))::int)")
  case t of
    Nothing -> return Nothing
    Just t' -> return $ liftM secondsToDiffTime t'

reviewStats :: Member -> App CGIResult
reviewStats member = do
  due <- fromJust . fromJust <$> $(queryTuple'
    "SELECT COUNT(*) FROM link_to_review \
    \WHERE member_no = {memberNumber member} AND current_timestamp > target_time")
  reviews <- fromJust . fromJust <$> $(queryTuple'
    "SELECT COUNT(*) FROM link_review \
    \WHERE member_no = {memberNumber member}")
  links <- fromJust . fromJust <$> $(queryTuple'
    "SELECT COUNT(*) FROM link_to_review \
    \WHERE member_no = {memberNumber member}")
  outputJSON [aesonQQ| {"due": <| due::Integer |>
                       ,"reviews": <| reviews::Integer |>
                       ,"links": <| links::Integer |>} |]

reviewPage :: App CGIResult
reviewPage = stdPage "Review Your Links" [JS "review", CSS "review", CSS "link-common"] mempty mempty
