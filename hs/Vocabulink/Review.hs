-- Copyright 2008, 2009, 2010, 2011, 2012, 2013 Chris Forno

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

module Vocabulink.Review ( newReview, scheduleNextReview
                         , reviewStats, dailyReviewStats, detailedReviewStats
                         , learnPage, upcomingLinks
                         , ClientLinkSync(..), syncLinks
                         ) where

-- For now, we have only 1 review algorithm (SuperMemo 2).

import qualified Vocabulink.Review.SM2 as SM2

import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Time.Calendar (toGregorian)

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

newReview :: E (Member -> Integer -> IO ())
newReview m linkNo = insertIgnore $
  $(execute "INSERT INTO link_to_review (member_no, link_no) \
                                \VALUES ({memberNumber m}, {linkNo})") ?db

-- We need to schedule the next review based on the review algorithm in use.
-- The algorithm needs to know how well the item was remembered. Also, we log
-- the amount of time it took to recall the item. The SM2 algorithm does not
-- use this information (nor any SuperMemo algorithm that I know of), but we
-- may find it useful when analyzing data later.

-- @recallGrade@ is passed as a real number between 0 and 1 to allow for future
-- variations in recall rating (such as fewer choices than 1 through 5 or less
-- discrete options like a slider). 0 indicates complete failure while 1
-- indicates perfect recall.

-- @recallTime@ is the time in milliseconds.

-- All database updates during this process are wrapped in a transaction.

scheduleNextReview :: E (Member -> Integer -> Float -> Integer -> EpochTime -> IO ())
scheduleNextReview m linkNo recallGrade recallTime reviewedAt = do
  previous <- fromJust <$> previousInterval m linkNo
  diff <- SM2.reviewInterval (memberNumber m) linkNo previous recallGrade
  liftIO $ withTransaction ?db $ do
    $(execute
      "INSERT INTO link_review (member_no, link_no, recall_grade, recall_time, actual_time, \
                               \target_time) \
                       \VALUES ({memberNumber m}, {linkNo}, {recallGrade}, {recallTime}, {reviewedAt}, \
                               \(SELECT target_time FROM link_to_review \
                                \WHERE member_no = {memberNumber m} AND link_no = {linkNo}))") ?db
    $(execute
      "UPDATE link_to_review \
      \SET target_time = {reviewedAt}::timestamp with time zone + {diff}::interval \
      \WHERE member_no = {memberNumber m} AND link_no = {linkNo}") ?db

-- There are at least 2 ways to decide which links should be brought up for
-- review first:
--
-- The method that Vocabulink used originally was taking the ones with the
-- oldest target time. They're the ones that have been waiting longest for
-- review, so we should review them first, right? Well, the problem is that you
-- get into a case when too many links are due for review where you spend
-- longer going through and reviewing all the due links before returning to the
-- beginning of the list and starting over again. If the list of links due for
-- review is long enough, it could take weeks to get through the list and a
-- link that was only waiting a few days for review will have been forgotten by
-- the time we get around to it. This breaks the whole principle of spaced
-- repetition as soon as you fall behind enough.

-- The method that Vocabulink now uses is to prioritize links based on when you
-- started learning them. The links that you started learning longest ago come
-- up for review with high priority as soon as they're due. Links you began
-- learning later have to wait until there's time. This way, if the learner
-- gets backed up, at least they're maintaining the links that they've been
-- spending the longest on (and are presumably the most familiar with). This
-- should also have the side effect of reducing the size of the queue faster
-- because links that the learner has been studying longer are likely to have
-- bigger payoff (time until the next review date) if they nail them.
dueForReview :: E (Member -> String -> String -> Int -> IO [Link])
dueForReview m learn' known' n = map (uncurryN Link) <$> $(queryTuples
  "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
  \FROM link_to_review l \
  \INNER JOIN link USING (link_no) \
  \WHERE member_no = {memberNumber m} AND current_timestamp >= target_time \
    \AND learn_lang = {learn'} AND known_lang = {known'} \
    \AND NOT deleted \
  \ORDER BY added_time ASC \
  \LIMIT {n}") ?db

-- First, use words with existing stories. Second, use linkwords or
-- soundalikes. Finally, use whatever.
newForReview :: E (String -> String -> Int -> IO [Link])
newForReview learn' known' n =
  case ?member of
    Nothing -> do
      storied <- map (uncurryN Link) <$> $(queryTuples
        "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
        \FROM link l \
        \INNER JOIN linkword_story ss ON (ss.link_no = l.link_no) \
        \WHERE learn_lang = {learn'} AND known_lang = {known'} \
          \AND NOT deleted \
        \ORDER BY random() LIMIT {n}") ?db
      if length storied >= n
        then return storied
        else do
          special <- map (uncurryN Link) <$> $(queryTuples
            "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
            \FROM link l \
            \LEFT JOIN linkword_story ss ON (ss.link_no = l.link_no) \
            \WHERE learn_lang = {learn'} AND known_lang = {known'} \
              \AND ss.link_no IS NULL \
              \AND (soundalike OR linkword IS NOT NULL) \
              \AND NOT deleted \
            \ORDER BY random() LIMIT {n - length storied}") ?db
          if length storied + length special >= n
            then return $ storied ++ special
            else do
              plain <- map (uncurryN Link) <$> $(queryTuples
                "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
                \FROM link l \
                \WHERE learn_lang = {learn'} AND known_lang = {known'} \
                  \AND (NOT soundalike AND linkword IS NULL) \
                  \AND NOT deleted \
                \ORDER BY random() LIMIT {n - length storied - length special}") ?db
              return $ storied ++ special ++ plain
    Just m -> do
      storied <- map (uncurryN Link) <$> $(queryTuples
        "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
        \FROM link l \
        \LEFT JOIN link_to_review r ON (r.link_no = l.link_no AND r.member_no = {memberNumber m}) \
        \INNER JOIN linkword_story ss ON (ss.link_no = l.link_no) \
        \WHERE learn_lang = {learn'} AND known_lang = {known'} \
          \AND r.link_no IS NULL \
          \AND NOT deleted \
        \ORDER BY random() LIMIT {n}") ?db
      if length storied >= n
        then return storied
        else do
          special <- map (uncurryN Link) <$> $(queryTuples
            "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
            \FROM link l \
            \LEFT JOIN link_to_review r ON (r.link_no = l.link_no AND r.member_no = {memberNumber m}) \
            \LEFT JOIN linkword_story ss ON (ss.link_no = l.link_no) \
            \WHERE learn_lang = {learn'} AND known_lang = {known'} \
              \AND r.link_no IS NULL AND ss.link_no IS NULL \
              \AND (soundalike OR linkword IS NOT NULL) \
              \AND NOT deleted \
            \ORDER BY random() LIMIT {n - length storied}") ?db
          if length storied + length special >= n
            then return $ storied ++ special
            else do
              plain <- map (uncurryN Link) <$> $(queryTuples
                "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
                \FROM link l \
                \LEFT JOIN link_to_review r ON (r.link_no = l.link_no AND r.member_no = {memberNumber m}) \
                \WHERE learn_lang = {learn'} AND known_lang = {known'} \
                  \AND r.link_no IS NULL \
                  \AND (NOT soundalike AND linkword IS NULL) \
                  \AND NOT deleted \
                \ORDER BY random() LIMIT {n - length storied - length special}") ?db
              return $ storied ++ special ++ plain

-- In order to determine the next review interval, the review scheduling
-- algorithm may need to know how long the last review period was (in fact, any
-- algorithm based on spaced reptition will). This returns the actual, not
-- scheduled, amount of time between the current and last review in seconds.

-- Note that this will not work before the link has been reviewed. We expect
-- that the review algorithm does not have to be used for determining the first
-- review (immediate).

previousInterval :: E (Member -> Integer -> IO (Maybe DiffTime))
previousInterval m linkNo =
  (secondsToDiffTime . fromJust) <$$> $(queryTuple
    "SELECT COALESCE(extract(epoch from current_timestamp - \
                            \(SELECT actual_time FROM link_review \
                             \WHERE member_no = {memberNumber m} AND link_no = {linkNo} \
                             \ORDER BY actual_time DESC LIMIT 1))::int, \
                    \extract(epoch from current_timestamp - \
                            \(SELECT target_time FROM link_to_review \
                             \WHERE member_no = {memberNumber m} AND link_no = {linkNo}))::int)") ?db

reviewStats :: E (Member -> IO Value)
reviewStats m = do
  due <- fromJust . fromJust <$> $(queryTuple
    "SELECT COUNT(*) FROM link_to_review \
    \WHERE member_no = {memberNumber m} AND current_timestamp > target_time") ?db
  reviews <- fromJust . fromJust <$> $(queryTuple
    "SELECT COUNT(*) FROM link_review \
    \WHERE member_no = {memberNumber m}") ?db
  links <- fromJust . fromJust <$> $(queryTuple
    "SELECT COUNT(*) FROM link_to_review \
    \WHERE member_no = {memberNumber m}") ?db
  return $ object [ "due" .= (due :: Integer)
                  , "reviews" .= (reviews :: Integer)
                  , "links" .= (links :: Integer)
                  ]

dailyReviewStats :: E (Member -> Day -> Day -> String -> IO [Value])
dailyReviewStats m start end tzOffset = do
  reviews <- $(queryTuples
    "SELECT (actual_time AT TIME ZONE {tzOffset})::date AS day, COUNT(DISTINCT link_no), SUM(recall_time) \
    \FROM link_review \
    \WHERE member_no = {memberNumber m} \
      \AND (actual_time AT TIME ZONE {tzOffset})::date BETWEEN {start}::date AND {end}::date \
    \GROUP BY day \
    \ORDER BY day") ?db
  scheduled <- $(queryTuples
    "SELECT (target_time AT TIME ZONE {tzOffset})::date AS day, COUNT(DISTINCT link_no) \
    \FROM link_to_review \
    \WHERE member_no = {memberNumber m} \
      \AND (target_time AT TIME ZONE {tzOffset})::date BETWEEN {start}::date AND {end}::date \
    \GROUP BY day \
    \ORDER BY day") ?db
  return $ map reviewJSON reviews ++ map scheduledJSON scheduled
 where reviewJSON (day, links, recallTime) =
         object [ "date" .= (toGregorian $ fromJust day)
                , "reviewed" .= (fromJust links ::Integer)
                , "recallTime" .= (fromJust recallTime :: Integer)
                ]
       scheduledJSON (day, links) =
         object [ "date" .= (toGregorian $ fromJust day)
                , "scheduled" .= (fromJust links :: Integer)
                ]

detailedReviewStats :: E (Member -> Day -> Day -> String -> IO Value)
detailedReviewStats m start end tzOffset = do
  reviews <- $(queryTuples
    "SELECT link_no, COALESCE(extract(epoch from actual_time))::int, recall_grade, \
           \learn, known \
    \FROM link_review \
    \INNER JOIN link USING (link_no) \
    \WHERE member_no = {memberNumber m} \
      \AND (actual_time AT TIME ZONE {tzOffset})::date BETWEEN {start}::date AND {end}::date \
    \ORDER BY actual_time") ?db
  scheduled <- $(queryTuples
    "SELECT link_no, COALESCE(extract(epoch from target_time))::int, \
           \learn, known \
    \FROM link_to_review \
    \INNER JOIN link USING (link_no) \
    \WHERE member_no = {memberNumber m} \
      \AND (target_time AT TIME ZONE {tzOffset})::date BETWEEN {start}::date AND {end}::date \
    \ORDER BY target_time") ?db
  return $ object ["reviewed" .= map reviewJSON reviews, "scheduled" .= map scheduledJSON scheduled]
 where reviewJSON (linkNo, time, grade, learn, known) =
         object [ "linkNumber" .= (linkNo :: Integer)
                , "time" .= (fromJust time :: Integer)
                , "grade" .= (round (grade * 5) :: Integer)
                , "learn" .= learn
                , "known" .= known
                ]
       scheduledJSON (linkNo, time, learn, known) = do
         object [ "linkNumber" .= (linkNo :: Integer)
                , "time" .= (fromJust time :: Integer)
                , "learn" .= learn
                , "known" .= known
                ]

learnPage :: E (String -> String -> IO Html)
learnPage learn known = do
  -- Send the initial batch of data with this page.
  due <- liftIO $ maybe (return []) (\m -> dueForReview m learn known 10) ?member
  new <- liftIO $ newForReview learn known (10 - length due)
  let learnLang = fromMaybe "Unknown Language" $ lookup learn languages
      knownLang = fromMaybe "Unknown Language" $ lookup known languages
      vars = "var review = " ++ (BLU.toString $ encode $ map compactLinkJSON due) ++ ";\n"
          ++ "var learn = " ++ (BLU.toString $ encode $ map compactLinkJSON new) ++ ";"
          ++ "var learnLanguage = '" ++ learnLang ++ "';"
          ++ "var knownLanguage = '" ++ knownLang ++ "';"
  return $ stdPage ("Learning " ++ learnLang ++ " Words") [JS "learn", CSS "learn", JS "link", CSS "link", InlineJS vars] mempty $ do
    div ! id "learn-header" $ do
      h2 $ "Loading..."
      when (isNothing ?member) $ div ! id "signup-invitation" $ do
        h1 $ "Join the Free Beta"
        sprite "icon" "wizard"
        unordList [ "Learn More Words"
                  , "Get Regular Review"
                  , "Track Your Progress"
                  ]
        p $ do
          "During the beta everything is free."
          br
          "This won't last forever. Join now."

upcomingLinks :: E (String -> String -> Int -> IO Value)
upcomingLinks learn known n = do
  due <- maybe (return []) (\ m -> dueForReview m learn known n) ?member
  new <- newForReview learn known (n - length due)
  return $ object ["review" .= map compactLinkJSON due, "learn" .= map compactLinkJSON new]

data ClientLinkSync = ClientLinkSync { clientRetain :: [Integer] }

$(deriveFromJSON ((\(x:xs) -> (toLower x):xs) . drop 6) ''ClientLinkSync)

syncLinks :: E (Member -> [Integer] -> IO Value)
syncLinks m retained = do
  reviewing <- reviewingLinks
  forM_ (retained \\ (map linkNumber reviewing)) $ newReview m
  let unretained = map compactLink $ filter (\ l -> not $ linkNumber l `elem` retained) reviewing
  return $ object ["unretained" .= unretained]
 where compactLink l = (show $ linkNumber l, ((linkLearn l, linkLearnLang l), (linkKnown l, linkKnownLang l), linkSoundalike l, linkWord l))
       reviewingLinks = map (uncurryN Link) <$> $(queryTuples
         "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
         \FROM link_to_review \
         \INNER JOIN link l USING (link_no) \
         \WHERE member_no = {memberNumber m}") ?db
