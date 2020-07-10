-- Creating links is great, but viewing a link doesn't mean that you've learned
-- it. We need a way to present links to members for regular (scheduled)
-- reviews.

module Vocabulink.Review ( newReview, scheduleNextReview
                         , reviewStats, dailyReviewStats, detailedReviewStats
                         , reviewPage
                         , syncLinks
                         ) where

-- For now, we have only 1 review algorithm (SuperMemo 2).

import qualified Vocabulink.Review.SM2 as SM2

import Vocabulink.Env
import Vocabulink.Html
import Vocabulink.Link
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

import Control.Exception (catch)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map.Lazy as ML
import Database.PostgreSQL.Typed.ErrCodes (not_null_violation)
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

newReview :: E (Member -> Int32 -> IO ())
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

scheduleNextReview :: E (Member -> Int32 -> Float -> Int32 -> EpochTime -> IO ())
scheduleNextReview m linkNo recallGrade recallTime reviewedAt = do
  previous <- fromJust <$> previousInterval m linkNo
  diff <- SM2.reviewInterval (memberNumber m) linkNo previous recallGrade
  liftIO $ withTransaction ?db $ do
    $(execute
      "INSERT INTO link_review (member_no, link_no, recall_grade, recall_time, actual_time, \
                               \target_time) \
                       \VALUES ({memberNumber m}, {linkNo}, {recallGrade}, {recallTime}, {utcFromEpoch reviewedAt}, \
                               \(SELECT target_time FROM link_to_review \
                                \WHERE member_no = {memberNumber m} AND link_no = {linkNo}))") ?db
    $(execute
      "UPDATE link_to_review \
      \SET target_time = {utcFromEpoch reviewedAt}::timestamp with time zone + {diff}::interval \
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
dueForReview :: E (Member -> String -> String -> Int64 -> IO [Link])
dueForReview m learn' known' n = map (uncurryN Link) <$> $(queryTuples
  "SELECT l.link_no, learn, known, learn_lang, known_lang, soundalike, linkword \
  \FROM link_to_review l \
  \INNER JOIN link USING (link_no) \
  \WHERE member_no = {memberNumber m} AND current_timestamp >= target_time \
    \AND learn_lang = {learn'} AND known_lang = {known'} \
    \AND NOT deleted \
  \ORDER BY added_time ASC \
  \LIMIT {n}") ?db

-- In order to determine the next review interval, the review scheduling
-- algorithm may need to know how long the last review period was (in fact, any
-- algorithm based on spaced reptition will). This returns the actual, not
-- scheduled, amount of time between the current and last review in seconds.

-- Note that this will not work before the link has been reviewed. We expect
-- that the review algorithm does not have to be used for determining the first
-- review (immediate).

previousInterval :: E (Member -> Int32 -> IO (Maybe DiffTime))
previousInterval m linkNo =
  (secondsToDiffTime . (fromIntegral :: Int32 -> Integer) . fromJust) <$$> $(queryTuple
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
  return $ object [ "due" .= (due :: Int64)
                  , "reviews" .= (reviews :: Int64)
                  , "links" .= (links :: Int64)
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
                , "reviewed" .= (fromJust links :: Int64)
                , "recallTime" .= (fromJust recallTime :: Int64)
                ]
       scheduledJSON (day, links) =
         object [ "date" .= (toGregorian $ fromJust day)
                , "scheduled" .= (fromJust links :: Int64)
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
         object [ "linkNumber" .= (linkNo :: Int32)
                , "time" .= (fromJust time :: Int32)
                , "grade" .= (round ((grade :: Double) * 5) :: Int32)
                , "learn" .= (learn :: Text)
                , "known" .= (known :: Text)
                ]
       scheduledJSON (linkNo, time, learn, known) = do
         object [ "linkNumber" .= (linkNo :: Int32)
                , "time" .= (fromJust time :: Int32)
                , "learn" .= (learn :: Text)
                , "known" .= (known :: Text)
                ]

reviewPage :: E (String -> String -> IO Html)
reviewPage learn known = withLoggedInMember $ \m -> do
  -- Send the initial batch of data with this page.
  due <- liftIO $ dueForReview m learn known 100
  let vars = "var review = " ++ (BLU.toString $ encode $ map compactLinkJSON due) ++ ";\n"
          ++ "var learnLanguage = '" ++ languageName learn ++ "';"
          ++ "var knownLanguage = '" ++ languageName known ++ "';"
  stdPage ("Reviewing " ++ languageName learn ++ " Words") [JS "review", CSS "review", JS "link", CSS "link", InlineJS vars] mempty $ do
    div ! id "review-header" $ do
      h2 $ "Loading..."

-- The client sends us a list of links that it thinks it's reviewing. We tell
-- it if we have any it doesn't. If it has some we don't, then we record them
-- server-side.
syncLinks :: E (Member -> [Int32] -> IO ([Int32], [Int32]))
syncLinks m retained = do
  reviewing <- reviewingLinks
  deleted <- reviewingDeleted
  missingLinks <- catMaybes `liftM` (forM (traceShow' $ (retained \\ reviewing) \\ deleted) $ \linkNo -> do
    catch (newReview m linkNo >> return Nothing)
          (\(PGError fields) -> return $ if (ML.!) fields 'C' == not_null_violation
                                           then Just linkNo
                                           else Nothing))
  let toDelete = missingLinks ++ (retained `intersect` deleted)
  return (reviewing \\ retained, toDelete)
 where reviewingLinks = $(queryTuples
         "SELECT l.link_no \
         \FROM link_to_review \
         \INNER JOIN link l USING (link_no) \
         \WHERE member_no = {memberNumber m} \
           \AND NOT deleted") ?db
       reviewingDeleted = $(queryTuples
         "SELECT l.link_no \
         \FROM link_to_review \
         \INNER JOIN link l USING (link_no) \
         \WHERE member_no = {memberNumber m} \
           \AND deleted") ?db
