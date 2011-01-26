-- Copyright 2011 Chris Forno

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

-- | System-level metrics (not individual)

module Vocabulink.Metrics (metricsPage) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Html
import Vocabulink.Page
import Vocabulink.Utils

import Prelude hiding (div, id, span)

-- | Display various high-level system metrics.
metricsPage :: App CGIResult
metricsPage = do
  today <- liftIO currentDay
  let start = addDays (-30) today
      end   = today
  signups  <- signupCounts start end
  links    <- linkCounts start end
  stories  <- storyCounts start end
  comments <- commentCounts start end
  simplePage "Metrics" [JS "lib.raphael", JS "metrics", CSS "metrics"] $ do
    h2 "Sign Ups"
    dateSeriesChart signups
      ! customAttribute "start" (stringValue $ showGregorian start)
      ! customAttribute "end"   (stringValue $ showGregorian end)
    h2 "Links"
    dateSeriesChart links
      ! customAttribute "start" (stringValue $ showGregorian start)
      ! customAttribute "end"   (stringValue $ showGregorian end)
    h2 "Stories"
    dateSeriesChart stories
      ! customAttribute "start" (stringValue $ showGregorian start)
      ! customAttribute "end"   (stringValue $ showGregorian end)
    h2 "Comments"
    dateSeriesChart comments
      ! customAttribute "start" (stringValue $ showGregorian start)
      ! customAttribute "end"   (stringValue $ showGregorian end)

dateSeriesChart :: [(Day, Integer)] -> Html
dateSeriesChart data' =
  table ! class_ "date-series" $ do
    thead $ tr $ do
      th "Date"
      th "Value"
    tbody $ mconcat $ map row data'
 where row datum = tr $ do
                     td $ string $ show $ fst datum
                     td $ string $ show $ snd datum

-- | # of signups each day in a given date range
signupCounts :: Day -- ^ start date
             -> Day -- ^ end date
             -> App [(Day, Integer)] -- ^ counts for each date
signupCounts start end = (\(d, i) -> (fromJust d, fromJust i)) <$$> $(queryTuples'
  "SELECT CAST(join_date AS date), COUNT(*) FROM member \
  \WHERE CAST(join_date AS date) BETWEEN {start} AND {end} \
  \GROUP BY CAST(join_date AS date) \
  \ORDER BY join_date")

linkCounts :: Day -- ^ start date
           -> Day -- ^ end date
           -> App [(Day, Integer)] -- ^ counts for each date
linkCounts start end = (\(d, i) -> (fromJust d, fromJust i)) <$$> $(queryTuples'
  "SELECT CAST(created AS date), COUNT(*) FROM link \
  \WHERE CAST(created AS date) BETWEEN {start} AND {end} \
  \GROUP BY CAST(created AS date) \
  \ORDER BY created")

storyCounts :: Day -- ^ start date
            -> Day -- ^ end date
            -> App [(Day, Integer)] -- ^ counts for each date
storyCounts start end = (\(d, i) -> (fromJust d, fromJust i)) <$$> $(queryTuples'
  "SELECT CAST(created AS date), COUNT(*) FROM linkword_story \
  \WHERE CAST(created AS date) BETWEEN {start} AND {end} \
  \GROUP BY CAST(created AS date) \
  \ORDER BY created")

commentCounts :: Day -- ^ start date
              -> Day -- ^ end date
              -> App [(Day, Integer)] -- ^ counts for each date
commentCounts start end = (\(d, i) -> (fromJust d, fromJust i)) <$$> $(queryTuples'
  "SELECT CAST(time AS date), COUNT(*) FROM comment \
  \WHERE CAST(time AS date) BETWEEN {start} AND {end} \
  \GROUP BY CAST(time AS date) \
  \ORDER BY time")