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

module Vocabulink.Link.Frequency ( frequencyLists, addFrequencyList, addFrequency
                                 ) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Member
import Vocabulink.Utils

import qualified Data.Aeson.Generic
import qualified Data.Aeson.Types
import qualified Data.Text

frequencyLists :: String -> App CGIResult
frequencyLists lang = do
  lists <- $(queryTuples' "SELECT list_no, list_name, description \
                          \FROM link_frequency_list \
                          \WHERE lang = {lang}")
  outputJSON $ map freqJSON lists
 where freqJSON (list_no, list_name, description) =
         [aesonQQ| {"number": <| list_no::Integer |>
                   ,"name": <| list_name |>
                   ,"description": <| description |>
                   } |]

addFrequencyList :: String -> App CGIResult
addFrequencyList lang = withRequiredMember $ \m -> do
  -- This is temporarily locked down.
  when (memberNumber m /= 1 && memberNumber m /= 2) $ error "Unauthorized"
  name <- getRequiredInput "name"
  description <- getRequiredInput "description"
  $(execute' "INSERT INTO link_frequency_list (lang, list_name, description) \
                                      \VALUES ({lang}, {name}, {description})")
  outputNothing

addFrequency :: Integer -> Integer -> Integer -> Float -> App ()
addFrequency linkNo listNo rank frequency = do
  $(execute' "INSERT INTO link_frequency (link_no, list_no, rank, frequency) \
                                 \VALUES ({linkNo}, {listNo}, {rank}, {frequency})")