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

-- | Member Page

module Vocabulink.Member.Html (memberPage) where

import Vocabulink.App
import Vocabulink.CGI
import Vocabulink.Member
import Vocabulink.Page
import Vocabulink.Utils

memberPage :: String -> App CGIResult
memberPage username = do
  member <- memberByName username
  case member of
    Nothing -> outputNotFound
    Just m  -> do
      stdPage (memberName m ++ "'s Page") [] mempty $ do
        mempty