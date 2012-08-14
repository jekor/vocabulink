-- Copyright 2008, 2009, 2010, 2011, 2012 Chris Forno

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

-- | Members of the Site

-- Most functionality on Vocabulink---such as review scheduling---is for
-- registered members only.

module Vocabulink.Member ( UserContent(..)
                         , memberByName, memberByNumber, memberAvatar
                         , withRequiredMember, withRequiredMember'
                         , gravatar, gravatarHash
                         {- Vocabulink.Member.Auth -}
                         , Member(..)
                         ) where

import Vocabulink.App
import Vocabulink.Html
import Vocabulink.Member.Auth
import Vocabulink.Utils

import Data.Default (def)
import Data.Text (pack)
import qualified Network.Gravatar as G
import Text.Regex

-- | Simple user-generated content permissions
class UserContent u where
  canView   :: u -> App Bool
  canEdit   :: u -> App Bool
  canDelete :: u -> App Bool

-- At the time of authentication, we have to fetch the member's number from the
-- database before it can be packed into their auth token. There may be a way
-- to put this step into password verification so that we don't need 2 queries.

memberByNumber :: Integer -- ^ member number
               -> App (Maybe Member)
memberByNumber n = memberFromTuple <$$> $(queryTuple'
  "SELECT member_no, username, email FROM member \
  \WHERE member_no = {n}")

memberByName :: String -- ^ member name
             -> App (Maybe Member)
memberByName n = memberFromTuple <$$> $(queryTuple'
  "SELECT member_no, username, email FROM member \
  \WHERE username = {n}")

memberFromTuple :: (Integer, String, Maybe String) -> Member
memberFromTuple (n, u, e) = Member { memberNumber = n
                                   , memberName   = u
                                   , memberEmail  = e
                                   }

memberAvatar :: Int -- ^ size (square) in pixels
             -> Member
             -> Maybe Html
memberAvatar size' member =
  (a ! href (toValue $ "/user/" ++ memberName member)) . gravatar size' <$> memberEmail member

-- | Only perform the given action if the user is authenticated and has
-- verified their email address. This provides a ``logged out default'' of
-- redirecting the client to the login page.
withRequiredMember :: (Member -> App a) -> App a
withRequiredMember f = do
  member <- asks appMember
  case member of
    Nothing -> error "Please log in."
    Just m  -> case memberEmail m of
                 Nothing -> error "Please verify your email address."
                 Just _  -> f m

withRequiredMember' :: (Member -> App a) -> App a
withRequiredMember' f = do
  member <- asks appMember
  case member of
    Nothing -> error "Please log in."
    Just m  -> f m

gravatar :: Int -- ^ size (square) in pixels
         -> String -- ^ email address
         -> Html
gravatar size' email =
  img ! width (toValue size')
      ! height (toValue size')
      ! class_ "avatar"
      ! src (toValue $ G.gravatar G.GravatarOptions {G.gSize = Just (G.Size size')
                                                    ,G.gDefault = Just G.Wavatar
                                                    ,G.gForceDefault = G.ForceDefault False
                                                    ,G.gRating = Just G.X}
                                  (pack $ map toLower email))

-- The gravatar library will generate the entire URL, but sometimes we just
-- need the hash. Rather than implement the hashing ourselves, we'll dissect
-- the one we receive from the gravatar library.

gravatarHash :: String -> Maybe String
gravatarHash email =
  let url = G.gravatar def (pack email)
      matches = matchRegex (mkRegex "/avatar/([0-9a-f]+)") url in
  case matches of
    Just [hash] -> Just hash
    _           -> Nothing
