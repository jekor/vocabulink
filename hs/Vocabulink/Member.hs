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

-- | Members of the Site

-- Most functionality on Vocabulink---such as review scheduling---is for
-- registered members only.

module Vocabulink.Member ( Member(..), AuthToken(..), validAuth, authToken
                         , authCookie, emptyAuthCookie, authShelfLife
                         ) where

import Vocabulink.Utils

import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.ByteString.UTF8 as BU
import Data.Digest.Pure.SHA (hmacSha1, showDigest)
import Language.Haskell.TH.Syntax (runIO, Exp(..), Lit(..))
import System.Environment (getEnv)
import Web.Cookie (SetCookie(..))

data Member = Member { memberNumber :: Integer
                     , memberName   :: String
                     , memberEmail  :: Maybe String
                     } deriving (Eq, Show)

instance ToJSON Member where
  toJSON m = object [ "name" .= (memberName m)
                    , "hash" .= (gravatarHash `liftM` memberEmail m)
                    ]

-- Creating the Auth Token

-- Each time a member logs in, we send an authentication cookie to their
-- browser. The cookie is a digest of some state information. We then use the
-- cookie for authenticating their identity on subsequent requests.
 
-- for generating auth token cookies
-- This is compiled in via Template Haskell to keep the key out of git.
authTokenKey :: String
authTokenKey = $((LitE . StringL) `liftM` runIO (getEnv "auth_token_key"))

data AuthToken = AuthToken { authMemberNumber  :: Integer
                           , authExpiry        :: EpochTime
                           , authDigest        :: String -- HMAC hash
                           }

-- We give the token 30 days to expire. We don't want it expiring too soon
-- because it becomes bothersome to keep logging in. However, we don't want
-- members to stay logged in forever, and using a non-expiring cookie would
-- require us to check the database for each authentication to see if we need
-- to de-authenticate the client.

authShelfLife :: EpochTime
authShelfLife = 30 * 24 * 60 * 60

-- Here is the format of the actual cookie we send to the client.

instance Show AuthToken where
  show a = show (authMemberNumber a) ++ "." ++ show (authExpiry a) ++ "." ++ (authDigest a)

instance Read AuthToken where
  readsPrec _ =
    \s -> case splitOn "." s of
            [m,e,d] -> [(AuthToken (read m) (read e) d, "")]
            _ -> []                    

-- | Create an AuthToken with the default expiration time, automatically
-- calculating the digest.
authToken :: Integer -> IO AuthToken
authToken memberNo = do
  now <- epochTime
  let expires = now + authShelfLife
      digest = tokenDigest AuthToken { authMemberNumber = memberNo
                                     , authExpiry = expires
                                     , authDigest = ""
                                     }
  return AuthToken { authMemberNumber  = memberNo
                   , authExpiry = expires
                   , authDigest = digest
                   }

-- | This generates the HMAC digest of the auth token using SHA1.
-- Eventually, we need to rotate the key used to generate the HMAC, while still
-- storing old keys long enough to use them for any valid login session. Without
-- this, authentication is less secure.
tokenDigest :: AuthToken -> String
tokenDigest a = showDigest $ hmacSha1 (BC8.pack authTokenKey) (BC8.pack token)
  where token = show (authMemberNumber a) ++ show (authExpiry a)

-- -- Setting the cookie is rather simple by this point. We just create the auth
-- -- token and send it to the client.

authCookie :: Integer -> IO SetCookie
authCookie memberNo = do
  token <- authToken memberNo
  return $ emptyAuthCookie { setCookieValue  = BU.fromString $ show token
                           , setCookieMaxAge = Just $ secondsToDiffTime $ convert authShelfLife
                           }

emptyAuthCookie :: SetCookie
emptyAuthCookie =
  def { setCookieName     = "auth"
      , setCookieValue    = ""
      , setCookieDomain   = Just "www.vocabulink.com"
      , setCookiePath     = Just "/"
      , setCookieHttpOnly = True
      }

-- Reading the Auth Token

-- This retrieves the auth token from the HTTP request, verifies it, and if
-- valid, returns it. To verify an auth token, we verify the token digest and
-- check that the cookie hasn't expired.

validAuth :: String -> IO (Maybe AuthToken)
validAuth token = do
  case readMaybe token of
    Nothing -> return Nothing
    Just t -> do
      now <- epochTime
      if tokenDigest t == authDigest t && authExpiry t > now
        then return $ Just t
        else return Nothing
