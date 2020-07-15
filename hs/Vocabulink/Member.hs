-- Most functionality on Vocabulink---such as review scheduling---is for
-- registered members only.

module Vocabulink.Member ( Member(..), AuthToken(..), validAuth, authToken
                         , authCookie, emptyAuthCookie, authShelfLife
                         ) where

import Vocabulink.Utils

import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.ByteString.UTF8 as BU
import Data.Digest.Pure.SHA (hmacSha1)
import Web.Cookie (SetCookie(..))

data Member = Member { memberNumber :: Int32
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

data AuthToken = AuthToken { authMemberNumber  :: Int32
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
authToken :: Int32 -> String -> IO AuthToken
authToken memberNo tokenKey = do
  now <- epochTime
  let expires = now + authShelfLife
      digest = tokenDigest AuthToken { authMemberNumber = memberNo
                                     , authExpiry = expires
                                     , authDigest = ""
                                     } tokenKey
  return AuthToken { authMemberNumber  = memberNo
                   , authExpiry = expires
                   , authDigest = digest
                   }

-- | This generates the HMAC digest of the auth token using SHA1.
-- Eventually, we need to rotate the key used to generate the HMAC, while still
-- storing old keys long enough to use them for any valid login session. Without
-- this, authentication is less secure.
tokenDigest :: AuthToken -> String -> String
tokenDigest a tokenKey = show $ hmacSha1 (BC8.pack tokenKey) (BC8.pack token)
  where token = show (authMemberNumber a) ++ show (authExpiry a)

-- -- Setting the cookie is rather simple by this point. We just create the auth
-- -- token and send it to the client.

authCookie :: Int32 -> String -> IO SetCookie
authCookie memberNo tokenKey = do
  token <- authToken memberNo tokenKey
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

validAuth :: String -> String -> IO (Maybe AuthToken)
validAuth tokenKey token = do
  case readMaybe token of
    Nothing -> return Nothing
    Just t -> do
      now <- epochTime
      if tokenDigest t tokenKey == authDigest t && authExpiry t > now
        then return $ Just t
        else return Nothing
