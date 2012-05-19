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

-- | Member Authentication

-- Much of what you can do on Vocabulink requires us to know and trust who you
-- say you are.

module Vocabulink.Member.Auth ( Member(..), AuthToken(..)
                              , verifiedAuthToken, authToken
                              , setAuthCookie, deleteAuthCookie
                              ) where

import Vocabulink.CGI
import Vocabulink.Utils hiding ((<$$>))

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (hmacSha1, showDigest)
import Data.Time.Format (parseTime)
import System.Locale (iso8601DateFormat)
import System.Time (TimeDiff(..), getClockTime, addToClockTime, toCalendarTime)
import Network.CGI.Cookie (showCookie)
import Network.URI (unEscapeString)
import Text.ParserCombinators.Parsec (Parser, parse, noneOf, many1, try, char, string, optional)
import Text.ParserCombinators.Parsec.Perm ((<$$>), (<||>), permute)

data Member = Member { memberNumber :: Integer
                     , memberName   :: String
                     , memberEmail  :: Maybe String
                     }

-- Creating the Auth Token

-- Each time a member logs in, we send an authentication cookie to their
-- browser. The cookie is a digest of some state information. We then use the
-- cookie for authenticating their identity on subsequent requests.

data AuthToken = AuthToken { authExpiry    :: Day
                           , authMemberNo  :: Integer
                           , authUsername  :: String
                           , authDigest    :: String
                           }

-- We give the token 30 days to expire. We don't want it expiring too soon
-- because it becomes bothersome to keep logging in. However, we don't want
-- members to stay logged in forever, and using a non-expiring cookie would
-- require us to check the database for each authentication to see if we need
-- to de-authenticate the client.

cookieShelfLife :: Integer
cookieShelfLife = 30

-- Here is the format of the actual cookie we send to the client.

instance Show AuthToken where
  show a = "exp="   ++ showGregorian (authExpiry a)
        ++ "&no="   ++ show (authMemberNo a)
        ++ "&name=" ++ escapeURIString' (encodeString $ authUsername a)
        ++ "&mac="  ++ authDigest a

-- | Create an AuthToken with the default expiration time, automatically
-- calculating the digest.
authToken :: Integer -- ^ member number
          -> String -- ^ username
          -> String -- ^ auth token key (from the config file)
          -> IO AuthToken
authToken memberNo username key = do
  now <- currentDay
  let expires = addDays cookieShelfLife now
      digest  = tokenDigest AuthToken { authExpiry    = expires
                                      , authMemberNo  = memberNo
                                      , authUsername  = username
                                      , authDigest    = ""
                                      } key
  return AuthToken { authExpiry    = expires
                   , authMemberNo  = memberNo
                   , authUsername  = username
                   , authDigest    = digest
                   }

-- | This generates the HMAC digest of the auth token using SHA1.
-- Eventually, we need to rotate the key used to generate the HMAC, while still
-- storing old keys long enough to use them for any valid login session. Without
-- this, authentication is less secure.
tokenDigest :: AuthToken
            -> String -- ^ auth token key (from the config file)
            -> String
tokenDigest a key = showDigest $ hmacSha1 (pack key) (pack token)
  where token = showGregorian (authExpiry a)
             ++ show (authMemberNo a)
             ++ encodeString (authUsername a)

-- Setting the cookie is rather simple by this point. We just create the auth
-- token and send it to the client.

setAuthCookie :: (MonadCGI m, MonadIO m) => AuthToken -> m ()
setAuthCookie authTok = do
  now <- liftIO getClockTime
  expires <- liftIO $ toCalendarTime
               (addToClockTime TimeDiff { tdYear    = 0
                                        , tdMonth   = 0
                                        , tdDay     = fromIntegral cookieShelfLife
                                        , tdHour    = 0
                                        , tdMin     = 0
                                        , tdSec     = 0
                                        , tdPicosec = 0
                                        } now)
  let cookie = Cookie { cookieName    = "auth"
                      , cookieValue   = show authTok
                      , cookieExpires = Just expires
                      , cookieDomain  = Just "www.vocabulink.com"
                      , cookiePath    = Just "/"
                      , cookieSecure  = False
                      }
  setHeader "Set-Cookie" $ showCookie cookie ++ "; HttpOnly"

deleteAuthCookie :: MonadCGI m => m ()
deleteAuthCookie =
  deleteCookie Cookie { cookieName    = "auth"
                      , cookieDomain  = Just "www.vocabulink.com"
                      , cookiePath    = Just "/"
                      , cookieValue   = ""
                      , cookieExpires = Nothing
                      , cookieSecure  = False
                      }

-- Reading the Auth Token

-- This retrieves the auth token from the HTTP request, verifies it, and if
-- valid, returns it. To verify an auth token, we verify the token digest and
-- check that the cookie hasn't expired.

verifiedAuthToken :: (MonadCGI m, MonadIO m) => String -> m (Maybe AuthToken)
verifiedAuthToken key = do
  cookie <- getCookie "auth"
  case parseAuthToken =<< cookie of
    Nothing -> return Nothing
    Just a  -> do
      now <- liftIO currentDay
      let digest = tokenDigest a key
      if digest == authDigest a && diffDays (authExpiry a) now > 0
        then return $ Just a
        else return Nothing

-- This is a Parsec parser for auth tokens (as stored in cookies). An auth
-- token looks like:
-- exp=2009-12-01&no=1&name=jekor&mac=d0170bc011b6260a1de7596bad1cd4de

authTokenParser :: Parser AuthToken
authTokenParser = permute
  (mkAuthToken <$$> authFrag "exp"
               <||> authFrag "no"
               <||> authFrag "name"
               <||> authFrag "mac")
 where authFrag key = do
         _ <- try $ string (key ++ "=")
         frag <- many1 $ noneOf "&"
         optional $ char '&'
         return frag
       mkAuthToken exp' no name mac =
         let day = parseTime defaultTimeLocale (iso8601DateFormat Nothing) exp' in
         case day of
           Nothing -> error "Failed to parse expiration time"
           Just d  -> AuthToken { authExpiry    = d
                                , authMemberNo  = read no
                                , authUsername  = decodeString $ unEscapeString name
                                , authDigest    = mac
                                }

parseAuthToken :: String -> Maybe AuthToken
parseAuthToken s = case parse authTokenParser "" s of
                     Left  _ -> Nothing
                     Right x -> Just x
