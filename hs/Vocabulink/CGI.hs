-- Our program is by nature a CGI application. This is handy and troublesome at
-- the same time. For one, we have to deal with producing some reasonable
-- output for the client of our program, even when given incomplete or
-- incorrect inputs. But, since we're dealing with each of many requests in
-- their own threads, it's not the end of the world if we generate an error or
-- don't catch an exception.

module Vocabulink.CGI ( SCGI, SCGIT, Response, Resp(..)
                      , notFound, notAuthorized, notAllowed, emptyResponse
                      , queryVars, queryVar, queryVarDefault, queryVarRequired
                      , bodyVars, bodyVar, bodyVarDefault, bodyVarRequired, bodyJSON
                      , redirect, permRedirect, referrerOrVocabulink
                      , setCookie, redirectWithMsg, MsgType(..), bounce
                      {- Web.Cookie -}
                      , SetCookie(..)
                      ) where

import Vocabulink.Html hiding (name, a, id)
import Vocabulink.Utils

import Blaze.ByteString.Builder (toByteString)
import Data.ByteString (append)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (ord)
import Network.CGI (formDecode)
import Network.SCGI (SCGIT, Response(..))
import qualified Network.SCGI as SCGI
import Network.URI (parseURI, escapeURIString)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Cookie (SetCookie(..), renderSetCookie)

type SCGI = SCGIT IO

notFound :: Response
notFound = Response "404 Resource Not Found" ""

notAuthorized :: Response
notAuthorized = Response "401 Unauthorized" ""

notAllowed :: Response
notAllowed = Response "504 Method not allowed" ""

emptyResponse :: Response
emptyResponse = Response "200 OK" ""

class Resp a where
  toResponse :: a -> SCGI Response

instance Resp Html where
  toResponse html = do
    SCGI.setHeader "Content-Type" "text/html; charset=utf-8"
    return $ Response "200 OK" $ renderHtml html

instance Resp String where
  toResponse string = do
    SCGI.setHeader "Content-Type" "text/plain; charset=utf-8"
    return $ Response "200 OK" $ BLU.fromString string

instance Resp Value where
  toResponse obj = do
    SCGI.setHeader "Content-Type" "application/json"
    return $ Response "200 OK" $ encode obj

instance (Resp a) => Resp (Maybe a) where
  toResponse Nothing = return notFound
  toResponse (Just r) = toResponse r

instance (Resp a) => Resp (IO a) where
  toResponse a = toResponse =<< liftIO a

instance Resp (SCGI Response) where
  toResponse = id

queryVars :: Monad m => SCGIT m [(String, String)]
queryVars = do
  queryString <- SCGI.header "QUERY_STRING"
  return $ maybe [] (map (second convertLineEndings) . formDecode . BU.toString) queryString

queryVar :: Monad m => String -> SCGIT m (Maybe String)
queryVar name = lookup name `liftM` queryVars

queryVarDefault :: Monad m => String -> String -> SCGIT m String
queryVarDefault name def' = fromMaybe def' `liftM` queryVar name

queryVarRequired :: Monad m => String -> SCGIT m String
queryVarRequired name = queryVarDefault name (error $ "Parameter '" ++ name ++ "' is required.")

bodyVars :: Monad m => SCGIT m [(String, String)]
bodyVars = (map (second convertLineEndings) . formDecode . BLU.toString) `liftM` SCGI.body

bodyVar :: Monad m => String -> SCGIT m (Maybe String)
bodyVar name = lookup name `liftM` bodyVars

bodyVarDefault :: Monad m => String -> String -> SCGIT m String
bodyVarDefault name def' = fromMaybe def' `liftM` bodyVar name

bodyVarRequired :: Monad m => String -> SCGIT m String
bodyVarRequired name = bodyVarDefault name (error $ "Parameter '" ++ name ++ "' is required.")

bodyJSON :: (Monad m, FromJSON a) => SCGIT m (Maybe a)
bodyJSON = decode `liftM` SCGI.body

redirect :: String -> SCGI Response
redirect url = do
  SCGI.setHeader "Location" (BU.fromString url)
  return $ Response "302 Found" $ BLU.fromString $ "Redirecting you to: " ++ url

permRedirect :: String -> SCGI Response
permRedirect url = do
  SCGI.setHeader "Location" (BU.fromString url)
  return $ Response "301 Moved Permanently" ""

referrerOrVocabulink :: SCGI String
referrerOrVocabulink =
  maybe "http://www.vocabulink.com/" show `liftM` maybe Nothing (parseURI . BU.toString) `liftM` SCGI.header "HTTP_REFERER"

setCookie :: SetCookie -> SCGI ()
setCookie cookie = do
  cookieHeader <- SCGI.responseHeader "Set-Cookie"
  SCGI.setHeader "Set-Cookie" $ maybe newCookie (\h -> h `append` ", " `append` newCookie) cookieHeader
 where newCookie = toByteString $ renderSetCookie $ cookie {setCookieValue = BU.fromString $ escapeURIString isUnescapedInCookie $ BU.toString $ setCookieValue cookie}
       isUnescapedInCookie = (\c -> c == 0x21 -- RFC 6265
                                 || (c >= 0x23 && c <= 0x2B)
                                 || (c >= 0x2D && c <= 0x3A)
                                 || (c >= 0x3C && c <= 0x5B)
                                 || (c >= 0x5D && c <= 0x7E)) . ord

-- These message types correspond to JS alert types.
data MsgType = MsgSuccess | MsgError | MsgNotice

instance Show MsgType where
  show MsgSuccess = "success"
  show MsgError   = "error"
  show MsgNotice  = "notice"

-- Redirect the user and display a message on whatever page they end up on.
redirectWithMsg :: MsgType -> String -> String -> SCGI Response
redirectWithMsg typ msg url = do
  let value' = BLU.toString $ encode $ object ["type" .= (show typ), "msg" .= msg]
      cookie = def { setCookieName    = "msg"
                   , setCookieValue   = BU.fromString $ escapeURIString' value'
                   , setCookieExpires = Nothing
                   , setCookieDomain  = Just "www.vocabulink.com"
                   , setCookiePath    = Just "/"
                   }
  setCookie cookie
  SCGI.setHeader "Location" (BU.fromString url)
  return $ Response "302 Found" $ BLU.fromString msg

-- Redirect a user to where they came from along with a message.
bounce :: MsgType -> String -> SCGI Response
bounce typ msg = redirectWithMsg typ msg =<< referrerOrVocabulink
