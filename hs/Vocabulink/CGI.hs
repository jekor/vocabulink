-- Our program is by nature a CGI application. This is handy and troublesome at
-- the same time. For one, we have to deal with producing some reasonable
-- output for the client of our program, even when given incomplete or
-- incorrect inputs. But, since we're dealing with each of many requests in
-- their own threads, it's not the end of the world if we generate an error or
-- don't catch an exception.

module Vocabulink.CGI ( notFound, notAuthorized, maybeNotFound
                      , bodyVars, bodyVar, bodyVarDefault, bodyVarRequired
                      , redirect, permRedirect, referrerOrVocabulink
                      , setCookie
                      , redirectWithMsg, redirectWithMsg', MsgType(..)
                      , bounce, cookieBounce, cookieRedirect
                      {- Web.Cookie -}
                      , SetCookie(..)
                      ) where

import Vocabulink.Utils

import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (ord)
import Data.String.Conv (toS)
import Network.HTTP.Types.Header (Header, hSetCookie, hLocation)
import Network.URI (parseURI, escapeURIString)
import Servant (throwError, err301, err302, err401, err404, errBody, errHeaders)
import Servant.Multipart (Input(iName, iValue), inputs, lookupInput)
import Web.Cookie (SetCookie(..), renderSetCookie)

notFound = throwError (err404 {errBody = "Not found"})

notAuthorized = throwError (err401 {errBody = "Not authorized"})

maybeNotFound f = do
  f' <- f
  case f' of
    Nothing -> notFound
    Just f'' -> return f''

redirect url = throwError (err302 {errHeaders = [("Location", url)], errBody = "Redirecting you to: " <> toS url})

permRedirect url = throwError (err301 {errHeaders = [("Location", url)], errBody = "Moved to: " <> toS url})

-- TODO: Put convertLineEndings where it belongs (it used to be here).
bodyVars :: _ -> [(String, String)]
bodyVars = fmap (\ i -> (toS (iName i), toS (iValue i))) . inputs

-- bodyVar :: Monad m => String -> SCGIT m (Maybe String)
bodyVar :: _ -> _ -> Maybe String
bodyVar name = (fmap toS) . lookupInput (toS name)

bodyVarDefault name def' = fromMaybe def' `liftM` bodyVar name

bodyVarRequired name = bodyVarDefault name (error $ "Parameter '" ++ name ++ "' is required.")

referrerOrVocabulink = maybe "https://www.vocabulink.com/" show ((parseURI . toS) =<< ?referrer)

-- TODO: Set Secure and HttpOnly
-- [ ("Set-Cookie", "auth=" <> token <> "; Path=/; Secure; HttpOnly")
-- , ("Cache-Control", "no-cache, no-store, max-age=0, must-revalidate") ]
setCookie :: SetCookie -> _
setCookie cookie = (hSetCookie, toS $ toLazyByteString $ renderSetCookie $ cookie {setCookieValue = toS $ escapeURIString isUnescapedInCookie $ toS $ setCookieValue cookie})
 where isUnescapedInCookie = (\c -> c == 0x21 -- RFC 6265
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

toastMsgCookie (typ :: MsgType) msg =
  let value' = BLU.toString $ encode $ object ["type" .= (show typ), "msg" .= msg]
  in def { setCookieName    = "msg"
         , setCookieValue   = BU.fromString $ escapeURIString' value'
         , setCookieExpires = Nothing
         , setCookieDomain  = Just "www.vocabulink.com"
         , setCookiePath    = Just "/"
         }

redirectWithMsg' url cookies typ msg =
  ((hLocation, toS url) : fmap setCookie (toastMsgCookie typ msg:cookies), toS (msg <> "\n\nRedirecting you to: " <> url)) :: ([Header], BLU.ByteString)

-- Redirect the user and display a message on whatever page they end up on.
redirectWithMsg url cookies typ msg =
  let (headers, body) = redirectWithMsg' url cookies typ msg
  in throwError (err302 {errHeaders = headers, errBody = body})

-- Redirect a user to where they came from along with a message.
bounce = redirectWithMsg referrerOrVocabulink []

cookieBounce = redirectWithMsg referrerOrVocabulink

cookieRedirect cookies url = throwError (err302 {errHeaders = ("Location", url) : (fmap setCookie cookies), errBody = toS ("Redirecting you to: " <> url)})
