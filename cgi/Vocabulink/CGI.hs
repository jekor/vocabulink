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

-- Our program is by nature a CGI application. This is handy and troublesome at
-- the same time. For one, we have to deal with producing some reasonable
-- output for the client of our program, even when given incomplete or
-- incorrect inputs. But, since we're dealing with each of many requests in
-- their own threads, it's not the end of the world if we generate an error or
-- don't catch an exception.

module Vocabulink.CGI ( outputText, outputHtml, outputJSON
                      , outputNotFound, outputUnauthorized, outputClientError, outputServerError
                      , getInput, getRequiredInput, getInputDefault, getRequiredInputFPS
                      , readInput, readRequiredInput, readInputDefault, getBody, getBodyJSON, urlify
                      , referrerOrVocabulink, redirect', permRedirect
                      , redirectWithMsg, MsgType(..), bounce
                      , handleErrors, escapeURIString', addToQueryString
                      {- Data.Aeson.QQ -}
                      , aesonQQ
                      {- Data.Aeson.Types -}
                      , ToJSON(..)
                      {- Network.CGI -}
                      , MonadCGI, CGIResult
                      , requestURI, requestMethod, requestAccept
                      , getInputFPS, getBodyFPS, getInputFilename
                      , getInputNames, getInputContentType
                      , getVar, setHeader, redirect, remoteAddr
                      , outputNothing, outputError, outputMethodNotAllowed
                      , ContentType(..), negotiate
                      , Cookie(..), getCookie, setCookie, deleteCookie
                      {- Network.URI -}
                      , uriPath, uriQuery, escapeURIString, isUnescapedInURI
                      ) where

import Vocabulink.Html hiding (title, style)
import Vocabulink.Utils

import Control.Exception (Exception, SomeException, try)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (WriterT(..))
import Database.TemplatePG (pgDisconnect)
import Data.Aeson (toJSON, decode)
import Data.Aeson.Encode as J (encode)
import qualified Data.Aeson.Generic
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), object)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Char (isAlphaNum)
import Data.Text (pack)
import Network.CGI hiding (getInput, readInput, getBody, Html, output, outputNotFound, handleErrors)
import qualified Network.CGI as CGI
import Network.CGI.Monad (CGIT(..))
import Network.CGI.Protocol (CGIResult(..))
import Network.URI (uriPath, uriQuery, escapeURIString, isUnescapedInURI, URI(..), URIAuth(..), parseURI)
import Text.Blaze.Html5 (docTypeHtml, head, body, title, style, link)
import Text.Blaze.Html5.Attributes (rel)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Regex

import Prelude hiding (head, id, div)

-- Also, we do not always output HTML. Sometimes we output JSON or HTML fragments.

outputText :: (MonadCGI m, MonadIO m) => String -> m CGIResult
outputText s = do
  setHeader "Content-Type" "text/plain; charset=utf-8"
  return $ CGIOutput $ fromString s

outputHtml :: MonadCGI m => Html -> m CGIResult
outputHtml html = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  return $ CGIOutput $ renderHtml html

-- To output JSON, we just need an associative list.

outputJSON :: (MonadCGI m, MonadIO m, ToJSON a) => a -> m CGIResult
outputJSON obj = do
  setHeader "Content-Type" "application/json"
  return $ CGIOutput $ J.encode obj

-- Errors

-- TODO: Add nice HTML error pages
outputError' :: (MonadCGI m, MonadIO m) =>
                Int      -- ^ HTTP Status code
             -> String   -- ^ Message
             -> m CGIResult
outputError' c m = do
  logCGI $ show (c,m)
  setStatus c m
  outputText m

-- TODO: Some of this HTML is duplicated from Page.hs. It would be nice to make
-- Page.hs not depend on CGI.hs so that I could use its functionality here.
-- More importantly, common.css needs to be included properly using the
-- dependency mechanisms.
outputNotFound :: (MonadCGI m, MonadIO m) => m CGIResult
outputNotFound = do
  setStatus 404 "Not Found"
  referrer <- decodeString <$$> getVar "HTTP_REFERER"
  let internal = isJust (matchRegex (mkRegex "://www\\.vocabulink\\.com/") <$> referrer)
  when internal $ do -- Log internal broken links.
    uri <- requestURI
    logCGI ("Broken internal link: " ++ fromJust referrer ++ " -> " ++ show uri)
  outputHtml $ docTypeHtml $ do
    head $ do
      title "Vocabulink: Page Not Found"
      link ! rel "icon" ! type_ "image/png" ! href "http://s.vocabulink.com/img/favicon.png"
      link ! rel "stylesheet" ! type_ "text/css" ! href "http://s.vocabulink.com/css/common.css"
      style ! type_ "text/css" $ toHtml $ unlines
        ["body {background-color: #2D2D2D; height: auto;}"
        ,"#body {background-color: #FFFFFF; margin-top: 15%; padding-bottom: 0;}"
        ,"#message {width: 55em; margin-left: auto; margin-right: auto; padding-top: 5ex; padding-bottom: 5ex;}"
        ,"img {float: left; margin-right: 5em;}"
        ,"h1 {text-align: left; margin-top: 0;}"
        ,"ul {list-style-type: disc; list-style-position: inside;}"
        ]
    body $ do
      div ! id "body" $ do
        div ! id "message" $ do
          img ! src "http://s.vocabulink.com/img/404.png"
          h1 "Page Not Found"
          p "The page you're looking for does not exist. You probably ended up here because of:"
          unordList $ case referrer of
                        Nothing -> [ "a mis-typed address"
                                   , "an out-of-date bookmark"
                                   ]
                        Just _  -> if internal
                                     then ["an error on our part (a broken link)"]
                                     else ["an incorrect referral to this site (a broken link)"]
          p "You might find one of the following useful:"
          unordList
            [mconcat ["Return to the ", a ! href "http://www.vocabulink.com/" $ "homepage", "."]
            ,case referrer of
                Nothing -> mconcat ["View the list of ", a ! href "http://www.vocabulink.com/links" $ "available languages", "."]
                Just r  -> mconcat ["Go ", a ! href (toValue r) $ "back", "."]
            ]
      script ! src "http://www.google-analytics.com/ga.js" $ mempty

outputClientError :: (MonadCGI m, MonadIO m) => String -> m CGIResult
outputClientError = outputError' 400

outputServerError :: (MonadCGI m, MonadIO m) => String -> m CGIResult
outputServerError = outputError' 500

-- Usually we use the |withRequired| functions when an action requires that the
-- client be authenticated. However, sometimes (as with AJAX) we want to output
-- an actual 403 error.

outputUnauthorized :: (MonadCGI m, MonadIO m) => m CGIResult
outputUnauthorized = outputError' 403 "Unauthorized"

-- We need to handle UTF-8-encoded GET and POST parameters. The following are
-- enhanced versions of Network.CGI's |getInput| and |readInput| along with a
-- few helpers.

getInput :: MonadCGI m => String -> m (Maybe String)
getInput = liftM (>>= Just . convertLineEndings . trim . toString) . CGI.getInputFPS

-- Often we'll want an input from the client but are happy to fall back to a
-- default value.

getInputDefault :: MonadCGI m => String -> String -> m String
getInputDefault d n = do
  i <- getInput n
  return $ case i of
             Nothing -> d
             Just i' -> i'

-- As a convenience, |getRequiredInput| will throw an error on a missing input.
-- It allows us to write simpler code, but eventually most calls to this should
-- be removed and we should more gracefully handle the error.

getRequiredInput :: MonadCGI m => String -> m String
getRequiredInput param =
  getInputDefault (error $ "Parameter '" ++ param ++ "' is required.") param

getRequiredInputFPS :: MonadCGI m => String -> m ByteString
getRequiredInputFPS param = liftM (fromMaybe (error $ "Parameter '" ++ param ++ "' is required")) $ getInputFPS param

-- The Read versions of the above handle automatically converting the requested
-- input to a required type (as long as that type is Readable).

readInput :: (Read a, MonadCGI m) => String -> m (Maybe a)
readInput = liftM (>>= maybeRead) . getInput

readInputDefault :: (Read a, MonadCGI m) => a -> String -> m a
readInputDefault d = liftM (fromMaybe d) . readInput

readRequiredInput :: (Read a, MonadCGI m) => String -> m a
readRequiredInput param =
  readInputDefault (error $ "Parameter '" ++ param ++ "' is required.") param

getBody :: MonadCGI m => m String
getBody = (trim . toString) `liftM` CGI.getBodyFPS

getBodyJSON :: (MonadCGI m, FromJSON a) => m (Maybe a)
getBodyJSON = decode `liftM` CGI.getBodyFPS

-- Working with URLs

-- Certain dynamic parts of the site are displayed to the user in a friendly
-- natural form but are also used in URLs. For those cases, it's generally
-- better to use URL-safe representations for both the URL and the natural key
-- in the database. This allows us, for example, to re-title an article without
-- changing the URL it's located at. Also, since we have the natural key from
-- the URL, we don't need to do an extra database lookup to find the key from a
-- mapping table.

-- Note that this is not meant to handle arbitrary input from users. For that
-- we can use URL encoding. This is to make common URLs friendly.

-- Note that this is currently very restrictive until the need arises to permit
-- new characters. It converts spaces to hyphens and only allows alphanumeric
-- characters and hyphens in the resulting string.

urlify :: String -> String
urlify = map toLower . filter (\e -> isAlphaNum e || e `elem` "-.") . translate [(' ', '-')]

-- In some cases we'll need to redirect the client to where it came from after
-- we perform some action. We use this to make sure that we don't redirect them
-- off of the site.

referrerOrVocabulink :: MonadCGI m => m URI
referrerOrVocabulink = do
  ref <- maybe Nothing (parseURI . decodeString) `liftM` getVar "HTTP_REFERER"
  return $ case ref of
             Nothing -> URI { uriScheme = "http:"
                            , uriAuthority = Just (URIAuth { uriUserInfo = ""
                                                           , uriRegName = "//www.vocabulink.com"
                                                           , uriPort = ""
                                                           })
                            , uriPath = "/"
                            , uriQuery = ""
                            , uriFragment = "" }
             Just uri -> uri

redirect' :: MonadCGI m => URI -> m CGIResult
redirect' = redirect . show

-- These message types correspond to JS alert types.
data MsgType = MsgSuccess | MsgError | MsgNotice

instance Show MsgType where
  show MsgSuccess = "success"
  show MsgError   = "error"
  show MsgNotice  = "notice"

-- Redirect the user and display a message on whatever page they end up on.
redirectWithMsg :: MonadCGI m => MsgType -> String -> URI -> m CGIResult
redirectWithMsg typ msg url = do
  let value' = toString $ J.encode [aesonQQ| {"type": <| show typ |>, "msg": <| msg |>} |]
      cookie = Cookie { cookieName    = "msg"
                      , cookieValue   = escapeURIString' value'
                      , cookieExpires = Nothing
                      , cookieDomain  = Just "www.vocabulink.com"
                      , cookiePath    = Just "/"
                      , cookieSecure  = False
                      }
  setCookie cookie
  redirect' url

-- Redirect a user to where they came from along with a message.
bounce :: MonadCGI m => MsgType -> String -> m CGIResult
bounce typ msg = redirectWithMsg typ msg =<< referrerOrVocabulink

permRedirect :: (MonadCGI m, MonadIO m) => String -> m CGIResult
permRedirect url = do
  setStatus 301 "Moved Permanently"
  redirect url

-- It's quite probable that we're going to trigger an unexpected exception
-- somewhere in the program. Rather than blow up, we'd like to catch and log
-- the exception before giving the client some sort of indication that
-- something went wrong.

catchCGI' :: Exception e => CGI a -> (e -> CGI a) -> CGI a
catchCGI' c h = tryCGI' c >>= either h return

tryCGI' :: Exception e => CGI a -> CGI (Either e a)
tryCGI' (CGIT c) = CGIT (ReaderT (WriterT . f . runWriterT . runReaderT c ))
 where f = fmap (either (\ex -> (Left ex,mempty)) (first Right)) . try

-- |catchCGI'| will handle exceptions in the CGI monad. If no exception is
--  thrown, we'll close the database handle and return the CGI result.

handleErrors :: Handle -> CGI CGIResult -> CGI CGIResult
handleErrors h act = catchCGI' (do r <- act
                                   liftIO $ pgDisconnect h
                                   return r)
                               (outputException' h)

-- Network.CGI provides |outputException| as a basic default error handler. This
-- is a slightly modified version that logs errors.

-- One case that needs to be tested is when an error message has non-ASCII
-- characters. I'm not sure how |outputInternalServerError| will handle it.

-- By the time we output the error to the client we no longer need the database
-- handle. This is the perfect place to close it, as it'll be the last thing we do
-- in the CGI monad (and the thread).

outputException' :: (MonadCGI m, MonadIO m) => Handle -> SomeException -> m CGIResult
outputException' h ex = do
  liftIO $ pgDisconnect h
  outputServerError $ show ex

escapeURIString' :: String -> String
escapeURIString' = escapeURIString isUnescapedInURI

addToQueryString :: String -> URI -> URI
addToQueryString s uri =
  let query' = case uriQuery uri of
                 "" -> "?" ++ s
                 q' -> q' ++ "&" ++ s in
  uri {uriQuery = query'}
