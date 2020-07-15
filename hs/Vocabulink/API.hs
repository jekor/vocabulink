module Vocabulink.API where

import Data.Data (Typeable)
import Data.Int (Int32)
import qualified Data.Map.Strict as M
import Data.String.Conv (toS)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Network.HTTP.Media ((//), (/:))
import Servant ((:>), (:<|>), JSON, PlainText, Accept(contentType), MimeRender(mimeRender), MimeUnrender(mimeUnrender), Get, Post, Capture, QueryParam', Required, Strict, ReqBody, PostNoContent, PutNoContent, GetNoContent, NoContent, FromHttpApiData(..))
import Servant.Multipart (MultipartForm, MultipartData, Mem)
import Text.Blaze (toMarkup)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Read (readEither)

import Vocabulink.Link (LinkOutput(..), Story)
import Vocabulink.Utils

type VocabulinkAPI =
       Get '[HTML] Html
  :<|> "download" :> GetNoContent '[HTML] NoContent
  :<|> ArticleAPI
  :<|> "link" :> LinkAPI
  :<|> "links" :> QueryParam' '[Required, Strict] "ol" Language :> QueryParam' '[Required, Strict] "dl" Language :> Get '[HTML] Html
  :<|> "reader" :> Capture "lang" String :> Capture "name" String :> Capture "page" Int32 :> Get '[HTML] Html
  :<|> "search" :> QueryParam' '[Required, Strict] "q" String :> Get '[HTML] Html
  :<|> "review" :> ReviewAPI
  :<|> "dashboard" :> Get '[HTML] Html
  :<|> "member" :> MemberAPI
  :<|> "user" :> Capture "username" String :> UserAPI
  :<|> "email" :> Capture "email" String :> "available" :> Get '[JSON] Bool
  :<|> "comment" :> Capture "commentNumber" Int32 :> CommentAPI
  :<|> "robots.txt" :> Get '[PlainText] String
  :<|> "google1e7c25c4bdfc5be7.html" :> Get '[PlainText] String

type ArticleAPI = "article" :> Capture "article" String :> Get '[HTML] Html
             :<|> "articles" :> Get '[HTML] Html
             :<|> "help" :> Get '[HTML] Html
             :<|> "privacy" :> Get '[HTML] Html
             :<|> "terms-of-use" :> Get '[HTML] Html
             :<|> "source" :> Get '[HTML] Html
             :<|> "api" :> Get '[HTML] Html

type LinkAPI = "story" :> Capture "storyNumber" Int32 :> LinkStoryAPI
          :<|> Capture "linkNumber" Int32 :> LinkAPI'

type LinkAPI' = Get '[JSON, HTML] LinkOutput
           :<|> "compact" :> Get '[HTML] Html
           :<|> "stories" :> Get '[HTML] [Story]
           :<|> "stories" :> MultipartForm Mem (MultipartData Mem) :> PostNoContent '[JSON] NoContent

type LinkStoryAPI = Get '[PlainText] String
               :<|> ReqBody '[PlainText] String :> PutNoContent '[JSON] NoContent
               :<|> MultipartForm Mem (MultipartData Mem) :> PostNoContent '[HTML] NoContent

type ReviewAPI = QueryParam' '[Required, Strict] "learn" Language :> QueryParam' '[Required, Strict] "known" Language :> Get '[HTML] Html
            -- TODO: What is this for?
            :<|> ReqBody '[PlainText] Int32 :> PutNoContent '[JSON] NoContent
            :<|> "stats" :> Get '[JSON] Value
            :<|> "stats" :> Capture "type" StatType :> QueryParam' '[Required, Strict] "start" Day :> QueryParam' '[Required, Strict] "end" Day :> QueryParam' '[Required, Strict] "tzoffset" String :> Get '[JSON] Value
            :<|> Capture "linkNumber" Int32 :> MultipartForm Mem (MultipartData Mem) :> PostNoContent '[JSON] NoContent

type MemberAPI = "signup" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html
            :<|> "confirmation" :> Capture "hash" String :> Get '[HTML] Html
            :<|> "confirmation" :> Post '[HTML] Html
            :<|> "login" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html
            :<|> "logout" :> Post '[HTML] Html
            :<|> "delete" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html
            :<|> "password" :> PasswordAPI
            :<|> "email" :> "change" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html

-- TODO: Put under a common prefix (instead of having a separate "change" endpoint).
type PasswordAPI = "reset" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html
              :<|> "reset" :> Capture "hash" String :> Get '[HTML] Html
              :<|> "reset" :> Capture "hash" String :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html
              :<|> "change" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] Html

type UserAPI = Get '[HTML] Html
          :<|> "available" :> Get '[JSON] Bool

type CommentAPI = "reply" :> MultipartForm Mem (MultipartData Mem) :> PostNoContent '[JSON] NoContent

data HTML deriving Typeable

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Html where
  mimeRender _ = renderHtml

instance MimeRender HTML LinkOutput where
  mimeRender _ (LinkOutput _ html) = renderHtml html

instance MimeRender JSON LinkOutput where
  mimeRender _ (LinkOutput link _) = encode link

instance MimeRender HTML [Story] where
  mimeRender _ = renderHtml . toMarkup . fmap toMarkup

instance MimeUnrender PlainText Int32 where
  mimeUnrender _ = readEither . toS

data Language = Language String

instance FromHttpApiData Language where
  parseQueryParam t = case M.lookup (T.unpack t) languages of
                        Just _ -> Right (Language (T.unpack t))
                        Nothing -> Left "Language not found"

data StatType = StatTypeDaily | StatTypeDetailed

instance FromHttpApiData StatType where
  parseUrlPiece "daily" = Right StatTypeDaily
  parseUrlPiece "detailed" = Right StatTypeDetailed
  parseUrlPiece _ = Left "Unknown stat type"
