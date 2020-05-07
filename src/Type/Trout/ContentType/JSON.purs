module Type.Trout.ContentType.JSON where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Map (Map, empty)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeParse, class MimeRender, getMediaType, mimeRender)

-- | A content type, corresponding to the `application/json` media type.
data JSON

instance hasMediaTypeJson :: HasMediaType JSON where
  getMediaType _ = applicationJSON

instance mimeRenderJson :: EncodeJson a => MimeRender a JSON ((Map String String) /\ String) where
  mimeRender _ = ((/\) empty) <<< stringify <<< encodeJson

instance mimeParseJson :: DecodeJson a => MimeParse String JSON a where
  mimeParse _ = jsonParser >=> decodeJson

instance allMimeRenderJson :: EncodeJson a => AllMimeRender a JSON ((Map String String) /\ String) where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
