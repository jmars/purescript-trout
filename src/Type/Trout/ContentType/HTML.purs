module Type.Trout.ContentType.HTML
       ( HTML
       , WithoutHeaders
       , WithHeaders
       -- TODO, linkTo
       , class EncodeHTML
       , encodeHTML
       , TroutURI
       ) where

import Prelude

import Data.Map (Map, empty)
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeRender, getMediaType, mimeRender)
import Type.Trout.Headers (class Headers, headers)
import URI (Fragment, HierPath, Host, Path, Port, Query, RelPath, URIRef, UserInfo)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.URIRef (URIRefPrintOptions)

data WithHeaders
data WithoutHeaders

-- | A content type, corresponding to the `text/html` media type.
data HTML a

-- TODO: Enforce that URI comes from a GET-able resource,
-- perhaps by wrapping the URI type and adding some phantom
-- type parameter for the HTTP method.

type TroutURI = URIRef UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment

uriOpts ∷ Record (URIRefPrintOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment ()) 
uriOpts =
  { printUserInfo: identity
  , printHosts: HostPortPair.print identity identity
  , printPath: identity
  , printHierPath: identity
  , printQuery: identity
  , printFragment: identity
  , printRelPath: identity
  }

-- | Helper function for generating a Smolder anchor tag based on
-- | a `URI`.
-- TODO: get this working again
-- linkTo :: TroutURI -> Markup Unit -> Markup Unit
-- linkTo uri = a ! href (URIRef.print uriOpts uri)

-- | Encodes a value as HTML, using Smolder markup.
class EncodeHTML a where
  -- | Encode the given value as Smolder markup.
  encodeHTML :: a -> String

instance hasMediaTypeHTMLWithHeaders :: HasMediaType (HTML WithHeaders) where
  getMediaType _ = textHTML
else instance hasMediaTypeHTML :: HasMediaType (HTML WithoutHeaders) where
  getMediaType _ = textHTML

instance mimeRenderHTMLEncodeHTML :: (EncodeHTML a, Headers a) => MimeRender a (HTML WithHeaders) ((Map String String) /\ String) where
  mimeRender _ v = (headers v) /\ (encodeHTML v)
else instance mimeRenderNoHeaders :: (EncodeHTML a) => MimeRender a (HTML WithoutHeaders) ((Map String String) /\ String) where
  mimeRender _ v = empty /\ (encodeHTML v)
else instance mimeRenderHTML :: MimeRender String (HTML WithoutHeaders) ((Map String String) /\ String) where
  mimeRender p v = empty /\ v

instance allMimeRenderHTML :: (EncodeHTML a, Headers a) => AllMimeRender a (HTML WithHeaders) ((Map String String) /\ String) where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
else instance allMimeRenderHTMLNoHeaders :: (EncodeHTML a) => AllMimeRender a (HTML WithoutHeaders) ((Map String String) /\ String) where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))