{-|
Module : SwaggerPetstore.API
-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Model as M

import qualified Data.Aeson as A

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL

import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH

import qualified Web.HttpApiData as WH
import qualified Web.FormUrlEncoded as WH

import qualified Data.CaseInsensitive as CI
import qualified Data.Data as P (Typeable)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified GHC.Base as P (Alternative)
import qualified Control.Arrow as P (left)

import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude (($), (.),(<$>),(<*>),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty)
import qualified Prelude as P

-- * Operations


-- ** addPet

-- | POST \/pet
-- 
-- Add a new pet to the store
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
addPet 
  :: UseContentType AddPet Pet contenttype
  => contenttype -- ^ request content-type (mimetype)
  -> Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest AddPet contenttype ()
addPet m body =
  _mkRequest "POST" ["/pet"] m
    `setBodyParam` body

data AddPet
instance HasOptionalBodyParam AddPet Pet where
  setBodyParam req xs =
    req `_setBodyLBS` mimeRender (ctype req) xs

-- | @application/json@
instance Consumes AddPet MimeJSON
-- | @application/xml@
instance Consumes AddPet MimeXML

-- | @application/xml@
instance Produces AddPet MimeXML
-- | @application/json@
instance Produces AddPet MimeJSON


-- ** updatePetWithForm

-- | POST \/pet\/{petId}
-- 
-- Updates a pet in the store with form data
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
updatePetWithForm 
  :: Integer -- ^ "petId" -  ID of pet that needs to be updated
  -> SwaggerPetstoreRequest UpdatePetWithForm MimeFormUrlEncoded ()
updatePetWithForm petId =
  _mkRequest "POST" ["/pet/",toPath petId] MimeFormUrlEncoded
    

data UpdatePetWithForm

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name where
  applyOptionalParam req (Name xs) =
    req `_addForm` toForm ("name", xs)

-- -- | /Optional Param/ "status" - Updated status of the pet
-- instance HasOptionalParam UpdatePetWithForm Status where
--   --applyOptionalParam :: SwaggerPetstoreRequest UpdatePetWithForm MimeFormUrlEncoded () -> Status -> SwaggerPetstoreRequest UpdatePetWithForm MimeFormUrlEncoded ()
--   applyOptionalParam req (Status xs) =
--     req `_addForm` toForm ("status", xs)

-- | @application/x-www-form-urlencoded@
instance Consumes UpdatePetWithForm MimeFormUrlEncoded

-- | @application/xml@
instance Produces UpdatePetWithForm MimeXML
-- | @application/json@
instance Produces UpdatePetWithForm MimeJSON


-- * HasOptionalParam

-- | Designates the optional parameters of a request
class HasOptionalParam req param where
  {-# MINIMAL applyOptionalParam | (-&-) #-}

  -- | Apply an optional parameter to a request
  applyOptionalParam :: SwaggerPetstoreRequest req i res -> param -> SwaggerPetstoreRequest req i res
  applyOptionalParam = (-&-)
  {-# INLINE applyOptionalParam #-}

  -- | infix operator \/ alias for 'addOptionalParam'
  (-&-) :: SwaggerPetstoreRequest req i res -> param -> SwaggerPetstoreRequest req i res
  (-&-) = applyOptionalParam
  {-# INLINE (-&-) #-}

infixl 2 -&-

class HasOptionalBodyParam req param where
  setBodyParam :: UseContentType req param i => SwaggerPetstoreRequest req i res -> param -> SwaggerPetstoreRequest req i res
 
-- * Optional Request Parameter Types


newtype Name = Name { unName :: Text } deriving (P.Eq, P.Show)

newtype Status = Status { unStatus :: Text } deriving (P.Eq, P.Show)


-- * SwaggerPetstoreRequest

-- | Represents a request. The "req" type variable is the request type. The "res" type variable is the response type.
data SwaggerPetstoreRequest req i res = SwaggerPetstoreRequest
  { rMethod  :: NH.Method   -- ^ Method of SwaggerPetstoreRequest
  , urlPath :: [BCL.ByteString] -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: Params -- ^ params of SwaggerPetstoreRequest
  , ctype :: i -- ^ content-type of SwaggerPetstoreRequest
  }
  deriving (P.Show)

-- | Request Params
data Params = Params
  { paramsQuery :: NH.Query
  , paramsHeaders :: NH.RequestHeaders
  , paramsBody :: ParamBody
  }
  deriving (P.Show)

-- | Request Body
data ParamBody
  = ParamBodyNone
  | ParamBodyB B.ByteString
  | ParamBodyBL BL.ByteString
  | ParamBodyFormUrlEncoded WH.Form
  | ParamBodyMultipartFormData [NH.Part]
  deriving (P.Show)

-- ** SwaggerPetstoreRequest Utils

_mkRequest :: NH.Method -- ^ Method 
          -> [BCL.ByteString] -- ^ Endpoint
          -> ctype -- ^ Content-Type
          -> SwaggerPetstoreRequest req ctype res -- ^ req: Request Type, res: Response Type
_mkRequest m u ctype = SwaggerPetstoreRequest m u _mkParams ctype

_mkParams :: Params
_mkParams = Params [] [] ParamBodyNone

_addHeader :: SwaggerPetstoreRequest req i res -> [NH.Header] -> SwaggerPetstoreRequest req i res
_addHeader req header = 
    let _params = params req
    in req { params = _params { paramsHeaders = header P.++ paramsHeaders _params } }

_addQuery :: SwaggerPetstoreRequest req i res -> [NH.QueryItem] -> SwaggerPetstoreRequest req i res
_addQuery req query = 
    let _params = params req 
    in req { params = _params { paramsQuery = query P.++ paramsQuery _params } }

_addForm :: SwaggerPetstoreRequest req i res -> WH.Form -> SwaggerPetstoreRequest req i res
_addForm req newform = 
    let _params = params req
        form = case paramsBody _params of
            ParamBodyFormUrlEncoded _form -> _form
            _ -> mempty
    in req { params = _params { paramsBody = ParamBodyFormUrlEncoded (newform <> form) } }

_addMultiFormPart :: SwaggerPetstoreRequest req MimeMultipartFormData res -> NH.Part -> SwaggerPetstoreRequest req MimeMultipartFormData res
_addMultiFormPart req newpart = 
    let _params = params req
        parts = case paramsBody _params of
            ParamBodyMultipartFormData _parts -> _parts
            _ -> []
    in req { params = _params { paramsBody = ParamBodyMultipartFormData (newpart : parts) } }

_setBodyBS :: SwaggerPetstoreRequest req i res -> B.ByteString -> SwaggerPetstoreRequest req i res
_setBodyBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyB body } }

_setBodyLBS :: SwaggerPetstoreRequest req i res -> BL.ByteString -> SwaggerPetstoreRequest req i res
_setBodyLBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyBL body } }


-- ** Params Utils

toPath
  :: WH.ToHttpApiData a
  => a -> BCL.ByteString
toPath = BB.toLazyByteString . WH.toEncodedUrlPiece

toHeader :: WH.ToHttpApiData a => (NH.HeaderName, a) -> [NH.Header]
toHeader x = [fmap WH.toHeader x]

toForm :: WH.ToHttpApiData v => (BC.ByteString, v) -> WH.Form
toForm (k,v) = WH.toForm [(BC.unpack k,v)]

toQuery :: WH.ToHttpApiData a => (BC.ByteString, Maybe a) -> [NH.QueryItem]
toQuery x = [(fmap . fmap) toQueryParam x]
  where toQueryParam = T.encodeUtf8 . WH.toQueryParam

-- *** Swagger `CollectionFormat` Utils

-- | Determines the format of the array if type array is used.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. This is valid only for parameters in "query" ('NH.Query') or "formData" ('WH.Form')

toHeaderColl :: WH.ToHttpApiData a => CollectionFormat -> (NH.HeaderName, [a]) -> [NH.Header]
toHeaderColl c xs = _toColl c toHeader xs

toFormColl :: WH.ToHttpApiData v => CollectionFormat -> (BC.ByteString, [v]) -> WH.Form
toFormColl c xs = WH.toForm $ fmap unpack $ _toColl c toHeader $ pack xs
  where
    pack (k,v) = (CI.mk k, v)
    unpack (k,v) = (BC.unpack (CI.original k), BC.unpack v)

toQueryColl :: WH.ToHttpApiData a => CollectionFormat -> (BC.ByteString, Maybe [a]) -> NH.Query
toQueryColl c xs = _toCollA c toQuery xs

_toColl :: P.Traversable f => CollectionFormat -> (f a -> [(b, BC.ByteString)]) -> f [a] -> [(b, BC.ByteString)]
_toColl c encode xs = fmap (fmap P.fromJust) (_toCollA' c fencode BC.singleton (fmap Just xs))
  where fencode = fmap (fmap Just) . encode . fmap P.fromJust
        {-# INLINE fencode #-}

_toCollA :: (P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t BC.ByteString)]) -> f (t [a]) -> [(b, t BC.ByteString)]
_toCollA c encode xs = _toCollA' c encode BC.singleton xs

_toCollA' :: (P.Monoid c, P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t c)]) -> (Char -> c) -> f (t [a]) -> [(b, t c)]
_toCollA' c encode one xs = case c of
  CommaSeparated -> go (one ',')
  SpaceSeparated -> go (one ' ')
  TabSeparated -> go (one '\t')
  PipeSeparated -> go (one '|')
  MultiParamArray -> expandList
  where
    go sep =
      [P.foldl1 (\(sk, sv) (_, v) -> (sk, (combine sep <$> sv <*> v) <|> sv <|> v)) expandList]
    combine sep x y = x <> sep <> y
    expandList = (P.concatMap encode . (P.traverse . P.traverse) P.toList) xs
    {-# INLINE go #-}
    {-# INLINE expandList #-}
    {-# INLINE combine #-}
  

-- * Content Negotiation

-- ** UseContentType constraint

-- | Combines Consumes & MimeRender for an operation
type UseContentType operation model mediatype = (Consumes operation mediatype, MimeRender mediatype model)

-- ** Mime Types

data MimeJSON = MimeJSON deriving (P.Typeable, P.Show)
data MimeXML = MimeXML deriving (P.Typeable, P.Show)
data MimePlainText = MimePlainText deriving (P.Typeable, P.Show)
data MimeFormUrlEncoded = MimeFormUrlEncoded deriving (P.Typeable, P.Show)
data MimeMultipartFormData = MimeMultipartFormData deriving (P.Typeable, P.Show)
data MimeNoContent = MimeNoContent deriving (P.Typeable, P.Show)


-- ** MimeType Class

class MimeType mtype  where
  mimeType :: mtype -> Maybe ME.MediaType
  mimeType p =
    case mimeTypes p of
      [] -> Nothing
      (x:_) -> Just x
  mimeTypes :: mtype -> [ME.MediaType]
  mimeTypes p =
    case mimeType p of
      Just x -> [x]
      Nothing -> []
    
  {-# MINIMAL mimeType | mimeTypes #-}

-- ** MimeType Instances

-- | @application/json@
instance MimeType MimeJSON where
  mimeTypes _ =
    [ "application" ME.// "json" ME./: ("charset", "utf-8")
    , "application" ME.// "json"
    ]

-- | @application/xml@
instance MimeType MimeXML where
  mimeType _ = Just $ "application" ME.// "xml"

-- | @application/x-www-form-urlencoded@
instance MimeType MimeFormUrlEncoded where
  mimeType _ = Just $ "application" ME.// "x-www-form-urlencoded"

-- | @multipart/form-data@
instance MimeType MimeMultipartFormData where
  mimeType _ = Just $ "multipart" ME.// "form-data"

-- | @text/plain;charset=utf-8@
instance MimeType MimePlainText where
  mimeType _ = Just $ "text" ME.// "plain" ME./: ("charset", "utf-8")

instance MimeType MimeNoContent where
  mimeType _ = Nothing


-- ** MimeRender Class

class MimeType mtype => MimeRender mtype i where
    mimeRender  :: mtype -> i -> BL.ByteString

-- ** MimeRender Instances

-- | `A.encode`
instance A.ToJSON a => MimeRender MimeJSON a where mimeRender _ = A.encode
-- | @WH.urlEncodeAsForm@
instance WH.ToForm a => MimeRender MimeFormUrlEncoded a where mimeRender _ = WH.urlEncodeAsForm
-- | `TL.encodeUtf8`
instance MimeRender MimePlainText TL.Text where mimeRender _ = TL.encodeUtf8
-- | @BL.fromStrict . T.encodeUtf8@
instance MimeRender MimePlainText T.Text where mimeRender _ = BL.fromStrict . T.encodeUtf8
-- | @BCL.pack@
instance MimeRender MimePlainText String where mimeRender _ = BCL.pack

-- ** MimeUnrender Class

class MimeType mtype => MimeUnrender mtype o where
    mimeUnrender :: mtype -> BL.ByteString -> P.Either String o

-- ** MimeUnrender Instances

-- | @A.eitherDecode@
instance A.FromJSON a => MimeUnrender MimeJSON a where mimeUnrender _ = A.eitherDecode
-- | @P.left T.unpack . WH.urlDecodeAsForm@
instance WH.FromForm a => MimeUnrender MimeFormUrlEncoded a where mimeUnrender _ = P.left T.unpack . WH.urlDecodeAsForm
-- | @P.left P.show . TL.decodeUtf8'@
instance MimeUnrender MimePlainText TL.Text where mimeUnrender _ = P.left P.show . TL.decodeUtf8'
-- | @P.left P.show . T.decodeUtf8' . BL.toStrict@
instance MimeUnrender MimePlainText T.Text where mimeUnrender _ = P.left P.show . T.decodeUtf8' . BL.toStrict
-- | @P.Right . BCL.unpack@
instance MimeUnrender MimePlainText String where mimeUnrender _ = P.Right . BCL.unpack



-- ** Request Consumes

class Consumes req mtype where

-- ** Request Produces

class Produces req mtype where
