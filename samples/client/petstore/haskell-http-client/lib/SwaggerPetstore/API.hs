{-|
Module : SwaggerPetstore.API
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Model as M

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL

import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Network.HTTP.Types.Method as NH
import qualified Network.HTTP.Types.URI as NH

import qualified Web.HttpApiData as WH
import qualified Web.FormUrlEncoded as WH

import qualified Control.Monad.IO.Class as P
import qualified Data.CaseInsensitive as CI
import qualified Data.Data as P (Typeable)
import qualified Data.Foldable as P
import qualified Data.Functor.Identity as P (Identity(..))
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import qualified Data.Void as Void
import qualified GHC.Base as P (Alternative)

import Data.Function ((&))
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
  :: Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest AddPet ()
addPet body =
  _mkRequest "POST" ["/pet"]
    `_setBodyLBS` A.encode body

data AddPet

-- | @application/json@
instance Consumes AddPet MimeJSON
-- | @application/xml@
instance Consumes AddPet MimeXML

  
-- | @application/xml@
instance Produces AddPet MimeXML
-- | @application/json@
instance Produces AddPet MimeJSON


-- ** deletePet

-- | DELETE \/pet\/{petId}
-- 
-- Deletes a pet
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
deletePet 
  :: Integer -- ^ "petId" -  Pet id to delete
  -> SwaggerPetstoreRequest DeletePet ()
deletePet petId =
  _mkRequest "DELETE" ["/pet/",toPath petId]
    

data DeletePet
instance HasOptionalParam DeletePet ApiUnderscorekey where
  applyOptionalParam req (ApiUnderscorekey xs) =
    req `_addHeader` toHeader ("api_key", xs)
  
-- | @application/xml@
instance Produces DeletePet MimeXML
-- | @application/json@
instance Produces DeletePet MimeJSON


-- ** findPetsByStatus

-- | GET \/pet\/findByStatus
-- 
-- Finds Pets by status
-- 
-- Multiple status values can be provided with comma separated strings
-- 
-- AuthMethod: petstore_auth
-- 
findPetsByStatus 
  :: [Text] -- ^ "status" -  Status values that need to be considered for filter
  -> SwaggerPetstoreRequest FindPetsByStatus [Pet]
findPetsByStatus status =
  _mkRequest "GET" ["/pet/findByStatus"]
    `_addQuery` toQueryColl MultiParamArray ("status", Just status)

data FindPetsByStatus
  
-- | @application/xml@
instance Produces FindPetsByStatus MimeXML
-- | @application/json@
instance Produces FindPetsByStatus MimeJSON


-- ** findPetsByTags

-- | GET \/pet\/findByTags
-- 
-- Finds Pets by tags
-- 
-- Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-- 
-- AuthMethod: petstore_auth
-- 
findPetsByTags 
  :: [Text] -- ^ "tags" -  Tags to filter by
  -> SwaggerPetstoreRequest FindPetsByTags [Pet]
findPetsByTags tags =
  _mkRequest "GET" ["/pet/findByTags"]
    `_addQuery` toQueryColl MultiParamArray ("tags", Just tags)

{-# DEPRECATED findPetsByTags "" #-}

data FindPetsByTags
  
-- | @application/xml@
instance Produces FindPetsByTags MimeXML
-- | @application/json@
instance Produces FindPetsByTags MimeJSON


-- ** getPetById

-- | GET \/pet\/{petId}
-- 
-- Find pet by ID
-- 
-- Returns a single pet
-- 
-- AuthMethod: api_key
-- 
getPetById 
  :: Integer -- ^ "petId" -  ID of pet to return
  -> SwaggerPetstoreRequest GetPetById Pet
getPetById petId =
  _mkRequest "GET" ["/pet/",toPath petId]
    

data GetPetById
  
-- | @application/xml@
instance Produces GetPetById MimeXML
-- | @application/json@
instance Produces GetPetById MimeJSON


-- ** updatePet

-- | PUT \/pet
-- 
-- Update an existing pet
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
updatePet 
  :: Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest UpdatePet ()
updatePet body =
  _mkRequest "PUT" ["/pet"]
    `_setBodyLBS` A.encode body

data UpdatePet

-- | @application/json@
instance Consumes UpdatePet MimeJSON
-- | @application/xml@
instance Consumes UpdatePet MimeXML

  
-- | @application/xml@
instance Produces UpdatePet MimeXML
-- | @application/json@
instance Produces UpdatePet MimeJSON


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
  -> SwaggerPetstoreRequest UpdatePetWithForm ()
updatePetWithForm petId =
  _mkRequest "POST" ["/pet/",toPath petId]
    

data UpdatePetWithForm

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name where
  applyOptionalParam req (Name xs) =
    req `_addForm` toForm ("name", xs)

-- | /Optional Param/ "status" - Updated status of the pet
instance HasOptionalParam UpdatePetWithForm Status where
  applyOptionalParam req (Status xs) =
    req `_addForm` toForm ("status", xs)

-- | @application/x-www-form-urlencoded@
instance Consumes UpdatePetWithForm MimeFormUrlEncoded

  
-- | @application/xml@
instance Produces UpdatePetWithForm MimeXML
-- | @application/json@
instance Produces UpdatePetWithForm MimeJSON


-- ** uploadFile

-- | POST \/pet\/{petId}\/uploadImage
-- 
-- uploads an image
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
uploadFile 
  :: Integer -- ^ "petId" -  ID of pet to update
  -> SwaggerPetstoreRequest UploadFile ApiResponse
uploadFile petId =
  _mkRequest "POST" ["/pet/",toPath petId,"/uploadImage"]
    

data UploadFile

-- | /Optional Param/ "additionalMetadata" - Additional data to pass to server
instance HasOptionalParam UploadFile AdditionalMetadata where
  applyOptionalParam req (AdditionalMetadata xs) =
    req `_addForm` toForm ("additionalMetadata", xs)

-- | /Optional Param/ "file" - file to upload
instance HasOptionalParam UploadFile File where
  applyOptionalParam req (File xs) =
    req `_addForm` toForm ("file", xs)

-- | @multipart/form-data@
instance Consumes UploadFile MimeMultipartFormData

  
-- | @application/json@
instance Produces UploadFile MimeJSON


-- ** deleteOrder

-- | DELETE \/store\/order\/{orderId}
-- 
-- Delete purchase order by ID
-- 
-- For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
-- 
deleteOrder 
  :: Integer -- ^ "orderId" -  ID of the order that needs to be deleted
  -> SwaggerPetstoreRequest DeleteOrder ()
deleteOrder orderId =
  _mkRequest "DELETE" ["/store/order/",toPath orderId]
    

data DeleteOrder
  
-- | @application/xml@
instance Produces DeleteOrder MimeXML
-- | @application/json@
instance Produces DeleteOrder MimeJSON


-- ** getInventory

-- | GET \/store\/inventory
-- 
-- Returns pet inventories by status
-- 
-- Returns a map of status codes to quantities
-- 
-- AuthMethod: api_key
-- 
getInventory 
  :: SwaggerPetstoreRequest GetInventory (Map.Map String Int)
getInventory =
  _mkRequest "GET" ["/store/inventory"]

data GetInventory
  
-- | @application/json@
instance Produces GetInventory MimeJSON


-- ** getOrderById

-- | GET \/store\/order\/{orderId}
-- 
-- Find purchase order by ID
-- 
-- For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions
-- 
getOrderById 
  :: Integer -- ^ "orderId" -  ID of pet that needs to be fetched
  -> SwaggerPetstoreRequest GetOrderById Order
getOrderById orderId =
  _mkRequest "GET" ["/store/order/",toPath orderId]
    

data GetOrderById
  
-- | @application/xml@
instance Produces GetOrderById MimeXML
-- | @application/json@
instance Produces GetOrderById MimeJSON


-- ** placeOrder

-- | POST \/store\/order
-- 
-- Place an order for a pet
-- 
-- 
-- 
placeOrder 
  :: Order -- ^ "body" -  order placed for purchasing the pet
  -> SwaggerPetstoreRequest PlaceOrder Order
placeOrder body =
  _mkRequest "POST" ["/store/order"]
    `_setBodyLBS` A.encode body

data PlaceOrder
  
-- | @application/xml@
instance Produces PlaceOrder MimeXML
-- | @application/json@
instance Produces PlaceOrder MimeJSON


-- ** createUser

-- | POST \/user
-- 
-- Create user
-- 
-- This can only be done by the logged in user.
-- 
createUser 
  :: User -- ^ "body" -  Created user object
  -> SwaggerPetstoreRequest CreateUser ()
createUser body =
  _mkRequest "POST" ["/user"]
    `_setBodyLBS` A.encode body

data CreateUser
  
-- | @application/xml@
instance Produces CreateUser MimeXML
-- | @application/json@
instance Produces CreateUser MimeJSON


-- ** createUsersWithArrayInput

-- | POST \/user\/createWithArray
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
createUsersWithArrayInput 
  :: [User] -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest CreateUsersWithArrayInput ()
createUsersWithArrayInput body =
  _mkRequest "POST" ["/user/createWithArray"]
    `_setBodyLBS` A.encode body

data CreateUsersWithArrayInput
  
-- | @application/xml@
instance Produces CreateUsersWithArrayInput MimeXML
-- | @application/json@
instance Produces CreateUsersWithArrayInput MimeJSON


-- ** createUsersWithListInput

-- | POST \/user\/createWithList
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
createUsersWithListInput 
  :: [User] -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest CreateUsersWithListInput ()
createUsersWithListInput body =
  _mkRequest "POST" ["/user/createWithList"]
    `_setBodyLBS` A.encode body

data CreateUsersWithListInput
  
-- | @application/xml@
instance Produces CreateUsersWithListInput MimeXML
-- | @application/json@
instance Produces CreateUsersWithListInput MimeJSON


-- ** deleteUser

-- | DELETE \/user\/{username}
-- 
-- Delete user
-- 
-- This can only be done by the logged in user.
-- 
deleteUser 
  :: Text -- ^ "username" -  The name that needs to be deleted
  -> SwaggerPetstoreRequest DeleteUser ()
deleteUser username =
  _mkRequest "DELETE" ["/user/",toPath username]
    

data DeleteUser
  
-- | @application/xml@
instance Produces DeleteUser MimeXML
-- | @application/json@
instance Produces DeleteUser MimeJSON


-- ** getUserByName

-- | GET \/user\/{username}
-- 
-- Get user by user name
-- 
-- 
-- 
getUserByName 
  :: Text -- ^ "username" -  The name that needs to be fetched. Use user1 for testing. 
  -> SwaggerPetstoreRequest GetUserByName User
getUserByName username =
  _mkRequest "GET" ["/user/",toPath username]
    

data GetUserByName
  
-- | @application/xml@
instance Produces GetUserByName MimeXML
-- | @application/json@
instance Produces GetUserByName MimeJSON


-- ** loginUser

-- | GET \/user\/login
-- 
-- Logs user into the system
-- 
-- 
-- 
loginUser 
  :: Text -- ^ "username" -  The user name for login
  -> Text -- ^ "password" -  The password for login in clear text
  -> SwaggerPetstoreRequest LoginUser Text
loginUser username password =
  _mkRequest "GET" ["/user/login"]
    `_addQuery` toQuery ("username", Just username)
    `_addQuery` toQuery ("password", Just password)

data LoginUser
  
-- | @application/xml@
instance Produces LoginUser MimeXML
-- | @application/json@
instance Produces LoginUser MimeJSON


-- ** logoutUser

-- | GET \/user\/logout
-- 
-- Logs out current logged in user session
-- 
-- 
-- 
logoutUser 
  :: SwaggerPetstoreRequest LogoutUser ()
logoutUser =
  _mkRequest "GET" ["/user/logout"]

data LogoutUser
  
-- | @application/xml@
instance Produces LogoutUser MimeXML
-- | @application/json@
instance Produces LogoutUser MimeJSON


-- ** updateUser

-- | PUT \/user\/{username}
-- 
-- Updated user
-- 
-- This can only be done by the logged in user.
-- 
updateUser 
  :: Text -- ^ "username" -  name that need to be updated
  -> User -- ^ "body" -  Updated user object
  -> SwaggerPetstoreRequest UpdateUser ()
updateUser username body =
  _mkRequest "PUT" ["/user/",toPath username]
    
    `_setBodyLBS` A.encode body

data UpdateUser
  
-- | @application/xml@
instance Produces UpdateUser MimeXML
-- | @application/json@
instance Produces UpdateUser MimeJSON

 
-- * HasOptionalParam

-- | Designates the optional parameters of a request
class HasOptionalParam req param where
  {-# MINIMAL applyOptionalParam | (-&-) #-}

  -- | Apply an optional parameter to a request
  applyOptionalParam :: forall res. SwaggerPetstoreRequest req res -> param -> SwaggerPetstoreRequest req res
  applyOptionalParam = (-&-)
  {-# INLINE applyOptionalParam #-}

  -- | infix operator \/ alias for 'addOptionalParam'
  (-&-) :: forall res. SwaggerPetstoreRequest req res -> param -> SwaggerPetstoreRequest req res
  (-&-) = applyOptionalParam
  {-# INLINE (-&-) #-}

infixl 2 -&-

-- * Optional Request Parameter Types


newtype ApiUnderscorekey = ApiUnderscorekey { unApiUnderscorekey :: Text } deriving (P.Eq, P.Show)

newtype Name = Name { unName :: Text } deriving (P.Eq, P.Show)

newtype Status = Status { unStatus :: Text } deriving (P.Eq, P.Show)

newtype AdditionalMetadata = AdditionalMetadata { unAdditionalMetadata :: Text } deriving (P.Eq, P.Show)

newtype File = File { unFile :: FilePath } deriving (P.Eq, P.Show)


-- * SwaggerPetstoreRequest

-- | Represents a request. The "req" type variable is the request type. The "res" type variable is the response type.
data SwaggerPetstoreRequest req res = SwaggerPetstoreRequest
  { rMethod  :: NH.Method   -- ^ Method of SwaggerPetstoreRequest
  , urlPath :: [BCL.ByteString] -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: Params -- ^ params of SwaggerPetstoreRequest
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
          -> SwaggerPetstoreRequest req res -- ^ req: Request Type, res: Response Type
_mkRequest m u = SwaggerPetstoreRequest m u _mkParams

_mkParams :: Params
_mkParams = Params [] [] ParamBodyNone

_addHeader :: SwaggerPetstoreRequest req res -> [NH.Header] -> SwaggerPetstoreRequest req res
_addHeader req header = 
    let _params = params req
    in req { params = _params { paramsHeaders = header P.++ paramsHeaders _params } }

_addQuery :: SwaggerPetstoreRequest req res -> [NH.QueryItem] -> SwaggerPetstoreRequest req res
_addQuery req query = 
    let _params = params req 
    in req { params = _params { paramsQuery = query P.++ paramsQuery _params } }

_addForm :: SwaggerPetstoreRequest req res -> WH.Form -> SwaggerPetstoreRequest req res
_addForm req newform = 
    let _params = params req
        form = case paramsBody _params of
            ParamBodyFormUrlEncoded _form -> _form
            _ -> mempty
    in req { params = _params { paramsBody = ParamBodyFormUrlEncoded (newform <> form) } }

_addMultiFormPart :: SwaggerPetstoreRequest req res -> NH.Part -> SwaggerPetstoreRequest req res
_addMultiFormPart req newpart = 
    let _params = params req
        parts = case paramsBody _params of
            ParamBodyMultipartFormData _parts -> _parts
            _ -> []
    in req { params = _params { paramsBody = ParamBodyMultipartFormData (newpart : parts) } }

_setBodyBS :: SwaggerPetstoreRequest req res -> B.ByteString -> SwaggerPetstoreRequest req res
_setBodyBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyB body } }

_setBodyLBS :: SwaggerPetstoreRequest req res -> BL.ByteString -> SwaggerPetstoreRequest req res
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

-- ** Mime Types

data MimeJSON deriving (P.Typeable)
data MimeXML deriving (P.Typeable)
data MimePlainText deriving (P.Typeable)
data MimeFormUrlEncoded deriving (P.Typeable)
data MimeMultipartFormData deriving (P.Typeable)
data MimeNoContent deriving (P.Typeable)
data MimeIdentityPassThough deriving (P.Typeable)


-- ** MimeType Class

class MimeType mtype  where
  mimeType :: P.Proxy mtype -> Maybe ME.MediaType
  mimeType p =
    case mimeTypes p of
      [] -> Nothing
      (x:_) -> Just x
  mimeTypes :: P.Proxy mtype -> [ME.MediaType]
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

instance MimeType MimeIdentityPassThough where
  mimeType _ = Nothing


-- ** MimeRender Class

class MimeType mtype => MimeRender mtype i o where
    mimeRender  :: P.Proxy mtype -> i -> o

-- ** MimeRender Instances

-- | `A.encode`
instance A.ToJSON a => MimeRender MimeJSON a BL.ByteString where mimeRender _ = A.encode
-- | @WH.urlEncodeAsForm@
instance WH.ToForm a => MimeRender MimeFormUrlEncoded a BL.ByteString where mimeRender _ = WH.urlEncodeAsForm
-- | `TL.encodeUtf8`
instance MimeRender MimePlainText TL.Text BL.ByteString where mimeRender _ = TL.encodeUtf8
-- | @BL.fromStrict . T.encodeUtf8@
instance MimeRender MimePlainText T.Text BL.ByteString where mimeRender _ = BL.fromStrict . T.encodeUtf8
-- | @BCL.pack@
instance MimeRender MimePlainText String BL.ByteString where mimeRender _ = BCL.pack
-- | @()@
instance MimeRender MimeNoContent a () where mimeRender _ _ = ()
-- | @P.id@
instance MimeRender MimeIdentityPassThough a a where mimeRender _ = P.id

-- ** MimeUnrender Class

class MimeType mtype => MimeUnrender mtype i o where
    mimeUnrender :: P.Proxy mtype -> i -> P.Either String o
    mimeUnrender p = mimeUnrenderWithType p (mimeType p)
    mimeUnrenderWithType :: P.Proxy mtype -> Maybe ME.MediaType -> i -> P.Either String o
    mimeUnrenderWithType p _ = mimeUnrender p
    {-# MINIMAL mimeUnrender | mimeUnrenderWithType #-}

-- ** MimeUnrender Instances


-- ** Request Consumes

class Consumes req mtype where

-- ** Request Produces

class Produces req mtype where