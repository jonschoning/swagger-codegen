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
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Model as M

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Void as Void

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types.Method as NH
import qualified Network.HTTP.Types as NH
import qualified Network.HTTP.Types.URI as NH

import qualified Web.HttpApiData as WH
import qualified Web.FormUrlEncoded as WF

import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Base ((<|>))
import GHC.Exts (IsString(..))
import Prelude (($), (.),(<$>),(<*>),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty)
import qualified Data.Foldable as P
import qualified Data.Functor.Identity as P (Identity(..))
import qualified Data.Maybe as P
import qualified GHC.Base as P (Alternative)
import qualified Prelude as P
import qualified Data.CaseInsensitive as CI

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
-- Consumes: application/json, application/xml
-- 
-- Produces: application/xml, application/json
addPet 
  :: Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest AddPet ()
addPet body =
  _mkRequest "POST" ["/pet"]
    `_setBodyLBS` A.encode body

data AddPet
-- instance Consumes AddPet application/json
-- instance Consumes AddPet application/xml
-- instance Produces AddPet application/xml
-- instance Produces AddPet application/json


-- ** deletePet

-- | DELETE \/pet\/{petId}
-- 
-- Deletes a pet
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
-- Produces: application/xml, application/json
deletePet 
  :: Integer -- ^ "petId" -  Pet id to delete
  -> SwaggerPetstoreRequest DeletePet ()
deletePet petId =
  _mkRequest "DELETE" ["/pet/",toPath petId]
    

data DeletePet
-- instance Produces DeletePet application/xml
-- instance Produces DeletePet application/json
instance HasOptionalParam DeletePet Api'Underscorekey where
  applyOptionalParam req (Api'Underscorekey xs) =
    req `_addHeader` toHeader ("api_key", xs)


-- ** findPetsByStatus

-- | GET \/pet\/findByStatus
-- 
-- Finds Pets by status
-- 
-- Multiple status values can be provided with comma separated strings
-- 
-- AuthMethod: petstore_auth
-- 
-- Produces: application/xml, application/json
findPetsByStatus 
  :: [Text] -- ^ "status" -  Status values that need to be considered for filter
  -> SwaggerPetstoreRequest FindPetsByStatus [Pet]
findPetsByStatus status =
  _mkRequest "GET" ["/pet/findByStatus"]
    `_addQuery` toQueryMulti MultiParamArray ("status", Just status)

data FindPetsByStatus
-- instance Produces FindPetsByStatus application/xml
-- instance Produces FindPetsByStatus application/json


-- ** findPetsByTags

-- | GET \/pet\/findByTags
-- 
-- Finds Pets by tags
-- 
-- Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-- 
-- AuthMethod: petstore_auth
-- 
-- Produces: application/xml, application/json
findPetsByTags 
  :: [Text] -- ^ "tags" -  Tags to filter by
  -> SwaggerPetstoreRequest FindPetsByTags [Pet]
findPetsByTags tags =
  _mkRequest "GET" ["/pet/findByTags"]
    `_addQuery` toQueryMulti MultiParamArray ("tags", Just tags)

{-# DEPRECATED findPetsByTags "" #-}

data FindPetsByTags
-- instance Produces FindPetsByTags application/xml
-- instance Produces FindPetsByTags application/json


-- ** getPetById

-- | GET \/pet\/{petId}
-- 
-- Find pet by ID
-- 
-- Returns a single pet
-- 
-- AuthMethod: api_key
-- 
-- Produces: application/xml, application/json
getPetById 
  :: Integer -- ^ "petId" -  ID of pet to return
  -> SwaggerPetstoreRequest GetPetById Pet
getPetById petId =
  _mkRequest "GET" ["/pet/",toPath petId]
    

data GetPetById
-- instance Produces GetPetById application/xml
-- instance Produces GetPetById application/json


-- ** updatePet

-- | PUT \/pet
-- 
-- Update an existing pet
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
-- Consumes: application/json, application/xml
-- 
-- Produces: application/xml, application/json
updatePet 
  :: Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest UpdatePet ()
updatePet body =
  _mkRequest "PUT" ["/pet"]
    `_setBodyLBS` A.encode body

data UpdatePet
-- instance Consumes UpdatePet application/json
-- instance Consumes UpdatePet application/xml
-- instance Produces UpdatePet application/xml
-- instance Produces UpdatePet application/json


-- ** updatePetWithForm

-- | POST \/pet\/{petId}
-- 
-- Updates a pet in the store with form data
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
-- Consumes: application/x-www-form-urlencoded
-- 
-- Produces: application/xml, application/json
updatePetWithForm 
  :: Integer -- ^ "petId" -  ID of pet that needs to be updated
  -> SwaggerPetstoreRequest UpdatePetWithForm ()
updatePetWithForm petId =
  _mkRequest "POST" ["/pet/",toPath petId]
    

data UpdatePetWithForm
-- instance Consumes UpdatePetWithForm application/x-www-form-urlencoded
-- instance Produces UpdatePetWithForm application/xml
-- instance Produces UpdatePetWithForm application/json

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name where
  applyOptionalParam req (Name xs) =
    req `_addForm` toForm ("name", xs)

-- | /Optional Param/ "status" - Updated status of the pet
instance HasOptionalParam UpdatePetWithForm Status where
  applyOptionalParam req (Status xs) =
    req `_addForm` toForm ("status", xs)


-- ** uploadFile

-- | POST \/pet\/{petId}\/uploadImage
-- 
-- uploads an image
-- 
-- 
-- 
-- AuthMethod: petstore_auth
-- 
-- Consumes: multipart/form-data
-- 
-- Produces: application/json
uploadFile 
  :: Integer -- ^ "petId" -  ID of pet to update
  -> SwaggerPetstoreRequest UploadFile ApiResponse
uploadFile petId =
  _mkRequest "POST" ["/pet/",toPath petId,"/uploadImage"]
    

data UploadFile
-- instance Consumes UploadFile multipart/form-data
-- instance Produces UploadFile application/json

-- | /Optional Param/ "additionalMetadata" - Additional data to pass to server
instance HasOptionalParam UploadFile AdditionalMetadata where
  applyOptionalParam req (AdditionalMetadata xs) =
    req `_addForm` toForm ("additionalMetadata", xs)

-- | /Optional Param/ "file" - file to upload
instance HasOptionalParam UploadFile File where
  applyOptionalParam req (File xs) =
    req `_addForm` toForm ("file", xs)


-- ** deleteOrder

-- | DELETE \/store\/order\/{orderId}
-- 
-- Delete purchase order by ID
-- 
-- For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
-- 
-- Produces: application/xml, application/json
deleteOrder 
  :: Integer -- ^ "orderId" -  ID of the order that needs to be deleted
  -> SwaggerPetstoreRequest DeleteOrder ()
deleteOrder orderId =
  _mkRequest "DELETE" ["/store/order/",toPath orderId]
    

data DeleteOrder
-- instance Produces DeleteOrder application/xml
-- instance Produces DeleteOrder application/json


-- ** getInventory

-- | GET \/store\/inventory
-- 
-- Returns pet inventories by status
-- 
-- Returns a map of status codes to quantities
-- 
-- AuthMethod: api_key
-- 
-- Produces: application/json
getInventory 
  :: SwaggerPetstoreRequest GetInventory (Map.Map String Int)
getInventory =
  _mkRequest "GET" ["/store/inventory"]

data GetInventory
-- instance Produces GetInventory application/json


-- ** getOrderById

-- | GET \/store\/order\/{orderId}
-- 
-- Find purchase order by ID
-- 
-- For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions
-- 
-- Produces: application/xml, application/json
getOrderById 
  :: Integer -- ^ "orderId" -  ID of pet that needs to be fetched
  -> SwaggerPetstoreRequest GetOrderById Order
getOrderById orderId =
  _mkRequest "GET" ["/store/order/",toPath orderId]
    

data GetOrderById
-- instance Produces GetOrderById application/xml
-- instance Produces GetOrderById application/json


-- ** placeOrder

-- | POST \/store\/order
-- 
-- Place an order for a pet
-- 
-- 
-- 
-- Produces: application/xml, application/json
placeOrder 
  :: Order -- ^ "body" -  order placed for purchasing the pet
  -> SwaggerPetstoreRequest PlaceOrder Order
placeOrder body =
  _mkRequest "POST" ["/store/order"]
    `_setBodyLBS` A.encode body

data PlaceOrder
-- instance Produces PlaceOrder application/xml
-- instance Produces PlaceOrder application/json


-- ** createUser

-- | POST \/user
-- 
-- Create user
-- 
-- This can only be done by the logged in user.
-- 
-- Produces: application/xml, application/json
createUser 
  :: User -- ^ "body" -  Created user object
  -> SwaggerPetstoreRequest CreateUser ()
createUser body =
  _mkRequest "POST" ["/user"]
    `_setBodyLBS` A.encode body

data CreateUser
-- instance Produces CreateUser application/xml
-- instance Produces CreateUser application/json


-- ** createUsersWithArrayInput

-- | POST \/user\/createWithArray
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
-- Produces: application/xml, application/json
createUsersWithArrayInput 
  :: [User] -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest CreateUsersWithArrayInput ()
createUsersWithArrayInput body =
  _mkRequest "POST" ["/user/createWithArray"]
    `_setBodyLBS` A.encode body

data CreateUsersWithArrayInput
-- instance Produces CreateUsersWithArrayInput application/xml
-- instance Produces CreateUsersWithArrayInput application/json


-- ** createUsersWithListInput

-- | POST \/user\/createWithList
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
-- Produces: application/xml, application/json
createUsersWithListInput 
  :: [User] -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest CreateUsersWithListInput ()
createUsersWithListInput body =
  _mkRequest "POST" ["/user/createWithList"]
    `_setBodyLBS` A.encode body

data CreateUsersWithListInput
-- instance Produces CreateUsersWithListInput application/xml
-- instance Produces CreateUsersWithListInput application/json


-- ** deleteUser

-- | DELETE \/user\/{username}
-- 
-- Delete user
-- 
-- This can only be done by the logged in user.
-- 
-- Produces: application/xml, application/json
deleteUser 
  :: Text -- ^ "username" -  The name that needs to be deleted
  -> SwaggerPetstoreRequest DeleteUser ()
deleteUser username =
  _mkRequest "DELETE" ["/user/",toPath username]
    

data DeleteUser
-- instance Produces DeleteUser application/xml
-- instance Produces DeleteUser application/json


-- ** getUserByName

-- | GET \/user\/{username}
-- 
-- Get user by user name
-- 
-- 
-- 
-- Produces: application/xml, application/json
getUserByName 
  :: Text -- ^ "username" -  The name that needs to be fetched. Use user1 for testing. 
  -> SwaggerPetstoreRequest GetUserByName User
getUserByName username =
  _mkRequest "GET" ["/user/",toPath username]
    

data GetUserByName
-- instance Produces GetUserByName application/xml
-- instance Produces GetUserByName application/json


-- ** loginUser

-- | GET \/user\/login
-- 
-- Logs user into the system
-- 
-- 
-- 
-- Produces: application/xml, application/json
loginUser 
  :: Text -- ^ "username" -  The user name for login
  -> Text -- ^ "password" -  The password for login in clear text
  -> SwaggerPetstoreRequest LoginUser Text
loginUser username password =
  _mkRequest "GET" ["/user/login"]
    `_addQuery` toQuery ("username", Just username)
    `_addQuery` toQuery ("password", Just password)

data LoginUser
-- instance Produces LoginUser application/xml
-- instance Produces LoginUser application/json


-- ** logoutUser

-- | GET \/user\/logout
-- 
-- Logs out current logged in user session
-- 
-- 
-- 
-- Produces: application/xml, application/json
logoutUser 
  :: SwaggerPetstoreRequest LogoutUser ()
logoutUser =
  _mkRequest "GET" ["/user/logout"]

data LogoutUser
-- instance Produces LogoutUser application/xml
-- instance Produces LogoutUser application/json


-- ** updateUser

-- | PUT \/user\/{username}
-- 
-- Updated user
-- 
-- This can only be done by the logged in user.
-- 
-- Produces: application/xml, application/json
updateUser 
  :: Text -- ^ "username" -  name that need to be updated
  -> User -- ^ "body" -  Updated user object
  -> SwaggerPetstoreRequest UpdateUser ()
updateUser username body =
  _mkRequest "PUT" ["/user/",toPath username]
    
    `_setBodyLBS` A.encode body

data UpdateUser
-- instance Produces UpdateUser application/xml
-- instance Produces UpdateUser application/json

 
-- * SwaggerPetstoreRequest

-- | Represents a request. The "req" type variable is the request type. The "res" type variable is the response type.
data SwaggerPetstoreRequest req res = SwaggerPetstoreRequest
  { rMethod  :: NH.Method   -- ^ Method of SwaggerPetstoreRequest
  , urlPath :: [BSL.ByteString] -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: Params -- ^ params of SwaggerPetstoreRequest
  }
  deriving (P.Show)


-- * SwaggerPetstoreRequest Helpers

_mkRequest :: NH.Method -- ^ Method 
          -> [BSL.ByteString] -- ^ Endpoint
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

_addForm :: SwaggerPetstoreRequest req res -> WF.Form -> SwaggerPetstoreRequest req res
_addForm req newform = 
    let _params = params req
        form = case paramsBody _params of
            ParamBodyForm _form -> _form
            _ -> mempty
    in req { params = _params { paramsBody = ParamBodyForm (newform <> form) } }

_addMultiFormPart :: SwaggerPetstoreRequest req res -> NH.Part -> SwaggerPetstoreRequest req res
_addMultiFormPart req newpart = 
    let _params = params req
        parts = case paramsBody _params of
            ParamBodyMultiForm _parts -> _parts
            _ -> []
    in req { params = _params { paramsBody = ParamBodyMultiForm (newpart : parts) } }

_setBodyBS :: SwaggerPetstoreRequest req res -> B.ByteString -> SwaggerPetstoreRequest req res
_setBodyBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyBS body } }

_setBodyLBS :: SwaggerPetstoreRequest req res -> BSL.ByteString -> SwaggerPetstoreRequest req res
_setBodyLBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyBSL body } }



-- * Params

-- ** Consumes
class Consumes req content where

-- ** Produces
class Produces req accept where

-- ** HasOptionalParam
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

-- | Request Params
data Params = Params
  { paramsQuery :: NH.Query
  , paramsHeaders :: NH.RequestHeaders
  , paramsBody :: ParamBody
  }
  deriving (P.Show)

data ParamBody
  = ParamBodyNone
  | ParamBodyForm WF.Form
  | ParamBodyMultiForm [NH.Part]
  | ParamBodyBS B.ByteString
  | ParamBodyBSL BSL.ByteString
  deriving (P.Show)

toPath
  :: WH.ToHttpApiData a
  => a -> BSL.ByteString
toPath = BSB.toLazyByteString . WH.toEncodedUrlPiece

toHeader :: WH.ToHttpApiData a => (NH.HeaderName, a) -> [NH.Header]
toHeader x = [fmap WH.toHeader x]

toForm :: WH.ToHttpApiData v => (BS8.ByteString, v) -> WF.Form
toForm (k,v) = WF.toForm [(BS8.unpack k,v)]

toQuery :: WH.ToHttpApiData a => (BS8.ByteString, Maybe a) -> [NH.QueryItem]
toQuery x = [(fmap . fmap) toQueryParam x]
  where toQueryParam = TE.encodeUtf8 . WH.toQueryParam

toHeaderMulti :: WH.ToHttpApiData a => CollectionFormat -> (NH.HeaderName, [a]) -> [NH.Header]
toHeaderMulti c xs = _toMulti c toHeader xs

toFormMulti :: WH.ToHttpApiData v => CollectionFormat -> (BS8.ByteString, [v]) -> WF.Form
toFormMulti c xs = WF.toForm $ fmap unpack $ _toMulti c toHeader $ pack xs
  where
    pack (k,v) = (CI.mk k, v)
    unpack (k,v) = (BS8.unpack (CI.original k), BS8.unpack v)

toQueryMulti :: WH.ToHttpApiData a => CollectionFormat -> (BS8.ByteString, Maybe [a]) -> NH.Query
toQueryMulti c xs = _toMultiA c toQuery xs

_toMulti :: P.Traversable f => CollectionFormat -> (f a -> [(b, BS8.ByteString)]) -> f [a] -> [(b, BS8.ByteString)]
_toMulti c encode xs = _toMulti' c encode BS8.singleton xs

_toMulti' :: (P.Monoid c, P.Traversable f) => CollectionFormat -> (f a -> [(b, c)]) -> (Char -> c) -> f [a] -> [(b, c)]
_toMulti' c encode one xs = fmap (fmap P.fromJust) (_toMultiA' c fencode one (fmap Just xs))
  where fencode = fmap (fmap Just) . encode . fmap P.fromJust
        {-# INLINE fencode #-}

_toMultiA :: (P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t BS8.ByteString)]) -> f (t [a]) -> [(b, t BS8.ByteString)]
_toMultiA c encode xs = _toMultiA' c encode BS8.singleton xs

_toMultiA' :: (P.Monoid c, P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t c)]) -> (Char -> c) -> f (t [a]) -> [(b, t c)]
_toMultiA' c encode one xs = case c of
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
  
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

showBS
  :: P.Show a
  => a -> B.ByteString
showBS = fromString . P.show

-- * Optional Request Params

newtype Api'Underscorekey = Api'Underscorekey { unApi'Underscorekey :: Text } deriving (P.Eq, P.Show)

newtype Name = Name { unName :: Text } deriving (P.Eq, P.Show)

newtype Status = Status { unStatus :: Text } deriving (P.Eq, P.Show)

newtype AdditionalMetadata = AdditionalMetadata { unAdditionalMetadata :: Text } deriving (P.Eq, P.Show)

newtype File = File { unFile :: FilePath } deriving (P.Eq, P.Show)
