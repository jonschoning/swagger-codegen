{-|
Module : SwaggerPetstore.API
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Function ((&))
import Data.Text (Text)
import GHC.Exts (IsString(..))

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

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

import Prelude 

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
addPet body = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/pet"]
    params = mkEncParams

data AddPet


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
deletePet petId = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" urlPath params
    urlPath = ["/pet/",toPath petId]
    params = mkEncParams

data DeletePet
instance HasOptionalParam DeletePet Api'Underscorekey where
  addOptionalParam req (Api'Underscorekey xs) =
    addHeaders req [("api_key", toBS8 xs)]


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
findPetsByStatus status = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/pet/findByStatus"]
    params = mkEncParams

data FindPetsByStatus


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
findPetsByTags tags = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/pet/findByTags"]
    params = mkEncParams
{-# DEPRECATED findPetsByTags "" #-}

data FindPetsByTags


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
getPetById petId = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/pet/",toPath petId]
    params = mkEncParams

data GetPetById


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
updatePet body = request
  where
    request = mkSwaggerPetstoreRequest "PUT" urlPath params
    urlPath = ["/pet"]
    params = mkEncParams

data UpdatePet


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
updatePetWithForm petId = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/pet/",toPath petId]
    params = mkEncParams

data UpdatePetWithForm

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name where
  addOptionalParam req (Name xs) =
    addFormUrlEncFields req [("name", Just (toBS8 xs))]

-- | /Optional Param/ "status" - Updated status of the pet
instance HasOptionalParam UpdatePetWithForm Status where
  addOptionalParam req (Status xs) =
    addFormUrlEncFields req [("status", Just (toBS8 xs))]


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
uploadFile petId = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/pet/",toPath petId,"/uploadImage"]
    params = mkEncParams

data UploadFile

-- | /Optional Param/ "additionalMetadata" - Additional data to pass to server
instance HasOptionalParam UploadFile AdditionalMetadata where
  addOptionalParam req (AdditionalMetadata xs) =
    addFormUrlEncFields req [("additionalMetadata", Just (toBS8 xs))]

-- | /Optional Param/ "file" - file to upload
instance HasOptionalParam UploadFile File where
  addOptionalParam req (File xs) =
    addFormUrlEncFields req [("file", Just (toBS8 xs))]


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
deleteOrder orderId = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" urlPath params
    urlPath = ["/store/order/",toPath orderId]
    params = mkEncParams

data DeleteOrder


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
getInventory = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/store/inventory"]
    params = mkEncParams

data GetInventory


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
getOrderById orderId = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/store/order/",toPath orderId]
    params = mkEncParams

data GetOrderById


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
placeOrder body = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/store/order"]
    params = mkEncParams

data PlaceOrder


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
createUser body = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/user"]
    params = mkEncParams

data CreateUser


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
createUsersWithArrayInput body = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/user/createWithArray"]
    params = mkEncParams

data CreateUsersWithArrayInput


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
createUsersWithListInput body = request
  where
    request = mkSwaggerPetstoreRequest "POST" urlPath params
    urlPath = ["/user/createWithList"]
    params = mkEncParams

data CreateUsersWithListInput


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
deleteUser username = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" urlPath params
    urlPath = ["/user/",toPath username]
    params = mkEncParams

data DeleteUser


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
getUserByName username = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/user/",toPath username]
    params = mkEncParams

data GetUserByName


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
loginUser username password = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/user/login"]
    params = mkEncParams

data LoginUser


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
logoutUser = request
  where
    request = mkSwaggerPetstoreRequest "GET" urlPath params
    urlPath = ["/user/logout"]
    params = mkEncParams

data LogoutUser


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
updateUser username body = request
  where
    request = mkSwaggerPetstoreRequest "PUT" urlPath params
    urlPath = ["/user/",toPath username]
    params = mkEncParams

data UpdateUser


-- * SwaggerPetstoreRequest

-- | Represents a request. The "req" type variable is the request type. The "res" type variable is the response type.
data SwaggerPetstoreRequest req res = SwaggerPetstoreRequest
  { rMethod  :: NH.Method   -- ^ Method of SwaggerPetstoreRequest
  , urlPath :: [BS8.ByteString] -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: EncParams -- ^ Encoded params of SwaggerPetstoreRequest
  }
  deriving (Show)


mkSwaggerPetstoreRequest :: NH.Method -- ^ Method 
                  -> [BS8.ByteString] -- ^ Endpoint
                  -> EncParams  -- ^ Encoded params
                  -> SwaggerPetstoreRequest req res -- ^ req: Request Type, res: Response Type
mkSwaggerPetstoreRequest m u p = SwaggerPetstoreRequest m u p

addHeaders :: SwaggerPetstoreRequest req res -> [NH.Header] -> SwaggerPetstoreRequest req res
addHeaders req header = 
    let encParams = params req
    in req { params = encParams { encParamsHeaders = header ++ encParamsHeaders encParams } }

addQuery :: SwaggerPetstoreRequest req res -> NH.Query -> SwaggerPetstoreRequest req res
addQuery req query = 
    let encParams = params req
    in req { params = encParams { encParamsQuery = query ++ encParamsQuery encParams } }

addBodyBS :: SwaggerPetstoreRequest req res -> B.ByteString -> SwaggerPetstoreRequest req res
addBodyBS req body = 
    let encParams = params req
    in req { params = encParams { encParamsBody = EncBodyBS body } }

addFormUrlEncFields :: SwaggerPetstoreRequest req res -> NH.Query -> SwaggerPetstoreRequest req res
addFormUrlEncFields req field = 
    let encParams = params req
        EncBodyFormUrlEnc fields = encParamsBody encParams
    in req { params = encParams { encParamsBody = EncBodyFormUrlEnc (field ++ fields) } }

addMultiFormParts :: SwaggerPetstoreRequest req res -> [NH.Part] -> SwaggerPetstoreRequest req res
addMultiFormParts req newparts = 
    let encParams = params req
        EncBodyMultiForm parts = encParamsBody encParams
    in req { params = encParams { encParamsBody = EncBodyMultiForm (newparts ++ parts) } }


-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- * Params

-- ** HasOptionalParam
-- | Designates the optional parameters of a request
class HasOptionalParam req param where
  {-# MINIMAL addOptionalParam | (-&-) #-}

  -- | Add an optional parameter to a request
  addOptionalParam :: forall res. SwaggerPetstoreRequest req res -> param -> SwaggerPetstoreRequest req res
  addOptionalParam = (-&-)
  {-# INLINE addOptionalParam #-}

  -- | infix operator \/ alias for 'addOptionalParam'
  (-&-) :: forall res. SwaggerPetstoreRequest req res -> param -> SwaggerPetstoreRequest req res
  (-&-) = addOptionalParam
  {-# INLINE (-&-) #-}

infixl 2 -&-

-- | The "wire" encoding of a param
data EncParams = EncParams
  { encParamsQuery :: NH.Query
  , encParamsHeaders :: NH.RequestHeaders
  , encParamsBody :: EncBody
  }
  deriving (Show)

mkEncParams :: EncParams
mkEncParams = EncParams [] [] EncBodyNone

data EncBody
  = EncBodyNone
  | EncBodyBS B.ByteString
  | EncBodyBSL BSL.ByteString
  | EncBodyFormUrlEnc NH.Query
  | EncBodyMultiForm [NH.Part]
  deriving (Show)

data ResultFormatType
  = FormatJson
  | FormatXml
  deriving (Show, Eq)


toPath
  :: Show a
  => a -> BS8.ByteString
toPath x = NH.urlEncode False (BS8.pack (show x))

toBS8
  :: Show a
  => a -> BS8.ByteString
toBS8 = TE.encodeUtf8 . T.pack . show

toBS
  :: Show a
  => a -> B.ByteString
toBS = TE.encodeUtf8 . T.pack . show

-- * Optional Request Params

newtype Api'Underscorekey = Api'Underscorekey { unApi'Underscorekey :: Text } deriving (Eq, Show)

newtype Name = Name { unName :: Text } deriving (Eq, Show)

newtype Status = Status { unStatus :: Text } deriving (Eq, Show)

newtype AdditionalMetadata = AdditionalMetadata { unAdditionalMetadata :: Text } deriving (Eq, Show)

newtype File = File { unFile :: FilePath } deriving (Eq, Show)
