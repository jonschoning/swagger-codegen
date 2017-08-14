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

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BSR
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/pet"]
    params = []

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
    request = mkSwaggerPetstoreRequest "DELETE" pathSegments params
    pathSegments = ["/pet/",toPath petId]
    params = []

data DeletePet
instance HasOptionalParam DeletePet Api'Underscorekey where
  addOptionalParam req (Api'Underscorekey xs) = req { params = Query ("api_key", TE.encodeUtf8 xs) : params req}


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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/pet/findByStatus"]
    params = []

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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/pet/findByTags"]
    params = []
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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/pet/",toPath petId]
    params = []

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
    request = mkSwaggerPetstoreRequest "PUT" pathSegments params
    pathSegments = ["/pet"]
    params = []

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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/pet/",toPath petId]
    params = []

data UpdatePetWithForm

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name where
  addOptionalParam req (Name xs) = req { params = Query ("name", TE.encodeUtf8 xs) : params req}

-- | /Optional Param/ "status" - Updated status of the pet
instance HasOptionalParam UpdatePetWithForm Status where
  addOptionalParam req (Status xs) = req { params = Query ("status", TE.encodeUtf8 xs) : params req}


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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/pet/",toPath petId,"/uploadImage"]
    params = []

data UploadFile

-- | /Optional Param/ "additionalMetadata" - Additional data to pass to server
instance HasOptionalParam UploadFile AdditionalMetadata where
  addOptionalParam req (AdditionalMetadata xs) = req { params = Query ("additionalMetadata", TE.encodeUtf8 xs) : params req}

-- | /Optional Param/ "file" - file to upload
-- instance HasOptionalParam UploadFile File where
--   addOptionalParam req (File xs) = req { params = Query ("file", TE.encodeUtf8 xs) : params req}


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
    request = mkSwaggerPetstoreRequest "DELETE" pathSegments params
    pathSegments = ["/store/order/",toPath orderId]
    params = []

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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/store/inventory"]
    params = []

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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/store/order/",toPath orderId]
    params = []

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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/store/order"]
    params = []

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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/user"]
    params = []

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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/user/createWithArray"]
    params = []

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
    request = mkSwaggerPetstoreRequest "POST" pathSegments params
    pathSegments = ["/user/createWithList"]
    params = []

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
    request = mkSwaggerPetstoreRequest "DELETE" pathSegments params
    pathSegments = ["/user/",toPath username]
    params = []

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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/user/",toPath username]
    params = []

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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/user/login"]
    params = []

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
    request = mkSwaggerPetstoreRequest "GET" pathSegments params
    pathSegments = ["/user/logout"]
    params = []

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
    request = mkSwaggerPetstoreRequest "PUT" pathSegments params
    pathSegments = ["/user/",toPath username]
    params = []

data UpdateUser


-- * SwaggerPetstoreRequest
-- | Represents a request. The "req" type variable is the request type. The "res" type variable is the response type.
data SwaggerPetstoreRequest req res = SwaggerPetstoreRequest
  { rMethod  :: NH.Method   -- ^ Method of SwaggerPetstoreRequest
  , pathSegments :: [BS8.ByteString]     -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: [EncodedParam] -- ^ Encoded params of SwaggerPetstoreRequest
  }
  deriving (Show)

mkSwaggerPetstoreRequest :: NH.Method
                  -> [BS8.ByteString]
                  -> [EncodedParam]
                  -> SwaggerPetstoreRequest req res
mkSwaggerPetstoreRequest m e p = SwaggerPetstoreRequest m e p

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

-- ** EncodedParam
-- | The "wire" encoding of a param
data EncodedParam
  = Query TupleBS8
  | Body BSL.ByteString
  | Header TupleBS8
  deriving (Show)

data ResultFormatType
  = FormatJson
  | FormatXml
  deriving (Show, Eq)

toPath
  :: (Show a)
  => a -> BS8.ByteString
toPath x = NH.urlEncode False (BS8.pack (show x))

-- * Optional Request Params

newtype Api'Underscorekey = Api'Underscorekey { unApi'Underscorekey :: Text } deriving (Eq, Show)

newtype Name = Name { unName :: Text } deriving (Eq, Show)

newtype Status = Status { unStatus :: Text } deriving (Eq, Show)

newtype AdditionalMetadata = AdditionalMetadata { unAdditionalMetadata :: Text } deriving (Eq, Show)

newtype File = File { unFile :: FilePath } deriving (Eq, Show)
