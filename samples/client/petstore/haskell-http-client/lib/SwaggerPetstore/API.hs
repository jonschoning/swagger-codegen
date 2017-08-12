{-|
Module : SwaggerPetstore.API
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Model

import Control.Monad.IO.Class
import Data.Aeson 
import Data.Aeson.Types 
import Data.Function ((&))
import Data.Text (Text)
import GHC.Exts (IsString(..))

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method as NHTM

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
-- 
addPet 
  :: Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest ()
addPet body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/pet"]
    params = []

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
-- 
-- Optional Params:
-- 
-- * "api_key" :: 'Text' 
-- 
deletePet 
  :: Integer -- ^ "petId" -  Pet id to delete
  -> SwaggerPetstoreRequest ()
deletePet petId = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" url params
    url = ["/pet/",toPath petId]
    params = []

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
-- 
findPetsByStatus 
  :: [Text] -- ^ "status" -  Status values that need to be considered for filter
  -> SwaggerPetstoreRequest [Pet]
findPetsByStatus status = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/pet/findByStatus"]
    params = []

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
-- 
findPetsByTags 
  :: [Text] -- ^ "tags" -  Tags to filter by
  -> SwaggerPetstoreRequest [Pet]
findPetsByTags tags = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/pet/findByTags"]
    params = []
{-# DEPRECATED findPetsByTags "" #-}

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
-- 
getPetById 
  :: Integer -- ^ "petId" -  ID of pet to return
  -> SwaggerPetstoreRequest Pet
getPetById petId = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/pet/",toPath petId]
    params = []

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
-- 
updatePet 
  :: Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest ()
updatePet body = request
  where
    request = mkSwaggerPetstoreRequest "PUT" url params
    url = ["/pet"]
    params = []

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
-- 
-- Optional Params:
-- 
-- * "name" :: 'Text'  - Updated name of the pet
-- 
-- * "status" :: 'Text'  - Updated status of the pet
-- 
updatePetWithForm 
  :: Integer -- ^ "petId" -  ID of pet that needs to be updated
  -> SwaggerPetstoreRequest ()
updatePetWithForm petId = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/pet/",toPath petId]
    params = []

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
-- 
-- Optional Params:
-- 
-- * "additionalMetadata" :: 'Text'  - Additional data to pass to server
-- 
-- * "file" :: 'FilePath'  - file to upload
-- 
uploadFile 
  :: Integer -- ^ "petId" -  ID of pet to update
  -> SwaggerPetstoreRequest ApiResponse
uploadFile petId = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/pet/",toPath petId,"/uploadImage"]
    params = []



-- ** deleteOrder
-- | DELETE \/store\/order\/{orderId}
-- 
-- Delete purchase order by ID
-- 
-- For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
-- 
-- Produces: application/xml, application/json
-- 
deleteOrder 
  :: Integer -- ^ "orderId" -  ID of the order that needs to be deleted
  -> SwaggerPetstoreRequest ()
deleteOrder orderId = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" url params
    url = ["/store/order/",toPath orderId]
    params = []

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
-- 
getInventory 
  :: SwaggerPetstoreRequest (Map.Map String Int)
getInventory = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/store/inventory"]
    params = []

-- ** getOrderById
-- | GET \/store\/order\/{orderId}
-- 
-- Find purchase order by ID
-- 
-- For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions
-- 
-- Produces: application/xml, application/json
-- 
getOrderById 
  :: Integer -- ^ "orderId" -  ID of pet that needs to be fetched
  -> SwaggerPetstoreRequest Order
getOrderById orderId = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/store/order/",toPath orderId]
    params = []

-- ** placeOrder
-- | POST \/store\/order
-- 
-- Place an order for a pet
-- 
-- 
-- 
-- Produces: application/xml, application/json
-- 
placeOrder 
  :: Order -- ^ "body" -  order placed for purchasing the pet
  -> SwaggerPetstoreRequest Order
placeOrder body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/store/order"]
    params = []



-- ** createUser
-- | POST \/user
-- 
-- Create user
-- 
-- This can only be done by the logged in user.
-- 
-- Produces: application/xml, application/json
-- 
createUser 
  :: User -- ^ "body" -  Created user object
  -> SwaggerPetstoreRequest ()
createUser body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/user"]
    params = []

-- ** createUsersWithArrayInput
-- | POST \/user\/createWithArray
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
-- Produces: application/xml, application/json
-- 
createUsersWithArrayInput 
  :: [User] -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest ()
createUsersWithArrayInput body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/user/createWithArray"]
    params = []

-- ** createUsersWithListInput
-- | POST \/user\/createWithList
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
-- Produces: application/xml, application/json
-- 
createUsersWithListInput 
  :: [User] -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest ()
createUsersWithListInput body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = ["/user/createWithList"]
    params = []

-- ** deleteUser
-- | DELETE \/user\/{username}
-- 
-- Delete user
-- 
-- This can only be done by the logged in user.
-- 
-- Produces: application/xml, application/json
-- 
deleteUser 
  :: Text -- ^ "username" -  The name that needs to be deleted
  -> SwaggerPetstoreRequest ()
deleteUser username = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" url params
    url = ["/user/",toPath username]
    params = []

-- ** getUserByName
-- | GET \/user\/{username}
-- 
-- Get user by user name
-- 
-- 
-- 
-- Produces: application/xml, application/json
-- 
getUserByName 
  :: Text -- ^ "username" -  The name that needs to be fetched. Use user1 for testing. 
  -> SwaggerPetstoreRequest User
getUserByName username = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/user/",toPath username]
    params = []

-- ** loginUser
-- | GET \/user\/login
-- 
-- Logs user into the system
-- 
-- 
-- 
-- Produces: application/xml, application/json
-- 
loginUser 
  :: Text -- ^ "username" -  The user name for login
  -> Text -- ^ "password" -  The password for login in clear text
  -> SwaggerPetstoreRequest Text
loginUser username password = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/user/login"]
    params = []

-- ** logoutUser
-- | GET \/user\/logout
-- 
-- Logs out current logged in user session
-- 
-- 
-- 
-- Produces: application/xml, application/json
-- 
logoutUser 
  :: SwaggerPetstoreRequest ()
logoutUser = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = ["/user/logout"]
    params = []

-- ** updateUser
-- | PUT \/user\/{username}
-- 
-- Updated user
-- 
-- This can only be done by the logged in user.
-- 
-- Produces: application/xml, application/json
-- 
updateUser 
  :: Text -- ^ "username" -  name that need to be updated
  -> User -- ^ "body" -  Updated user object
  -> SwaggerPetstoreRequest ()
updateUser username body = request
  where
    request = mkSwaggerPetstoreRequest "PUT" url params
    url = ["/user/",toPath username]
    params = []



-- * SwaggerPetstoreRequest

data SwaggerPetstoreRequest r = SwaggerPetstoreRequest
  { rMethod  :: NHTM.Method   -- ^ Method of SwaggerPetstoreRequest
  , endpoint :: [Text]     -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: [EncodedParam] -- ^ Encoded params of SwaggerPetstoreRequest
  }
  deriving (Show)

mkSwaggerPetstoreRequest :: NHTM.Method
                  -> [Text]
                  -> [EncodedParam]
                  -> SwaggerPetstoreRequest r
mkSwaggerPetstoreRequest m e p = SwaggerPetstoreRequest m e p

-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- * EncodedParam

data EncodedParam
  = Query TupleBS8
  | Body BSL.ByteString
  | Header TupleBS8
  deriving (Show)

data ResultFormatType
  = FormatJson
  | FormatXml
  deriving (Show, Eq)

toPath _ = "toPath"
