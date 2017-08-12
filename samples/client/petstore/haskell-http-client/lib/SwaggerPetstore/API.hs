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

-- * Request Params

data SwaggerPetstoreRequest = SwaggerPetstoreRequest
  { rMethod  :: NHTM.Method   -- ^ Method of SwaggerPetstoreRequest
  , endpoint :: Text     -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: [Params] -- ^ Request params of SwaggerPetstoreRequest
  }
  deriving (Show)

mkSwaggerPetstoreRequest :: NHTM.Method
                  -> Text
                  -> [Params]
                  -> SwaggerPetstoreRequest
mkSwaggerPetstoreRequest m e p = SwaggerPetstoreRequest m e p

data Params
  = Query TupleBS8
  | Body BSL.ByteString
  deriving (Show)

data ResultFormatType
  = FormatJson
  | FormatXml
  deriving (Show, Eq)

-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

toPath _ = "toPath"

-- * Operations


-- | /pet
-- Add a new pet to the store
-- 
-- body :: Pet  - Required - Pet object that needs to be added to the store
addPet 
  :: Pet -- ^ 'body': Pet object that needs to be added to the store
  -> ()
addPet body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/pet"]
    params = []

-- | /pet/{petId}
-- Deletes a pet
-- 
-- petId :: Integer  - Required - Pet id to delete
-- api_key :: Maybe Text 
deletePet 
  :: Integer -- ^ 'petId': Pet id to delete
  -> ()
deletePet petId = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" url params
    url = T.concat ["/pet/",toPath petId]
    params = []

-- | /pet/findByStatus
-- Finds Pets by status
-- Multiple status values can be provided with comma separated strings
-- status :: [Text]  - Required - Status values that need to be considered for filter
findPetsByStatus 
  :: [Text] -- ^ 'status': Status values that need to be considered for filter
  -> [Pet]
findPetsByStatus status = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/pet/findByStatus"]
    params = []

-- | /pet/findByTags
-- Finds Pets by tags
-- Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-- tags :: [Text]  - Required - Tags to filter by
findPetsByTags 
  :: [Text] -- ^ 'tags': Tags to filter by
  -> [Pet]
findPetsByTags tags = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/pet/findByTags"]
    params = []
{-# DEPRECATED findPetsByTags #-}

-- | /pet/{petId}
-- Find pet by ID
-- Returns a single pet
-- petId :: Integer  - Required - ID of pet to return
getPetById 
  :: Integer -- ^ 'petId': ID of pet to return
  -> Pet
getPetById petId = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/pet/",toPath petId]
    params = []

-- | /pet
-- Update an existing pet
-- 
-- body :: Pet  - Required - Pet object that needs to be added to the store
updatePet 
  :: Pet -- ^ 'body': Pet object that needs to be added to the store
  -> ()
updatePet body = request
  where
    request = mkSwaggerPetstoreRequest "PUT" url params
    url = T.concat ["/pet"]
    params = []

-- | /pet/{petId}
-- Updates a pet in the store with form data
-- 
-- petId :: Integer  - Required - ID of pet that needs to be updated
-- name :: Maybe Text  - Updated name of the pet
-- status :: Maybe Text  - Updated status of the pet
updatePetWithForm 
  :: Integer -- ^ 'petId': ID of pet that needs to be updated
  -> ()
updatePetWithForm petId = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/pet/",toPath petId]
    params = []

-- | /pet/{petId}/uploadImage
-- uploads an image
-- 
-- petId :: Integer  - Required - ID of pet to update
-- additionalMetadata :: Maybe Text  - Additional data to pass to server
-- file :: Maybe FilePath  - file to upload
uploadFile 
  :: Integer -- ^ 'petId': ID of pet to update
  -> ApiResponse
uploadFile petId = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/pet/",toPath petId,"/uploadImage"]
    params = []



-- | /store/order/{orderId}
-- Delete purchase order by ID
-- For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
-- orderId :: Integer  - Required - ID of the order that needs to be deleted
deleteOrder 
  :: Integer -- ^ 'orderId': ID of the order that needs to be deleted
  -> ()
deleteOrder orderId = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" url params
    url = T.concat ["/store/order/",toPath orderId]
    params = []

-- | /store/inventory
-- Returns pet inventories by status
-- Returns a map of status codes to quantities
getInventory 
  :: (Map.Map String Int)
getInventory = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/store/inventory"]
    params = []

-- | /store/order/{orderId}
-- Find purchase order by ID
-- For valid response try integer IDs with value &gt;&#x3D; 1 and &lt;&#x3D; 10. Other values will generated exceptions
-- orderId :: Integer  - Required - ID of pet that needs to be fetched
getOrderById 
  :: Integer -- ^ 'orderId': ID of pet that needs to be fetched
  -> Order
getOrderById orderId = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/store/order/",toPath orderId]
    params = []

-- | /store/order
-- Place an order for a pet
-- 
-- body :: Order  - Required - order placed for purchasing the pet
placeOrder 
  :: Order -- ^ 'body': order placed for purchasing the pet
  -> Order
placeOrder body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/store/order"]
    params = []



-- | /user
-- Create user
-- This can only be done by the logged in user.
-- body :: User  - Required - Created user object
createUser 
  :: User -- ^ 'body': Created user object
  -> ()
createUser body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/user"]
    params = []

-- | /user/createWithArray
-- Creates list of users with given input array
-- 
-- body :: [User]  - Required - List of user object
createUsersWithArrayInput 
  :: [User] -- ^ 'body': List of user object
  -> ()
createUsersWithArrayInput body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/user/createWithArray"]
    params = []

-- | /user/createWithList
-- Creates list of users with given input array
-- 
-- body :: [User]  - Required - List of user object
createUsersWithListInput 
  :: [User] -- ^ 'body': List of user object
  -> ()
createUsersWithListInput body = request
  where
    request = mkSwaggerPetstoreRequest "POST" url params
    url = T.concat ["/user/createWithList"]
    params = []

-- | /user/{username}
-- Delete user
-- This can only be done by the logged in user.
-- username :: Text  - Required - The name that needs to be deleted
deleteUser 
  :: Text -- ^ 'username': The name that needs to be deleted
  -> ()
deleteUser username = request
  where
    request = mkSwaggerPetstoreRequest "DELETE" url params
    url = T.concat ["/user/",toPath username]
    params = []

-- | /user/{username}
-- Get user by user name
-- 
-- username :: Text  - Required - The name that needs to be fetched. Use user1 for testing. 
getUserByName 
  :: Text -- ^ 'username': The name that needs to be fetched. Use user1 for testing. 
  -> User
getUserByName username = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/user/",toPath username]
    params = []

-- | /user/login
-- Logs user into the system
-- 
-- username :: Text  - Required - The user name for login
-- password :: Text  - Required - The password for login in clear text
loginUser 
  :: Text -- ^ 'username': The user name for login
  -> Text -- ^ 'password': The password for login in clear text
  -> Text
loginUser username password = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/user/login"]
    params = []

-- | /user/logout
-- Logs out current logged in user session
-- 
logoutUser 
  :: ()
logoutUser = request
  where
    request = mkSwaggerPetstoreRequest "GET" url params
    url = T.concat ["/user/logout"]
    params = []

-- | /user/{username}
-- Updated user
-- This can only be done by the logged in user.
-- username :: Text  - Required - name that need to be updated
-- body :: User  - Required - Updated user object
updateUser 
  :: Text -- ^ 'username': name that need to be updated
  -> User -- ^ 'body': Updated user object
  -> ()
updateUser username body = request
  where
    request = mkSwaggerPetstoreRequest "PUT" url params
    url = T.concat ["/user/",toPath username]
    params = []

