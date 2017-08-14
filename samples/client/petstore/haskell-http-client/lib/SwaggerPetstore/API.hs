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

import Prelude as P

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
  mkRequest "POST" ["/pet"]
    `setBodyLBS` A.encode body

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
deletePet petId =
  mkRequest "DELETE" ["/pet/",toPath petId]
    

data DeletePet
instance HasOptionalParam DeletePet Api'Underscorekey where
  applyOptionalParam req (Api'Underscorekey xs) =
    addHeader req ("api_key", toBS8 xs)


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
  mkRequest "GET" ["/pet/findByStatus"]
    `addQuery` ("status", Just (toBS8 status))

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
findPetsByTags tags =
  mkRequest "GET" ["/pet/findByTags"]
    `addQuery` ("tags", Just (toBS8 tags))

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
getPetById petId =
  mkRequest "GET" ["/pet/",toPath petId]
    

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
updatePet body =
  mkRequest "PUT" ["/pet"]
    `setBodyLBS` A.encode body

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
updatePetWithForm petId =
  mkRequest "POST" ["/pet/",toPath petId]
    

data UpdatePetWithForm

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name where
  applyOptionalParam req (Name xs) =
    addFormUrlField req ("name", Just (toBS8 xs))

-- | /Optional Param/ "status" - Updated status of the pet
instance HasOptionalParam UpdatePetWithForm Status where
  applyOptionalParam req (Status xs) =
    addFormUrlField req ("status", Just (toBS8 xs))


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
  mkRequest "POST" ["/pet/",toPath petId,"/uploadImage"]
    

data UploadFile

-- | /Optional Param/ "additionalMetadata" - Additional data to pass to server
instance HasOptionalParam UploadFile AdditionalMetadata where
  applyOptionalParam req (AdditionalMetadata xs) =
    addFormUrlField req ("additionalMetadata", Just (toBS8 xs))

-- | /Optional Param/ "file" - file to upload
instance HasOptionalParam UploadFile File where
  applyOptionalParam req (File xs) =
    addFormUrlField req ("file", Just (toBS8 xs))


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
  mkRequest "DELETE" ["/store/order/",toPath orderId]
    

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
getInventory =
  mkRequest "GET" ["/store/inventory"]

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
getOrderById orderId =
  mkRequest
    "GET"
    ["/store/order/",toPath orderId]
    

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
placeOrder body =
  mkRequest "POST" ["/store/order"]
    `setBodyLBS` A.encode body

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
createUser body =
  mkRequest "POST" ["/user"]
    `setBodyLBS` A.encode body

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
createUsersWithArrayInput body =
  mkRequest "POST" ["/user/createWithArray"]
    `setBodyLBS` A.encode body

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
createUsersWithListInput body =
  mkRequest "POST" ["/user/createWithList"]
    `setBodyLBS` A.encode body

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
deleteUser username =
  mkRequest "DELETE" ["/user/",toPath username]
    

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
getUserByName username =
  mkRequest "GET" ["/user/",toPath username]
    

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
loginUser username password =
  mkRequest "GET" ["/user/login"]
    `addQuery` ("username", Just (toBS8 username))
    `addQuery` ("password", Just (toBS8 password))

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
logoutUser =
  mkRequest "GET" ["/user/logout"]

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
updateUser username body =
  mkRequest "PUT" ["/user/",toPath username]
    
    `setBodyLBS` A.encode body

data UpdateUser


-- * SwaggerPetstoreRequest

-- | Represents a request. The "req" type variable is the request type. The "res" type variable is the response type.
data SwaggerPetstoreRequest req res = SwaggerPetstoreRequest
  { rMethod  :: NH.Method   -- ^ Method of SwaggerPetstoreRequest
  , urlPath :: [BS8.ByteString] -- ^ Endpoint of SwaggerPetstoreRequest
  , params   :: Params -- ^ params of SwaggerPetstoreRequest
  }
  deriving (Show)


-- * SwaggerPetstoreRequest Helpers

mkRequest :: NH.Method -- ^ Method 
          -> [BS8.ByteString] -- ^ Endpoint
          -> SwaggerPetstoreRequest req res -- ^ req: Request Type, res: Response Type
mkRequest m u = SwaggerPetstoreRequest m u mkParams

mkParams :: Params
mkParams = Params [] [] ParamBodyNone

addHeader :: SwaggerPetstoreRequest req res -> NH.Header -> SwaggerPetstoreRequest req res
addHeader req header = 
    let _params = params req
    in req { params = _params { paramsHeaders = header : paramsHeaders _params } }

addQuery :: SwaggerPetstoreRequest req res -> NH.QueryItem -> SwaggerPetstoreRequest req res
addQuery req query = 
    let _params = params req
    in req { params = _params { paramsQuery = query : paramsQuery _params } }

setBodyBS :: SwaggerPetstoreRequest req res -> B.ByteString -> SwaggerPetstoreRequest req res
setBodyBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyBS body } }

setBodyLBS :: SwaggerPetstoreRequest req res -> BSL.ByteString -> SwaggerPetstoreRequest req res
setBodyLBS req body = 
    let _params = params req
    in req { params = _params { paramsBody = ParamBodyBSL body } }

addFormUrlField :: SwaggerPetstoreRequest req res -> NH.QueryItem -> SwaggerPetstoreRequest req res
addFormUrlField req field = 
    let _params = params req
        fields = case paramsBody _params of
            ParamBodyFormUrl _fields -> _fields
            _ -> []
    in req { params = _params { paramsBody = ParamBodyFormUrl (field : fields) } }

addMultiFormPart :: SwaggerPetstoreRequest req res -> NH.Part -> SwaggerPetstoreRequest req res
addMultiFormPart req newpart = 
    let _params = params req
        parts = case paramsBody _params of
            ParamBodyMultiForm _parts -> _parts
            _ -> []
    in req { params = _params { paramsBody = ParamBodyMultiForm (newpart : parts) } }


-- | Type alias for query parameters
type TupleBS8 = (BS8.ByteString, BS8.ByteString)

-- * Params

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
  deriving (Show)

data ParamBody
  = ParamBodyNone
  | ParamBodyBS B.ByteString
  | ParamBodyBSL BSL.ByteString
  | ParamBodyFormUrl NH.Query
  | ParamBodyMultiForm [NH.Part]
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
