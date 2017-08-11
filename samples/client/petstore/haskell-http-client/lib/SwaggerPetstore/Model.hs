{-|
Module : SwaggerPetstore.Model
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Model where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types 
import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Applicative

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM

import Prelude 


-- * Models


-- ** ApiResponse

data ApiResponse = ApiResponse
  { apiResponseCode :: Maybe Int -- ^ "code"
  , apiResponseType :: Maybe Text -- ^ "type"
  , apiResponseMessage :: Maybe Text -- ^ "message"
  } deriving (Show,Eq)

instance FromJSON ApiResponse where
  parseJSON = withObject "ApiResponse" $ \o ->
    ApiResponse
      <$> o .:? "code" 
      <*> o .:? "type" 
      <*> o .:? "message" 

instance ToJSON ApiResponse where
  toJSON ApiResponse {..} =
    omitNulls
      [ "code" .= apiResponseCode
      , "type" .= apiResponseType
      , "message" .= apiResponseMessage
      ]

-- | Construct a value of type 'ApiResponse' (by applying it's required fields, if any)
mkApiResponse
  :: ApiResponse
mkApiResponse =
  ApiResponse
  { apiResponseCode = Nothing
  , apiResponseType = Nothing
  , apiResponseMessage = Nothing
  }



-- ** Category

data Category = Category
  { categoryId :: Maybe Integer -- ^ "id"
  , categoryName :: Maybe Text -- ^ "name"
  } deriving (Show,Eq)

instance FromJSON Category where
  parseJSON = withObject "Category" $ \o ->
    Category
      <$> o .:? "id" 
      <*> o .:? "name" 

instance ToJSON Category where
  toJSON Category {..} =
    omitNulls
      [ "id" .= categoryId
      , "name" .= categoryName
      ]

-- | Construct a value of type 'Category' (by applying it's required fields, if any)
mkCategory
  :: Category
mkCategory =
  Category
  { categoryId = Nothing
  , categoryName = Nothing
  }



-- ** Order

data Order = Order
  { orderId :: Maybe Integer -- ^ "id"
  , orderPetId :: Maybe Integer -- ^ "petId"
  , orderQuantity :: Maybe Int -- ^ "quantity"
  , orderShipDate :: Maybe Integer -- ^ "shipDate"
  , orderStatus :: Maybe Text -- ^ "status" - Order Status
  , orderComplete :: Maybe Bool -- ^ "complete"
  } deriving (Show,Eq)

instance FromJSON Order where
  parseJSON = withObject "Order" $ \o ->
    Order
      <$> o .:? "id" 
      <*> o .:? "petId" 
      <*> o .:? "quantity" 
      <*> o .:? "shipDate" 
      <*> o .:? "status" 
      <*> o .:? "complete" 

instance ToJSON Order where
  toJSON Order {..} =
    omitNulls
      [ "id" .= orderId
      , "petId" .= orderPetId
      , "quantity" .= orderQuantity
      , "shipDate" .= orderShipDate
      , "status" .= orderStatus
      , "complete" .= orderComplete
      ]

-- | Construct a value of type 'Order' (by applying it's required fields, if any)
mkOrder
  :: Order
mkOrder =
  Order
  { orderId = Nothing
  , orderPetId = Nothing
  , orderQuantity = Nothing
  , orderShipDate = Nothing
  , orderStatus = Nothing
  , orderComplete = Nothing
  }



-- ** Pet

data Pet = Pet
  { petId :: Maybe Integer -- ^ "id"
  , petCategory :: Maybe Category -- ^ "category"
  , petName :: Text -- ^ "name" - __(Required)__
  , petPhotoUrls :: [Text] -- ^ "photoUrls" - __(Required)__
  , petTags :: Maybe [Tag] -- ^ "tags"
  , petStatus :: Maybe Text -- ^ "status" - pet status in the store
  } deriving (Show,Eq)

instance FromJSON Pet where
  parseJSON = withObject "Pet" $ \o ->
    Pet
      <$> o .:? "id" 
      <*> o .:? "category" 
      <*> o .:  "name" 
      <*> o .:  "photoUrls" 
      <*> o .:? "tags" 
      <*> o .:? "status" 

instance ToJSON Pet where
  toJSON Pet {..} =
    omitNulls
      [ "id" .= petId
      , "category" .= petCategory
      , "name" .= petName
      , "photoUrls" .= petPhotoUrls
      , "tags" .= petTags
      , "status" .= petStatus
      ]

-- | Construct a value of type 'Pet' (by applying it's required fields, if any)
mkPet
  :: Text -- ^ 'petName' 
  -> [Text] -- ^ 'petPhotoUrls' 
  -> Pet
mkPet petName petPhotoUrls =
  Pet
  { petId = Nothing
  , petCategory = Nothing
  , petName
  , petPhotoUrls
  , petTags = Nothing
  , petStatus = Nothing
  }



-- ** Tag

data Tag = Tag
  { tagId :: Maybe Integer -- ^ "id"
  , tagName :: Maybe Text -- ^ "name"
  } deriving (Show,Eq)

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \o ->
    Tag
      <$> o .:? "id" 
      <*> o .:? "name" 

instance ToJSON Tag where
  toJSON Tag {..} =
    omitNulls
      [ "id" .= tagId
      , "name" .= tagName
      ]

-- | Construct a value of type 'Tag' (by applying it's required fields, if any)
mkTag
  :: Tag
mkTag =
  Tag
  { tagId = Nothing
  , tagName = Nothing
  }



-- ** User

data User = User
  { userId :: Maybe Integer -- ^ "id"
  , userUsername :: Maybe Text -- ^ "username"
  , userFirstName :: Maybe Text -- ^ "firstName"
  , userLastName :: Maybe Text -- ^ "lastName"
  , userEmail :: Maybe Text -- ^ "email"
  , userPassword :: Maybe Text -- ^ "password"
  , userPhone :: Maybe Text -- ^ "phone"
  , userUserStatus :: Maybe Int -- ^ "userStatus" - User Status
  } deriving (Show,Eq)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User
      <$> o .:? "id" 
      <*> o .:? "username" 
      <*> o .:? "firstName" 
      <*> o .:? "lastName" 
      <*> o .:? "email" 
      <*> o .:? "password" 
      <*> o .:? "phone" 
      <*> o .:? "userStatus" 

instance ToJSON User where
  toJSON User {..} =
    omitNulls
      [ "id" .= userId
      , "username" .= userUsername
      , "firstName" .= userFirstName
      , "lastName" .= userLastName
      , "email" .= userEmail
      , "password" .= userPassword
      , "phone" .= userPhone
      , "userStatus" .= userUserStatus
      ]

-- | Construct a value of type 'User' (by applying it's required fields, if any)
mkUser
  :: User
mkUser =
  User
  { userId = Nothing
  , userUsername = Nothing
  , userFirstName = Nothing
  , userLastName = Nothing
  , userEmail = Nothing
  , userPassword = Nothing
  , userPhone = Nothing
  , userUserStatus = Nothing
  }



-- * Utils

-- | Removes Null fields.  OpenAPI-Specification 2.0 does not allow Null in JSON.
omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
