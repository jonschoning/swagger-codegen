{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- |
-- Module      : SwaggerPetstore.Types
module SwaggerPetstore.Types where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types 
import Data.Data (Data, Typeable)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.Applicative
import Prelude 

import qualified Data.Map as Map
import qualified Data.Text as T



-- * ApiResponse
-- | 

data ApiResponse = ApiResponse
  { apiResponseCode :: Maybe Int
  , apiResponseType :: Maybe Text
  , apiResponseMessage :: Maybe Text
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON ApiResponse where
  parseJSON (Object o) =
    ApiResponse
      <$> o .:! "code" 
      <*> o .:! "type" 
      <*> o .:! "message" 
  parseJSON _ = fail "bad ApiResponse parse"

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
{-# INLINE mkApiResponse #-}




-- * Category
-- | 

data Category = Category
  { categoryId :: Maybe Integer
  , categoryName :: Maybe Text
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Category where
  parseJSON (Object o) =
    Category
      <$> o .:! "id" 
      <*> o .:! "name" 
  parseJSON _ = fail "bad Category parse"

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
{-# INLINE mkCategory #-}




-- * Order
-- | 

data Order = Order
  { orderId :: Maybe Integer
  , orderPetId :: Maybe Integer
  , orderQuantity :: Maybe Int
  , orderShipDate :: Maybe Integer
  , orderStatus :: Maybe Text -- ^ Order Status
  , orderComplete :: Maybe Bool
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Order where
  parseJSON (Object o) =
    Order
      <$> o .:! "id" 
      <*> o .:! "petId" 
      <*> o .:! "quantity" 
      <*> o .:! "shipDate" 
      <*> o .:! "status" 
      <*> o .:! "complete" 
  parseJSON _ = fail "bad Order parse"

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
{-# INLINE mkOrder #-}




-- * Pet
-- | 

data Pet = Pet
  { petId :: Maybe Integer
  , petCategory :: Maybe Category
  , petName :: Text
  , petPhotoUrls :: [Text]
  , petTags :: Maybe [Tag]
  , petStatus :: Maybe Text -- ^ pet status in the store
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Pet where
  parseJSON (Object o) =
    Pet
      <$> o .:! "id" 
      <*> o .:! "category" 
      <*> o .:  "name" 
      <*> o .:  "photoUrls" 
      <*> o .:! "tags" 
      <*> o .:! "status" 
  parseJSON _ = fail "bad Pet parse"

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
{-# INLINE mkPet #-}




-- * Tag
-- | 

data Tag = Tag
  { tagId :: Maybe Integer
  , tagName :: Maybe Text
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Tag where
  parseJSON (Object o) =
    Tag
      <$> o .:! "id" 
      <*> o .:! "name" 
  parseJSON _ = fail "bad Tag parse"

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
{-# INLINE mkTag #-}




-- * User
-- | 

data User = User
  { userId :: Maybe Integer
  , userUsername :: Maybe Text
  , userFirstName :: Maybe Text
  , userLastName :: Maybe Text
  , userEmail :: Maybe Text
  , userPassword :: Maybe Text
  , userPhone :: Maybe Text
  , userUserStatus :: Maybe Int -- ^ User Status
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON User where
  parseJSON (Object o) =
    User
      <$> o .:! "id" 
      <*> o .:! "username" 
      <*> o .:! "firstName" 
      <*> o .:! "lastName" 
      <*> o .:! "email" 
      <*> o .:! "password" 
      <*> o .:! "phone" 
      <*> o .:! "userStatus" 
  parseJSON _ = fail "bad User parse"

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
{-# INLINE mkUser #-}




-- * Utilities

-- | Removes Null fields.  OpenAPI-Specification 2.0 does not allow Null in JSON.
omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
