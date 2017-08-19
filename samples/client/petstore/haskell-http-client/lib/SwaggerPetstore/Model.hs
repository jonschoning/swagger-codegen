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
import Control.Applicative

import qualified Data.Map as Map

import Prelude 



-- * Models


-- ** Category

data Category = Category
  { categoryId :: Maybe Integer -- ^ "id"
  , categoryName :: Maybe Text -- ^ "name"
  } deriving (Show,Eq,Typeable)

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



-- ** Pet

data Pet = Pet
  { petId :: Maybe Integer -- ^ "id"
  , petCategory :: Maybe Category -- ^ "category"
  , petName :: Text -- ^ /Required/ "name"
  , petPhotoUrls :: [Text] -- ^ /Required/ "photoUrls"
  , petTags :: Maybe [Tag] -- ^ "tags"
  , petStatus :: Maybe Text -- ^ "status" - pet status in the store
  } deriving (Show,Eq,Typeable)

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
  } deriving (Show,Eq,Typeable)

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



-- * Utils

-- | Removes Null fields.  OpenAPI-Specification 2.0 does not allow Null in JSON.
omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
