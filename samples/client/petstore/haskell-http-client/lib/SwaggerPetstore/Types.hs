{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- |
-- Module      : SwaggerPetstore.Types
module SwaggerPetstore.Types where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types 
import Data.Data (Data, Typeable)
import Data.Text (Text)
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
      <$> o .: "code" 
      <*> o .: "type" 
      <*> o .: "message" 
  parseJSON _ = fail "bad ApiResponse parse"

instance ToJSON ApiResponse where
  toJSON ApiResponse {..} =
    object
      [ "code" .= toJSON apiResponseCode
      , "type" .= toJSON apiResponseType
      , "message" .= toJSON apiResponseMessage
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

-- | 'apiResponseCode' Traversal for 'ApiResponse'
apiResponseCodeT :: Traversal_' ApiResponse Int
apiResponseCodeT f s = maybe (pure s) (\a -> (\b -> s { apiResponseCode = Just b} ) <$> f a) (apiResponseCode s)
{-# INLINE apiResponseCodeT #-}

-- | 'apiResponseType' Traversal for 'ApiResponse'
apiResponseTypeT :: Traversal_' ApiResponse Text
apiResponseTypeT f s = maybe (pure s) (\a -> (\b -> s { apiResponseType = Just b} ) <$> f a) (apiResponseType s)
{-# INLINE apiResponseTypeT #-}

-- | 'apiResponseMessage' Traversal for 'ApiResponse'
apiResponseMessageT :: Traversal_' ApiResponse Text
apiResponseMessageT f s = maybe (pure s) (\a -> (\b -> s { apiResponseMessage = Just b} ) <$> f a) (apiResponseMessage s)
{-# INLINE apiResponseMessageT #-}




-- * Category
-- | 

data Category = Category
  { categoryId :: Maybe Integer
  , categoryName :: Maybe Text
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Category where
  parseJSON (Object o) =
    Category
      <$> o .: "id" 
      <*> o .: "name" 
  parseJSON _ = fail "bad Category parse"

instance ToJSON Category where
  toJSON Category {..} =
    object
      [ "id" .= toJSON categoryId
      , "name" .= toJSON categoryName
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

-- | 'categoryId' Traversal for 'Category'
categoryIdT :: Traversal_' Category Integer
categoryIdT f s = maybe (pure s) (\a -> (\b -> s { categoryId = Just b} ) <$> f a) (categoryId s)
{-# INLINE categoryIdT #-}

-- | 'categoryName' Traversal for 'Category'
categoryNameT :: Traversal_' Category Text
categoryNameT f s = maybe (pure s) (\a -> (\b -> s { categoryName = Just b} ) <$> f a) (categoryName s)
{-# INLINE categoryNameT #-}




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
      <$> o .: "id" 
      <*> o .: "petId" 
      <*> o .: "quantity" 
      <*> o .: "shipDate" 
      <*> o .: "status" 
      <*> o .: "complete" 
  parseJSON _ = fail "bad Order parse"

instance ToJSON Order where
  toJSON Order {..} =
    object
      [ "id" .= toJSON orderId
      , "petId" .= toJSON orderPetId
      , "quantity" .= toJSON orderQuantity
      , "shipDate" .= toJSON orderShipDate
      , "status" .= toJSON orderStatus
      , "complete" .= toJSON orderComplete
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

-- | 'orderId' Traversal for 'Order'
orderIdT :: Traversal_' Order Integer
orderIdT f s = maybe (pure s) (\a -> (\b -> s { orderId = Just b} ) <$> f a) (orderId s)
{-# INLINE orderIdT #-}

-- | 'orderPetId' Traversal for 'Order'
orderPetIdT :: Traversal_' Order Integer
orderPetIdT f s = maybe (pure s) (\a -> (\b -> s { orderPetId = Just b} ) <$> f a) (orderPetId s)
{-# INLINE orderPetIdT #-}

-- | 'orderQuantity' Traversal for 'Order'
orderQuantityT :: Traversal_' Order Int
orderQuantityT f s = maybe (pure s) (\a -> (\b -> s { orderQuantity = Just b} ) <$> f a) (orderQuantity s)
{-# INLINE orderQuantityT #-}

-- | 'orderShipDate' Traversal for 'Order'
orderShipDateT :: Traversal_' Order Integer
orderShipDateT f s = maybe (pure s) (\a -> (\b -> s { orderShipDate = Just b} ) <$> f a) (orderShipDate s)
{-# INLINE orderShipDateT #-}

-- | 'orderStatus' Traversal for 'Order'
orderStatusT :: Traversal_' Order Text
orderStatusT f s = maybe (pure s) (\a -> (\b -> s { orderStatus = Just b} ) <$> f a) (orderStatus s)
{-# INLINE orderStatusT #-}

-- | 'orderComplete' Traversal for 'Order'
orderCompleteT :: Traversal_' Order Bool
orderCompleteT f s = maybe (pure s) (\a -> (\b -> s { orderComplete = Just b} ) <$> f a) (orderComplete s)
{-# INLINE orderCompleteT #-}




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
      <$> o .: "id" 
      <*> o .: "category" 
      <*> o .: "name" 
      <*> o .: "photoUrls" 
      <*> o .: "tags" 
      <*> o .: "status" 
  parseJSON _ = fail "bad Pet parse"

instance ToJSON Pet where
  toJSON Pet {..} =
    object
      [ "id" .= toJSON petId
      , "category" .= toJSON petCategory
      , "name" .= toJSON petName
      , "photoUrls" .= toJSON petPhotoUrls
      , "tags" .= toJSON petTags
      , "status" .= toJSON petStatus
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

-- | 'petId' Traversal for 'Pet'
petIdT :: Traversal_' Pet Integer
petIdT f s = maybe (pure s) (\a -> (\b -> s { petId = Just b} ) <$> f a) (petId s)
{-# INLINE petIdT #-}

-- | 'petCategory' Traversal for 'Pet'
petCategoryT :: Traversal_' Pet Category
petCategoryT f s = maybe (pure s) (\a -> (\b -> s { petCategory = Just b} ) <$> f a) (petCategory s)
{-# INLINE petCategoryT #-}

-- | 'petName' Lens for 'Pet'
petNameL :: Lens_' Pet Text
petNameL f Pet{..} = (\petName -> Pet { petName, ..} ) <$> f petName
{-# INLINE petNameL #-}

-- | 'petPhotoUrls' Lens for 'Pet'
petPhotoUrlsL :: Lens_' Pet [Text]
petPhotoUrlsL f Pet{..} = (\petPhotoUrls -> Pet { petPhotoUrls, ..} ) <$> f petPhotoUrls
{-# INLINE petPhotoUrlsL #-}

-- | 'petTags' Traversal for 'Pet'
petTagsT :: Traversal_' Pet [Tag]
petTagsT f s = maybe (pure s) (\a -> (\b -> s { petTags = Just b} ) <$> f a) (petTags s)
{-# INLINE petTagsT #-}

-- | 'petStatus' Traversal for 'Pet'
petStatusT :: Traversal_' Pet Text
petStatusT f s = maybe (pure s) (\a -> (\b -> s { petStatus = Just b} ) <$> f a) (petStatus s)
{-# INLINE petStatusT #-}




-- * Tag
-- | 

data Tag = Tag
  { tagId :: Maybe Integer
  , tagName :: Maybe Text
  } deriving (Show, Eq, Data, Typeable, Generic, Ord)

instance FromJSON Tag where
  parseJSON (Object o) =
    Tag
      <$> o .: "id" 
      <*> o .: "name" 
  parseJSON _ = fail "bad Tag parse"

instance ToJSON Tag where
  toJSON Tag {..} =
    object
      [ "id" .= toJSON tagId
      , "name" .= toJSON tagName
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

-- | 'tagId' Traversal for 'Tag'
tagIdT :: Traversal_' Tag Integer
tagIdT f s = maybe (pure s) (\a -> (\b -> s { tagId = Just b} ) <$> f a) (tagId s)
{-# INLINE tagIdT #-}

-- | 'tagName' Traversal for 'Tag'
tagNameT :: Traversal_' Tag Text
tagNameT f s = maybe (pure s) (\a -> (\b -> s { tagName = Just b} ) <$> f a) (tagName s)
{-# INLINE tagNameT #-}




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
      <$> o .: "id" 
      <*> o .: "username" 
      <*> o .: "firstName" 
      <*> o .: "lastName" 
      <*> o .: "email" 
      <*> o .: "password" 
      <*> o .: "phone" 
      <*> o .: "userStatus" 
  parseJSON _ = fail "bad User parse"

instance ToJSON User where
  toJSON User {..} =
    object
      [ "id" .= toJSON userId
      , "username" .= toJSON userUsername
      , "firstName" .= toJSON userFirstName
      , "lastName" .= toJSON userLastName
      , "email" .= toJSON userEmail
      , "password" .= toJSON userPassword
      , "phone" .= toJSON userPhone
      , "userStatus" .= toJSON userUserStatus
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

-- | 'userId' Traversal for 'User'
userIdT :: Traversal_' User Integer
userIdT f s = maybe (pure s) (\a -> (\b -> s { userId = Just b} ) <$> f a) (userId s)
{-# INLINE userIdT #-}

-- | 'userUsername' Traversal for 'User'
userUsernameT :: Traversal_' User Text
userUsernameT f s = maybe (pure s) (\a -> (\b -> s { userUsername = Just b} ) <$> f a) (userUsername s)
{-# INLINE userUsernameT #-}

-- | 'userFirstName' Traversal for 'User'
userFirstNameT :: Traversal_' User Text
userFirstNameT f s = maybe (pure s) (\a -> (\b -> s { userFirstName = Just b} ) <$> f a) (userFirstName s)
{-# INLINE userFirstNameT #-}

-- | 'userLastName' Traversal for 'User'
userLastNameT :: Traversal_' User Text
userLastNameT f s = maybe (pure s) (\a -> (\b -> s { userLastName = Just b} ) <$> f a) (userLastName s)
{-# INLINE userLastNameT #-}

-- | 'userEmail' Traversal for 'User'
userEmailT :: Traversal_' User Text
userEmailT f s = maybe (pure s) (\a -> (\b -> s { userEmail = Just b} ) <$> f a) (userEmail s)
{-# INLINE userEmailT #-}

-- | 'userPassword' Traversal for 'User'
userPasswordT :: Traversal_' User Text
userPasswordT f s = maybe (pure s) (\a -> (\b -> s { userPassword = Just b} ) <$> f a) (userPassword s)
{-# INLINE userPasswordT #-}

-- | 'userPhone' Traversal for 'User'
userPhoneT :: Traversal_' User Text
userPhoneT f s = maybe (pure s) (\a -> (\b -> s { userPhone = Just b} ) <$> f a) (userPhone s)
{-# INLINE userPhoneT #-}

-- | 'userUserStatus' Traversal for 'User'
userUserStatusT :: Traversal_' User Int
userUserStatusT f s = maybe (pure s) (\a -> (\b -> s { userUserStatus = Just b} ) <$> f a) (userUserStatus s)
{-# INLINE userUserStatusT #-}




-- * Lens Helpers

type Traversal_' s a = Traversal_ s s a a
type Traversal_ s t a b = forall (f :: * -> *). Applicative f => (a -> f b) -> s -> f t
type Lens_' s a = Lens_ s s a a
type Lens_ s t a b = forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t
