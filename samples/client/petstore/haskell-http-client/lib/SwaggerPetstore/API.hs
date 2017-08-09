{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson 
import Data.Aeson.Types 
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Web.FormUrlEncoded as WF
import Web.HttpApiData as WH

import qualified Data.Map as Map
import qualified Data.Text as T


-- | FormData for operation updatePetWithForm
data FormUpdatePetWithForm = FormUpdatePetWithForm
  { updatePetWithFormName :: Maybe Text -- ^ Updated name of the pet
  , updatePetWithFormStatus :: Maybe Text -- ^ Updated status of the pet
  } deriving (Show, Eq, Generic)

instance FromForm FormUpdatePetWithForm where
  fromForm inputs = FormUpdatePetWithForm <$> WF.parseMaybe "name" inputs <*> WF.parseMaybe "status" inputs

instance ToForm FormUpdatePetWithForm where
  toForm value =
    [ ("name", toQueryParam $ updatePetWithFormName value)
    , ("status", toQueryParam $ updatePetWithFormStatus value)
    ]
-- | FormData for operation uploadFile
data FormUploadFile = FormUploadFile
  { uploadFileAdditionalMetadata :: Maybe Text -- ^ Additional data to pass to server
  , uploadFileFile :: Maybe FilePath -- ^ file to upload
  } deriving (Show, Eq, Generic)

instance FromForm FormUploadFile where
  fromForm inputs = FormUploadFile <$> WF.parseMaybe "additionalMetadata" inputs <*> WF.parseMaybe "file" inputs

instance ToForm FormUploadFile where
  toForm value =
    [ ("additionalMetadata", toQueryParam $ uploadFileAdditionalMetadata value)
    , ("file", toQueryParam $ uploadFileFile value)
    ]

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where parseQueryParam = parseSeparatedQueryList ','
instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where parseQueryParam = parseSeparatedQueryList '\t'
instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where parseQueryParam = parseSeparatedQueryList ' '
instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where parseQueryParam = parseSeparatedQueryList '|'
instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where toQueryParam = formatSeparatedQueryList ','
instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where toQueryParam = formatSeparatedQueryList '\t'
instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where toQueryParam = formatSeparatedQueryList ' '
instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where toQueryParam = formatSeparatedQueryList '|'
instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList
