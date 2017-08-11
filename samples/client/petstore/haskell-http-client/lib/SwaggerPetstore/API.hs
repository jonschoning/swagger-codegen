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
