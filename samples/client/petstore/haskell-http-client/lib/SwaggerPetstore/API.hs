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

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method as NHTM
