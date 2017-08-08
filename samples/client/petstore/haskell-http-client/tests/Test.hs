{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import PropJSON
import Instances ()

import SwaggerPetstore.Types

main :: IO ()
main =
  hspec $
  do describe "JSON instances" $
       do propJSONEq (Proxy :: Proxy ApiResponse)
          propJSONEq (Proxy :: Proxy Category)
          propJSONEq (Proxy :: Proxy Order)
          propJSONEq (Proxy :: Proxy Pet)
          propJSONEq (Proxy :: Proxy Tag)
          propJSONEq (Proxy :: Proxy User)
          
