{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropJSON
import Instances ()

import SwaggerPetstore.Model

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $
  do describe "JSON instances" $
       do propJSONEq (Proxy :: Proxy Category)
          propJSONEq (Proxy :: Proxy Pet)
          propJSONEq (Proxy :: Proxy Tag)
          
