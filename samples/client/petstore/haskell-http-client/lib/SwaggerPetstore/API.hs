{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import Control.Monad.Logger

import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Types.Method as NHTM

-- * Config

data SwaggerPetstoreConfig = SwaggerPetstoreConfig
  { host  :: Text
  , execLoggingT :: ExecLoggingT
  , filterLoggingT :: LogSource -> LogLevel -> Bool
  }

mkSwaggerPetstoreConfig :: SwaggerPetstoreConfig
mkSwaggerPetstoreConfig =
  SwaggerPetstoreConfig
  { host = "http://petstore.swagger.io/v2"
  , execLoggingT = runNullLoggingT
  , filterLoggingT = infoLevelFilter
  }

withStdoutLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withStdoutLogging p = p { execLoggingT = runStdoutLoggingT}

withStderrLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withStderrLogging p = p { execLoggingT = runStderrLoggingT}

withNoLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withNoLogging p = p { execLoggingT = runNullLoggingT}

-- * Dispatch

dispatch' :: SwaggerPetstoreConfig
          -> SwaggerPetstoreRequest
          -> IO (Response BSL.ByteString)
dispatch' SwaggerPetstoreConfig {..} SwaggerPetstoreRequest {..} = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest $ T.unpack $ T.append host endpoint
  let reqBody | rMethod == NHTM.methodGet = mempty
              | otherwise = filterBody params
      reqQuery  = paramsToByteString $ filterQuery params
      req = initReq { method = rMethod
                    , requestBody = RequestBodyLBS reqBody
                    , queryString = reqQuery
                    }
  httpLbs req manager

dispatchJson
  :: (FromJSON a)
  => SwaggerPetstoreConfig
  -> SwaggerPetstoreRequest
  -> IO (Either SwaggerPetstoreError a)
dispatchJson config request = do
  response <- dispatch' config request
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (SwaggerPetstoreError s response))
    (Right r) -> return (Right r)


-- * Error

data SwaggerPetstoreError =
  SwaggerPetstoreError {
    parseError   :: String
  , reponseError :: Response BSL.ByteString
  } deriving (Eq, Show)

-- * Logging

type ExecLoggingT = forall m. MonadIO m =>
                              forall a. LoggingT m a -> m a

-- ** Null Logger

nullLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
nullLogger _ _ _ _ = return ()

runNullLoggingT :: LoggingT m a -> m a
runNullLoggingT = (`runLoggingT` nullLogger)

-- ** Logging Filters

errorLevelFilter :: LogSource -> LogLevel -> Bool
errorLevelFilter = minLevelFilter LevelError

infoLevelFilter :: LogSource -> LogLevel -> Bool
infoLevelFilter = minLevelFilter LevelInfo

debugLevelFilter :: LogSource -> LogLevel -> Bool
debugLevelFilter = minLevelFilter LevelDebug

minLevelFilter :: LogLevel -> LogSource -> LogLevel -> Bool
minLevelFilter l _ l' = l' >= l


-- * Util    

-- | Conversion of a key value pair to a query parameterized string
paramsToByteString
    :: (Monoid m, IsString m)
    => [(m, m)]
    -> m
paramsToByteString []           = mempty
paramsToByteString ((x,y) : []) = x <> "=" <> y
paramsToByteString ((x,y) : xs) =
    mconcat [ x, "=", y, "&" ] <> paramsToByteString xs

-- | Find the body from the list of [Params TupleBS8 BSL.ByteString]
filterBody :: [Params] -> BSL.ByteString
filterBody [] = ""
filterBody xs = case [c | Body c <- xs] of
               [] -> ""
               [c] -> c
               _ -> error "Bad input"

-- | Find the query parameters from the list of
-- [Params TupleBS8 BSL.ByteString]
filterQuery :: [Params] -> [(BS8.ByteString, BS8.ByteString)]
filterQuery [] = []
filterQuery xs = [b | Query b <- xs]
