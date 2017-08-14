{-|
Module : SwaggerPetstore.Client
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Client where

import SwaggerPetstore.Model
import SwaggerPetstore.API

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
import Control.Monad.Catch (MonadThrow)

import Control.Monad.Logger

import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types.Method as NH
import qualified Network.HTTP.Types as NH
import qualified Network.HTTP.Types.URI as NH

-- * Config

data SwaggerPetstoreConfig = SwaggerPetstoreConfig
  { host  :: BS8.ByteString
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

dispatch' :: SwaggerPetstoreConfig -- ^ config
          -> SwaggerPetstoreRequest req res -- ^ request
          -> IO (NH.Response BSL.ByteString) -- ^ response
dispatch' config request = do
  (InitRequest req) <- toInitRequest config request
  manager <- NH.newManager NH.tlsManagerSettings
  NH.httpLbs req manager

dispatchJson
  :: (FromJSON res)
  => SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req res -- ^ request
  -> IO (Either SwaggerPetstoreError res) -- ^ response
dispatchJson config request = do
  response <- dispatch' config request
  let result = eitherDecode $ NH.responseBody response
  case result of
    Left s -> return (Left (SwaggerPetstoreError s response))
    (Right r) -> return (Right r)

-- * InitRequest

-- | wraps an http-client 'Request' with the return type parameter "r"
newtype InitRequest r = InitRequest { unInitRequest :: NH.Request }

-- |  Build an http-client 'Request' record from the supplied config and request
toInitRequest
    :: SwaggerPetstoreConfig -- ^ config
    -> SwaggerPetstoreRequest req res -- ^ request
    -> IO (InitRequest r) -- ^ initialized request
toInitRequest SwaggerPetstoreConfig {..} SwaggerPetstoreRequest {..} = do
  parsedReq <- NH.parseRequest $ BS8.unpack $ BS8.append host (BS8.concat urlPath)
  let reqHeaders = encParamsHeaders params
      reqQuery = NH.renderQuery True (encParamsQuery params)
      req = parsedReq { NH.method = rMethod
                        , NH.requestHeaders = reqHeaders
                        , NH.queryString = reqQuery
                      }
  initReq <- case encParamsBody params of
    EncBodyNone -> pure (req { NH.requestBody = mempty })
    EncBodyBS bs -> pure (req { NH.requestBody = NH.RequestBodyBS bs })
    EncBodyBSL bsl -> pure (req { NH.requestBody = NH.RequestBodyLBS bsl })
    EncBodyFormUrlEnc query -> pure (req { NH.requestBody = NH.RequestBodyBS $ NH.renderQuery True query })
    EncBodyMultiForm parts -> NH.formDataBody parts req
  pure (InitRequest initReq)
-- * Error

data SwaggerPetstoreError =
  SwaggerPetstoreError {
    parseError   :: String
  , reponseError :: NH.Response BSL.ByteString
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
