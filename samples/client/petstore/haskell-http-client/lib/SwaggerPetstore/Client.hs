{-|
Module : SwaggerPetstore.Client
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Client where

import SwaggerPetstore.Model
import SwaggerPetstore.API
import SwaggerPetstore.MimeTypes

import qualified Control.Monad.IO.Class as P
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Proxy as P (Proxy(..))
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Web.FormUrlEncoded as WH
import Web.HttpApiData as WH
import Control.Monad.Catch (MonadThrow)

import qualified Control.Monad.Logger as LG

import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types.Method as NH
import qualified Network.HTTP.Types as NH
import qualified Network.HTTP.Types.URI as NH

-- * Config

data SwaggerPetstoreConfig = SwaggerPetstoreConfig
  { host  :: BCL.ByteString
  , execLoggingT :: ExecLoggingT
  , filterLoggingT :: LG.LogSource -> LG.LogLevel -> Bool
  }

mkConfig :: SwaggerPetstoreConfig
mkConfig =
  SwaggerPetstoreConfig
  { host = "http://petstore.swagger.io/v2"
  , execLoggingT = runNullLoggingT
  , filterLoggingT = infoLevelFilter
  }

withStdoutLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withStdoutLogging p = p { execLoggingT = LG.runStdoutLoggingT}

withStderrLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withStderrLogging p = p { execLoggingT = LG.runStderrLoggingT}

withNoLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withNoLogging p = p { execLoggingT = runNullLoggingT}

-- * Dispatch

dispatch
  :: UseAccept req res accept
  => SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept
  -> IO (Either SwaggerPetstoreError res) -- ^ response
dispatch config request accept = do
  response <- dispatch' config request accept
  case mimeUnrender' accept (NH.responseBody response) of
    Left s -> return (Left (SwaggerPetstoreError s response))
    (Right r) -> return (Right r)

dispatch'
  :: Produces req accept
  => SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatch' config request accept = do
  manager <- NH.newManager NH.tlsManagerSettings
  InitRequest req <- toInitRequest config request accept
  NH.httpLbs req manager
  
-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req contentType accept res = InitRequest
  { unInitRequest :: NH.Request
  } deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
toInitRequest
  :: forall req accept contentType res.
     Produces req accept
  => SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> accept
  -> IO (InitRequest req contentType accept res) -- ^ initialized request
toInitRequest SwaggerPetstoreConfig {..} req0 accept = do
  parsedReq <- NH.parseRequest $ BCL.unpack $ BCL.append host (BCL.concat (urlPath req0))
  let req2 = _addAcceptHeader req0 accept
      reqHeaders = paramsHeaders (params req2)
      reqQuery = NH.renderQuery True (paramsQuery (params req2))
      pReq = parsedReq { NH.method = (rMethod req2)
                       , NH.requestHeaders = reqHeaders
                       , NH.queryString = reqQuery
                       }
  outReq <- case paramsBody (params req2) of
    ParamBodyNone -> pure (pReq { NH.requestBody = mempty })
    ParamBodyB bs -> pure (pReq { NH.requestBody = NH.RequestBodyBS bs })
    ParamBodyBL bl -> pure (pReq { NH.requestBody = NH.RequestBodyLBS bl })
    ParamBodyFormUrlEncoded form -> pure (pReq { NH.requestBody = NH.RequestBodyLBS (WH.urlEncodeForm form) })
    ParamBodyMultipartFormData parts -> NH.formDataBody parts pReq

  pure (InitRequest outReq)

-- | convenience method for modifying the underlying Request
modifyInitRequest :: InitRequest req contentType accept res -> (NH.Request -> NH.Request) -> InitRequest req contentType accept res
modifyInitRequest (InitRequest req) f = InitRequest (f req)

-- | convenience method for modifying the underlying Request (monadic)
modifyInitRequestM :: Monad m => InitRequest req contentType accept res -> (NH.Request -> m NH.Request) -> m (InitRequest req contentType accept res)
modifyInitRequestM (InitRequest req) f = fmap InitRequest (f req)

-- * Error

data SwaggerPetstoreError =
  SwaggerPetstoreError {
    parseError   :: String
  , reponseError :: NH.Response BCL.ByteString
  } deriving (Eq, Show)

-- * Logging

type ExecLoggingT = forall m. P.MonadIO m =>
                              forall a. LG.LoggingT m a -> m a

-- ** Null Logger

nullLogger :: LG.Loc -> LG.LogSource -> LG.LogLevel -> LG.LogStr -> IO ()
nullLogger _ _ _ _ = return ()

runNullLoggingT :: LG.LoggingT m a -> m a
runNullLoggingT = (`LG.runLoggingT` nullLogger)

-- ** Logging Filters

errorLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
errorLevelFilter = minLevelFilter LG.LevelError

infoLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
infoLevelFilter = minLevelFilter LG.LevelInfo

debugLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
debugLevelFilter = minLevelFilter LG.LevelDebug

minLevelFilter :: LG.LogLevel -> LG.LogSource -> LG.LogLevel -> Bool
minLevelFilter l _ l' = l' >= l
