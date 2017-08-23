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

dispatchReq
  :: UseAccept req res accept
  => NH.Manager -- ^ http-client Connection manager
  -> accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (NH.Response BCL.ByteString, Either SwaggerPetstoreError res) -- ^ response
dispatchReq manager accept config request = do
  httpResponse <- dispatchReqLbs manager accept config request
  let parsedResult =
        case mimeUnrender' accept (NH.responseBody httpResponse) of
          Left s -> Left (SwaggerPetstoreError s httpResponse)
          Right r -> Right r
  return (httpResponse, parsedResult)

dispatchReqLbs
  :: Produces req accept
  => NH.Manager -- ^ http-client Connection manager
  -> accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchReqLbs manager accept config request = do
  initReq <- toInitRequest accept config request 
  dispatchInitLbs manager config initReq

-- | dispatch an InitRequest
dispatchInitLbs
  :: NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> InitRequest req contentType res accept -- ^ init request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchInitLbs manager _ (InitRequest req) = do
  NH.httpLbs req manager
  
-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req contentType res accept = InitRequest
  { unInitRequest :: NH.Request
  } deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
toInitRequest
  :: forall req accept contentType res.
     Produces req accept
  => accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (InitRequest req contentType res accept) -- ^ initialized request
toInitRequest accept config req0 = do
  parsedReq <- NH.parseRequest $ BCL.unpack $ BCL.append (host config) (BCL.concat (urlPath req0))
  let req1 = _addAcceptHeader req0 accept
      reqHeaders = paramsHeaders (params req1)
      reqQuery = NH.renderQuery True (paramsQuery (params req1))
      pReq = parsedReq { NH.method = (rMethod req1)
                       , NH.requestHeaders = reqHeaders
                       , NH.queryString = reqQuery
                       }
  outReq <- case paramsBody (params req1) of
    ParamBodyNone -> pure (pReq { NH.requestBody = mempty })
    ParamBodyB bs -> pure (pReq { NH.requestBody = NH.RequestBodyBS bs })
    ParamBodyBL bl -> pure (pReq { NH.requestBody = NH.RequestBodyLBS bl })
    ParamBodyFormUrlEncoded form -> pure (pReq { NH.requestBody = NH.RequestBodyLBS (WH.urlEncodeForm form) })
    ParamBodyMultipartFormData parts -> NH.formDataBody parts pReq

  pure (InitRequest outReq)

-- | convenience method for modifying the underlying Request
modifyInitRequest :: InitRequest req contentType res accept -> (NH.Request -> NH.Request) -> InitRequest req contentType res accept 
modifyInitRequest (InitRequest req) f = InitRequest (f req)

-- | convenience method for modifying the underlying Request (monadic)
modifyInitRequestM :: Monad m => InitRequest req contentType res accept -> (NH.Request -> m NH.Request) -> m (InitRequest req contentType res accept)
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
