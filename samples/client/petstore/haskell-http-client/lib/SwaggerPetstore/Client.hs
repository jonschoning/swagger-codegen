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

dispatch' :: SwaggerPetstoreConfig -- ^ config
          -> SwaggerPetstoreRequest r -- ^ request
          -> IO (Response BSL.ByteString) -- ^ response
dispatch' config request = do
  (InitRequest req) <- toInitRequest config request
  manager <- newManager tlsManagerSettings
  httpLbs req manager

dispatchJson
  :: (FromJSON r)
  => SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest r -- ^ request
  -> IO (Either SwaggerPetstoreError r) -- ^ response
dispatchJson config request = do
  response <- dispatch' config request
  let result = eitherDecode $ responseBody response
  case result of
    Left s -> return (Left (SwaggerPetstoreError s response))
    (Right r) -> return (Right r)

-- * toInitRequest

-- | wraps an http-client 'Request' with the return type parameter "r"
newtype InitRequest r = InitRequest { unInitRequest :: Request }

-- |  Build an http-client 'Request' record from the supplied config and request
toInitRequest
    :: MonadThrow m
    => SwaggerPetstoreConfig -- ^ config
    -> SwaggerPetstoreRequest r -- ^ request
    -> m (InitRequest r) -- ^ initialized request
toInitRequest SwaggerPetstoreConfig {..} SwaggerPetstoreRequest {..} = do
  parsedReq <- parseRequest $ T.unpack $ T.append host (T.concat endpoint)
  let reqBody | rMethod == NHTM.methodGet = mempty
              | otherwise = filterBody params
      reqQuery  = paramsToByteString $ filterQuery params
      initReq = parsedReq { method = rMethod
                          , requestBody = RequestBodyLBS reqBody
                          , queryString = reqQuery
                          }
  return (InitRequest initReq)

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
filterBody :: [EncodedParam] -> BSL.ByteString
filterBody [] = ""
filterBody xs = case [c | Body c <- xs] of
               [] -> ""
               [c] -> c
               _ -> error "Bad input"

-- | Find the query parameters from the list of
-- [Params TupleBS8 BSL.ByteString]
filterQuery :: [EncodedParam] -> [TupleBS8]
filterQuery [] = []
filterQuery xs = [b | Query b <- xs]
