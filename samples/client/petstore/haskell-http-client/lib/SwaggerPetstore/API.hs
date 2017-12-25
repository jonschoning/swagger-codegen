{-
   Swagger Petstore

   This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\

   OpenAPI spec version: 2.0
   Swagger Petstore API version: 1.0.0
   Contact: apiteam@swagger.io
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : SwaggerPetstore.API
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Core
import SwaggerPetstore.MimeTypes
import SwaggerPetstore.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified GHC.Base as P (Alternative)
import qualified Lens.Micro as L
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Monoid ((<>))
import Data.Function ((&))
import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** AnotherFake

-- *** testSpecialTags

-- | @PATCH \/another-fake\/dummy@
-- 
-- To test special tags
-- 
-- To test special tags
-- 
testSpecialTags 
  :: (Consumes TestSpecialTags contentType, MimeRender contentType Client)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Client -- ^ "body" -  client model
  -> SwaggerPetstoreRequest TestSpecialTags contentType Client accept
testSpecialTags _  _ body =
  _mkRequest "PATCH" ["/another-fake/dummy"]
    `setBodyParam` body

data TestSpecialTags 

-- | /Body Param/ "body" - client model
instance HasBodyParam TestSpecialTags Client 

-- | @application/json@
instance Consumes TestSpecialTags MimeJSON

-- | @application/json@
instance Produces TestSpecialTags MimeJSON


-- ** Fake

-- *** fakeOuterBooleanSerialize

-- | @POST \/fake\/outer\/boolean@
-- 
-- Test serialization of outer boolean types
-- 
fakeOuterBooleanSerialize 
  :: (Consumes FakeOuterBooleanSerialize contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest FakeOuterBooleanSerialize contentType OuterBoolean accept
fakeOuterBooleanSerialize _  _ =
  _mkRequest "POST" ["/fake/outer/boolean"]

data FakeOuterBooleanSerialize 

-- | /Body Param/ "body" - Input boolean as post body
instance HasBodyParam FakeOuterBooleanSerialize OuterBoolean 

-- *** fakeOuterCompositeSerialize

-- | @POST \/fake\/outer\/composite@
-- 
-- Test serialization of object with outer number type
-- 
fakeOuterCompositeSerialize 
  :: (Consumes FakeOuterCompositeSerialize contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest FakeOuterCompositeSerialize contentType OuterComposite accept
fakeOuterCompositeSerialize _  _ =
  _mkRequest "POST" ["/fake/outer/composite"]

data FakeOuterCompositeSerialize 

-- | /Body Param/ "body" - Input composite as post body
instance HasBodyParam FakeOuterCompositeSerialize OuterComposite 

-- *** fakeOuterNumberSerialize

-- | @POST \/fake\/outer\/number@
-- 
-- Test serialization of outer number types
-- 
fakeOuterNumberSerialize 
  :: (Consumes FakeOuterNumberSerialize contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest FakeOuterNumberSerialize contentType OuterNumber accept
fakeOuterNumberSerialize _  _ =
  _mkRequest "POST" ["/fake/outer/number"]

data FakeOuterNumberSerialize 

-- | /Body Param/ "body" - Input number as post body
instance HasBodyParam FakeOuterNumberSerialize OuterNumber 

-- *** fakeOuterStringSerialize

-- | @POST \/fake\/outer\/string@
-- 
-- Test serialization of outer string types
-- 
fakeOuterStringSerialize 
  :: (Consumes FakeOuterStringSerialize contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest FakeOuterStringSerialize contentType OuterString accept
fakeOuterStringSerialize _  _ =
  _mkRequest "POST" ["/fake/outer/string"]

data FakeOuterStringSerialize 

-- | /Body Param/ "body" - Input string as post body
instance HasBodyParam FakeOuterStringSerialize OuterString 

-- *** testClientModel

-- | @PATCH \/fake@
-- 
-- To test \"client\" model
-- 
-- To test \"client\" model
-- 
testClientModel 
  :: (Consumes TestClientModel contentType, MimeRender contentType Client)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Client -- ^ "body" -  client model
  -> SwaggerPetstoreRequest TestClientModel contentType Client accept
testClientModel _  _ body =
  _mkRequest "PATCH" ["/fake"]
    `setBodyParam` body

data TestClientModel 

-- | /Body Param/ "body" - client model
instance HasBodyParam TestClientModel Client 

-- | @application/json@
instance Consumes TestClientModel MimeJSON

-- | @application/json@
instance Produces TestClientModel MimeJSON


-- *** testEndpointParameters

-- | @POST \/fake@
-- 
-- Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
-- 
-- Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
-- 
-- AuthMethod: 'AuthBasicHttpBasicTest'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
testEndpointParameters 
  :: (Consumes TestEndpointParameters contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Number -- ^ "number" -  None
  -> ParamDouble -- ^ "double" -  None
  -> PatternWithoutDelimiter -- ^ "patternWithoutDelimiter" -  None
  -> Byte -- ^ "byte" -  None
  -> SwaggerPetstoreRequest TestEndpointParameters contentType res accept
testEndpointParameters _  _ (Number number) (ParamDouble double) (PatternWithoutDelimiter patternWithoutDelimiter) (Byte byte) =
  _mkRequest "POST" ["/fake"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicHttpBasicTest)
    `addForm` toForm ("number", number)
    `addForm` toForm ("double", double)
    `addForm` toForm ("pattern_without_delimiter", patternWithoutDelimiter)
    `addForm` toForm ("byte", byte)

data TestEndpointParameters  

-- | /Optional Param/ "integer" - None
instance HasOptionalParam TestEndpointParameters ParamInteger where
  applyOptionalParam req (ParamInteger xs) =
    req `addForm` toForm ("integer", xs)

-- | /Optional Param/ "int32" - None
instance HasOptionalParam TestEndpointParameters Int32 where
  applyOptionalParam req (Int32 xs) =
    req `addForm` toForm ("int32", xs)

-- | /Optional Param/ "int64" - None
instance HasOptionalParam TestEndpointParameters Int64 where
  applyOptionalParam req (Int64 xs) =
    req `addForm` toForm ("int64", xs)

-- | /Optional Param/ "float" - None
instance HasOptionalParam TestEndpointParameters ParamFloat where
  applyOptionalParam req (ParamFloat xs) =
    req `addForm` toForm ("float", xs)

-- | /Optional Param/ "string" - None
instance HasOptionalParam TestEndpointParameters ParamString where
  applyOptionalParam req (ParamString xs) =
    req `addForm` toForm ("string", xs)

-- | /Optional Param/ "binary" - None
instance HasOptionalParam TestEndpointParameters ParamBinary where
  applyOptionalParam req (ParamBinary xs) =
    req `addForm` toForm ("binary", xs)

-- | /Optional Param/ "date" - None
instance HasOptionalParam TestEndpointParameters ParamDate where
  applyOptionalParam req (ParamDate xs) =
    req `addForm` toForm ("date", xs)

-- | /Optional Param/ "dateTime" - None
instance HasOptionalParam TestEndpointParameters ParamDateTime where
  applyOptionalParam req (ParamDateTime xs) =
    req `addForm` toForm ("dateTime", xs)

-- | /Optional Param/ "password" - None
instance HasOptionalParam TestEndpointParameters Password where
  applyOptionalParam req (Password xs) =
    req `addForm` toForm ("password", xs)

-- | /Optional Param/ "callback" - None
instance HasOptionalParam TestEndpointParameters Callback where
  applyOptionalParam req (Callback xs) =
    req `addForm` toForm ("callback", xs)

-- | @application/xml; charset=utf-8@
instance Consumes TestEndpointParameters MimeXmlCharsetutf8
-- | @application/json; charset=utf-8@
instance Consumes TestEndpointParameters MimeJsonCharsetutf8

-- | @application/xml; charset=utf-8@
instance Produces TestEndpointParameters MimeXmlCharsetutf8
-- | @application/json; charset=utf-8@
instance Produces TestEndpointParameters MimeJsonCharsetutf8


-- *** testEnumParameters

-- | @GET \/fake@
-- 
-- To test enum parameters
-- 
-- To test enum parameters
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
testEnumParameters 
  :: (Consumes TestEnumParameters contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest TestEnumParameters contentType res accept
testEnumParameters _  _ =
  _mkRequest "GET" ["/fake"]

data TestEnumParameters  

-- | /Optional Param/ "enum_form_string_array" - Form parameter enum test (string array)
instance HasOptionalParam TestEnumParameters EnumFormStringArray where
  applyOptionalParam req (EnumFormStringArray xs) =
    req `addForm` toFormColl CommaSeparated ("enum_form_string_array", xs)

-- | /Optional Param/ "enum_form_string" - Form parameter enum test (string)
instance HasOptionalParam TestEnumParameters EnumFormString where
  applyOptionalParam req (EnumFormString xs) =
    req `addForm` toForm ("enum_form_string", xs)

-- | /Optional Param/ "enum_header_string_array" - Header parameter enum test (string array)
instance HasOptionalParam TestEnumParameters EnumHeaderStringArray where
  applyOptionalParam req (EnumHeaderStringArray xs) =
    req `setHeader` toHeaderColl CommaSeparated ("enum_header_string_array", xs)

-- | /Optional Param/ "enum_header_string" - Header parameter enum test (string)
instance HasOptionalParam TestEnumParameters EnumHeaderString where
  applyOptionalParam req (EnumHeaderString xs) =
    req `setHeader` toHeader ("enum_header_string", xs)

-- | /Optional Param/ "enum_query_string_array" - Query parameter enum test (string array)
instance HasOptionalParam TestEnumParameters EnumQueryStringArray where
  applyOptionalParam req (EnumQueryStringArray xs) =
    req `setQuery` toQueryColl CommaSeparated ("enum_query_string_array", Just xs)

-- | /Optional Param/ "enum_query_string" - Query parameter enum test (string)
instance HasOptionalParam TestEnumParameters EnumQueryString where
  applyOptionalParam req (EnumQueryString xs) =
    req `setQuery` toQuery ("enum_query_string", Just xs)

-- | /Optional Param/ "enum_query_integer" - Query parameter enum test (double)
instance HasOptionalParam TestEnumParameters EnumQueryInteger where
  applyOptionalParam req (EnumQueryInteger xs) =
    req `setQuery` toQuery ("enum_query_integer", Just xs)

-- | /Optional Param/ "enum_query_double" - Query parameter enum test (double)
instance HasOptionalParam TestEnumParameters EnumQueryDouble where
  applyOptionalParam req (EnumQueryDouble xs) =
    req `addForm` toForm ("enum_query_double", xs)

-- | @*/*@
instance MimeType mtype => Consumes TestEnumParameters mtype

-- | @*/*@
instance MimeType mtype => Produces TestEnumParameters mtype


-- *** testInlineAdditionalProperties

-- | @POST \/fake\/inline-additionalProperties@
-- 
-- test inline additionalProperties
-- 
-- 
-- 
testInlineAdditionalProperties 
  :: (Consumes TestInlineAdditionalProperties contentType, MimeRender contentType A.Value)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> A.Value -- ^ "param" -  request body
  -> SwaggerPetstoreRequest TestInlineAdditionalProperties contentType NoContent MimeNoContent
testInlineAdditionalProperties _ param =
  _mkRequest "POST" ["/fake/inline-additionalProperties"]
    `setBodyParam` param

data TestInlineAdditionalProperties 

-- | /Body Param/ "param" - request body
instance HasBodyParam TestInlineAdditionalProperties A.Value 

-- | @application/json@
instance Consumes TestInlineAdditionalProperties MimeJSON


-- *** testJsonFormData

-- | @GET \/fake\/jsonFormData@
-- 
-- test json serialization of form data
-- 
-- 
-- 
testJsonFormData 
  :: (Consumes TestJsonFormData contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Param -- ^ "param" -  field1
  -> Param2 -- ^ "param2" -  field2
  -> SwaggerPetstoreRequest TestJsonFormData contentType NoContent MimeNoContent
testJsonFormData _ (Param param) (Param2 param2) =
  _mkRequest "GET" ["/fake/jsonFormData"]
    `addForm` toForm ("param", param)
    `addForm` toForm ("param2", param2)

data TestJsonFormData  

-- | @application/json@
instance Consumes TestJsonFormData MimeJSON


-- ** FakeClassnameTags123

-- *** testClassname

-- | @PATCH \/fake_classname_test@
-- 
-- To test class name in snake case
-- 
-- AuthMethod: 'AuthApiKeyApiKeyQuery'
-- 
testClassname 
  :: (Consumes TestClassname contentType, MimeRender contentType Client)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Client -- ^ "body" -  client model
  -> SwaggerPetstoreRequest TestClassname contentType Client accept
testClassname _  _ body =
  _mkRequest "PATCH" ["/fake_classname_test"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKeyQuery)
    `setBodyParam` body

data TestClassname 

-- | /Body Param/ "body" - client model
instance HasBodyParam TestClassname Client 

-- | @application/json@
instance Consumes TestClassname MimeJSON

-- | @application/json@
instance Produces TestClassname MimeJSON


-- ** Pet

-- *** addPet

-- | @POST \/pet@
-- 
-- Add a new pet to the store
-- 
-- 
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
addPet 
  :: (Consumes AddPet contentType, MimeRender contentType Pet)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest AddPet contentType res accept
addPet _  _ body =
  _mkRequest "POST" ["/pet"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)
    `setBodyParam` body

data AddPet 

-- | /Body Param/ "body" - Pet object that needs to be added to the store
instance HasBodyParam AddPet Pet 

-- | @application/json@
instance Consumes AddPet MimeJSON
-- | @application/xml@
instance Consumes AddPet MimeXML

-- | @application/xml@
instance Produces AddPet MimeXML
-- | @application/json@
instance Produces AddPet MimeJSON


-- *** deletePet

-- | @DELETE \/pet\/{petId}@
-- 
-- Deletes a pet
-- 
-- 
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
deletePet 
  :: Accept accept -- ^ request accept ('MimeType')
  -> PetId -- ^ "petId" -  Pet id to delete
  -> SwaggerPetstoreRequest DeletePet MimeNoContent res accept
deletePet  _ (PetId petId) =
  _mkRequest "DELETE" ["/pet/",toPath petId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)

data DeletePet  
instance HasOptionalParam DeletePet ApiKey where
  applyOptionalParam req (ApiKey xs) =
    req `setHeader` toHeader ("api_key", xs)
-- | @application/xml@
instance Produces DeletePet MimeXML
-- | @application/json@
instance Produces DeletePet MimeJSON


-- *** findPetsByStatus

-- | @GET \/pet\/findByStatus@
-- 
-- Finds Pets by status
-- 
-- Multiple status values can be provided with comma separated strings
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
findPetsByStatus 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Status -- ^ "status" -  Status values that need to be considered for filter
  -> SwaggerPetstoreRequest FindPetsByStatus MimeNoContent [Pet] accept
findPetsByStatus  _ (Status status) =
  _mkRequest "GET" ["/pet/findByStatus"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)
    `setQuery` toQueryColl CommaSeparated ("status", Just status)

data FindPetsByStatus  
-- | @application/xml@
instance Produces FindPetsByStatus MimeXML
-- | @application/json@
instance Produces FindPetsByStatus MimeJSON


-- *** findPetsByTags

-- | @GET \/pet\/findByTags@
-- 
-- Finds Pets by tags
-- 
-- Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
findPetsByTags 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Tags -- ^ "tags" -  Tags to filter by
  -> SwaggerPetstoreRequest FindPetsByTags MimeNoContent [Pet] accept
findPetsByTags  _ (Tags tags) =
  _mkRequest "GET" ["/pet/findByTags"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)
    `setQuery` toQueryColl CommaSeparated ("tags", Just tags)

{-# DEPRECATED findPetsByTags "" #-}

data FindPetsByTags  
-- | @application/xml@
instance Produces FindPetsByTags MimeXML
-- | @application/json@
instance Produces FindPetsByTags MimeJSON


-- *** getPetById

-- | @GET \/pet\/{petId}@
-- 
-- Find pet by ID
-- 
-- Returns a single pet
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getPetById 
  :: Accept accept -- ^ request accept ('MimeType')
  -> PetId -- ^ "petId" -  ID of pet to return
  -> SwaggerPetstoreRequest GetPetById MimeNoContent Pet accept
getPetById  _ (PetId petId) =
  _mkRequest "GET" ["/pet/",toPath petId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetPetById  
-- | @application/xml@
instance Produces GetPetById MimeXML
-- | @application/json@
instance Produces GetPetById MimeJSON


-- *** updatePet

-- | @PUT \/pet@
-- 
-- Update an existing pet
-- 
-- 
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updatePet 
  :: (Consumes UpdatePet contentType, MimeRender contentType Pet)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Pet -- ^ "body" -  Pet object that needs to be added to the store
  -> SwaggerPetstoreRequest UpdatePet contentType res accept
updatePet _  _ body =
  _mkRequest "PUT" ["/pet"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)
    `setBodyParam` body

data UpdatePet 

-- | /Body Param/ "body" - Pet object that needs to be added to the store
instance HasBodyParam UpdatePet Pet 

-- | @application/json@
instance Consumes UpdatePet MimeJSON
-- | @application/xml@
instance Consumes UpdatePet MimeXML

-- | @application/xml@
instance Produces UpdatePet MimeXML
-- | @application/json@
instance Produces UpdatePet MimeJSON


-- *** updatePetWithForm

-- | @POST \/pet\/{petId}@
-- 
-- Updates a pet in the store with form data
-- 
-- 
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updatePetWithForm 
  :: (Consumes UpdatePetWithForm contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> PetId -- ^ "petId" -  ID of pet that needs to be updated
  -> SwaggerPetstoreRequest UpdatePetWithForm contentType res accept
updatePetWithForm _  _ (PetId petId) =
  _mkRequest "POST" ["/pet/",toPath petId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)

data UpdatePetWithForm  

-- | /Optional Param/ "name" - Updated name of the pet
instance HasOptionalParam UpdatePetWithForm Name2 where
  applyOptionalParam req (Name2 xs) =
    req `addForm` toForm ("name", xs)

-- | /Optional Param/ "status" - Updated status of the pet
instance HasOptionalParam UpdatePetWithForm StatusText where
  applyOptionalParam req (StatusText xs) =
    req `addForm` toForm ("status", xs)

-- | @application/x-www-form-urlencoded@
instance Consumes UpdatePetWithForm MimeFormUrlEncoded

-- | @application/xml@
instance Produces UpdatePetWithForm MimeXML
-- | @application/json@
instance Produces UpdatePetWithForm MimeJSON


-- *** uploadFile

-- | @POST \/pet\/{petId}\/uploadImage@
-- 
-- uploads an image
-- 
-- 
-- 
-- AuthMethod: 'AuthOAuthPetstoreAuth'
-- 
uploadFile 
  :: (Consumes UploadFile contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> PetId -- ^ "petId" -  ID of pet to update
  -> SwaggerPetstoreRequest UploadFile contentType ApiResponse accept
uploadFile _  _ (PetId petId) =
  _mkRequest "POST" ["/pet/",toPath petId,"/uploadImage"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPetstoreAuth)

data UploadFile  

-- | /Optional Param/ "additionalMetadata" - Additional data to pass to server
instance HasOptionalParam UploadFile AdditionalMetadata where
  applyOptionalParam req (AdditionalMetadata xs) =
    req `_addMultiFormPart` NH.partLBS "additionalMetadata" (mimeRender' MimeMultipartFormData xs)

-- | /Optional Param/ "file" - file to upload
instance HasOptionalParam UploadFile File where
  applyOptionalParam req (File xs) =
    req `_addMultiFormPart` NH.partFileSource "file" xs

-- | @multipart/form-data@
instance Consumes UploadFile MimeMultipartFormData

-- | @application/json@
instance Produces UploadFile MimeJSON


-- ** Store

-- *** deleteOrder

-- | @DELETE \/store\/order\/{order_id}@
-- 
-- Delete purchase order by ID
-- 
-- For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
deleteOrder 
  :: Accept accept -- ^ request accept ('MimeType')
  -> OrderIdText -- ^ "orderId" -  ID of the order that needs to be deleted
  -> SwaggerPetstoreRequest DeleteOrder MimeNoContent res accept
deleteOrder  _ (OrderIdText orderId) =
  _mkRequest "DELETE" ["/store/order/",toPath orderId]

data DeleteOrder  
-- | @application/xml@
instance Produces DeleteOrder MimeXML
-- | @application/json@
instance Produces DeleteOrder MimeJSON


-- *** getInventory

-- | @GET \/store\/inventory@
-- 
-- Returns pet inventories by status
-- 
-- Returns a map of status codes to quantities
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getInventory 
  :: Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest GetInventory MimeNoContent ((Map.Map String Int)) accept
getInventory  _ =
  _mkRequest "GET" ["/store/inventory"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetInventory  
-- | @application/json@
instance Produces GetInventory MimeJSON


-- *** getOrderById

-- | @GET \/store\/order\/{order_id}@
-- 
-- Find purchase order by ID
-- 
-- For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
-- 
getOrderById 
  :: Accept accept -- ^ request accept ('MimeType')
  -> OrderId -- ^ "orderId" -  ID of pet that needs to be fetched
  -> SwaggerPetstoreRequest GetOrderById MimeNoContent Order accept
getOrderById  _ (OrderId orderId) =
  _mkRequest "GET" ["/store/order/",toPath orderId]

data GetOrderById  
-- | @application/xml@
instance Produces GetOrderById MimeXML
-- | @application/json@
instance Produces GetOrderById MimeJSON


-- *** placeOrder

-- | @POST \/store\/order@
-- 
-- Place an order for a pet
-- 
-- 
-- 
placeOrder 
  :: (Consumes PlaceOrder contentType, MimeRender contentType Order)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Order -- ^ "body" -  order placed for purchasing the pet
  -> SwaggerPetstoreRequest PlaceOrder contentType Order accept
placeOrder _  _ body =
  _mkRequest "POST" ["/store/order"]
    `setBodyParam` body

data PlaceOrder 

-- | /Body Param/ "body" - order placed for purchasing the pet
instance HasBodyParam PlaceOrder Order 
-- | @application/xml@
instance Produces PlaceOrder MimeXML
-- | @application/json@
instance Produces PlaceOrder MimeJSON


-- ** User

-- *** createUser

-- | @POST \/user@
-- 
-- Create user
-- 
-- This can only be done by the logged in user.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
createUser 
  :: (Consumes CreateUser contentType, MimeRender contentType User)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> User -- ^ "body" -  Created user object
  -> SwaggerPetstoreRequest CreateUser contentType res accept
createUser _  _ body =
  _mkRequest "POST" ["/user"]
    `setBodyParam` body

data CreateUser 

-- | /Body Param/ "body" - Created user object
instance HasBodyParam CreateUser User 
-- | @application/xml@
instance Produces CreateUser MimeXML
-- | @application/json@
instance Produces CreateUser MimeJSON


-- *** createUsersWithArrayInput

-- | @POST \/user\/createWithArray@
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
createUsersWithArrayInput 
  :: (Consumes CreateUsersWithArrayInput contentType, MimeRender contentType Body)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Body -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest CreateUsersWithArrayInput contentType res accept
createUsersWithArrayInput _  _ body =
  _mkRequest "POST" ["/user/createWithArray"]
    `setBodyParam` body

data CreateUsersWithArrayInput 

-- | /Body Param/ "body" - List of user object
instance HasBodyParam CreateUsersWithArrayInput Body 
-- | @application/xml@
instance Produces CreateUsersWithArrayInput MimeXML
-- | @application/json@
instance Produces CreateUsersWithArrayInput MimeJSON


-- *** createUsersWithListInput

-- | @POST \/user\/createWithList@
-- 
-- Creates list of users with given input array
-- 
-- 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
createUsersWithListInput 
  :: (Consumes CreateUsersWithListInput contentType, MimeRender contentType Body)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Body -- ^ "body" -  List of user object
  -> SwaggerPetstoreRequest CreateUsersWithListInput contentType res accept
createUsersWithListInput _  _ body =
  _mkRequest "POST" ["/user/createWithList"]
    `setBodyParam` body

data CreateUsersWithListInput 

-- | /Body Param/ "body" - List of user object
instance HasBodyParam CreateUsersWithListInput Body 
-- | @application/xml@
instance Produces CreateUsersWithListInput MimeXML
-- | @application/json@
instance Produces CreateUsersWithListInput MimeJSON


-- *** deleteUser

-- | @DELETE \/user\/{username}@
-- 
-- Delete user
-- 
-- This can only be done by the logged in user.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
deleteUser 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Username -- ^ "username" -  The name that needs to be deleted
  -> SwaggerPetstoreRequest DeleteUser MimeNoContent res accept
deleteUser  _ (Username username) =
  _mkRequest "DELETE" ["/user/",toPath username]

data DeleteUser  
-- | @application/xml@
instance Produces DeleteUser MimeXML
-- | @application/json@
instance Produces DeleteUser MimeJSON


-- *** getUserByName

-- | @GET \/user\/{username}@
-- 
-- Get user by user name
-- 
-- 
-- 
getUserByName 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Username -- ^ "username" -  The name that needs to be fetched. Use user1 for testing. 
  -> SwaggerPetstoreRequest GetUserByName MimeNoContent User accept
getUserByName  _ (Username username) =
  _mkRequest "GET" ["/user/",toPath username]

data GetUserByName  
-- | @application/xml@
instance Produces GetUserByName MimeXML
-- | @application/json@
instance Produces GetUserByName MimeJSON


-- *** loginUser

-- | @GET \/user\/login@
-- 
-- Logs user into the system
-- 
-- 
-- 
loginUser 
  :: Accept accept -- ^ request accept ('MimeType')
  -> Username -- ^ "username" -  The user name for login
  -> Password -- ^ "password" -  The password for login in clear text
  -> SwaggerPetstoreRequest LoginUser MimeNoContent Text accept
loginUser  _ (Username username) (Password password) =
  _mkRequest "GET" ["/user/login"]
    `setQuery` toQuery ("username", Just username)
    `setQuery` toQuery ("password", Just password)

data LoginUser  
-- | @application/xml@
instance Produces LoginUser MimeXML
-- | @application/json@
instance Produces LoginUser MimeJSON


-- *** logoutUser

-- | @GET \/user\/logout@
-- 
-- Logs out current logged in user session
-- 
-- 
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
logoutUser 
  :: Accept accept -- ^ request accept ('MimeType')
  -> SwaggerPetstoreRequest LogoutUser MimeNoContent res accept
logoutUser  _ =
  _mkRequest "GET" ["/user/logout"]

data LogoutUser  
-- | @application/xml@
instance Produces LogoutUser MimeXML
-- | @application/json@
instance Produces LogoutUser MimeJSON


-- *** updateUser

-- | @PUT \/user\/{username}@
-- 
-- Updated user
-- 
-- This can only be done by the logged in user.
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updateUser 
  :: (Consumes UpdateUser contentType, MimeRender contentType User)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Username -- ^ "username" -  name that need to be deleted
  -> User -- ^ "body" -  Updated user object
  -> SwaggerPetstoreRequest UpdateUser contentType res accept
updateUser _  _ (Username username) body =
  _mkRequest "PUT" ["/user/",toPath username]
    `setBodyParam` body

data UpdateUser 

-- | /Body Param/ "body" - Updated user object
instance HasBodyParam UpdateUser User 
-- | @application/xml@
instance Produces UpdateUser MimeXML
-- | @application/json@
instance Produces UpdateUser MimeJSON



-- * Parameter newtypes

newtype AdditionalMetadata = AdditionalMetadata { unAdditionalMetadata :: Text } deriving (P.Eq, P.Show)
newtype ApiKey = ApiKey { unApiKey :: Text } deriving (P.Eq, P.Show)
newtype Body = Body { unBody :: [User] } deriving (P.Eq, P.Show, A.ToJSON)
newtype Byte = Byte { unByte :: ByteArray } deriving (P.Eq, P.Show)
newtype Callback = Callback { unCallback :: Text } deriving (P.Eq, P.Show)
newtype EnumFormString = EnumFormString { unEnumFormString :: E'EnumFormString } deriving (P.Eq, P.Show)
newtype EnumFormStringArray = EnumFormStringArray { unEnumFormStringArray :: [E'Inner2] } deriving (P.Eq, P.Show)
newtype EnumHeaderString = EnumHeaderString { unEnumHeaderString :: E'EnumFormString } deriving (P.Eq, P.Show)
newtype EnumHeaderStringArray = EnumHeaderStringArray { unEnumHeaderStringArray :: [E'Inner2] } deriving (P.Eq, P.Show)
newtype EnumQueryDouble = EnumQueryDouble { unEnumQueryDouble :: E'EnumNumber } deriving (P.Eq, P.Show)
newtype EnumQueryInteger = EnumQueryInteger { unEnumQueryInteger :: E'EnumQueryInteger } deriving (P.Eq, P.Show)
newtype EnumQueryString = EnumQueryString { unEnumQueryString :: E'EnumFormString } deriving (P.Eq, P.Show)
newtype EnumQueryStringArray = EnumQueryStringArray { unEnumQueryStringArray :: [E'Inner2] } deriving (P.Eq, P.Show)
newtype File = File { unFile :: FilePath } deriving (P.Eq, P.Show)
newtype Int32 = Int32 { unInt32 :: Int } deriving (P.Eq, P.Show)
newtype Int64 = Int64 { unInt64 :: Integer } deriving (P.Eq, P.Show)
newtype Name2 = Name2 { unName2 :: Text } deriving (P.Eq, P.Show)
newtype Number = Number { unNumber :: Double } deriving (P.Eq, P.Show)
newtype OrderId = OrderId { unOrderId :: Integer } deriving (P.Eq, P.Show)
newtype OrderIdText = OrderIdText { unOrderIdText :: Text } deriving (P.Eq, P.Show)
newtype Param = Param { unParam :: Text } deriving (P.Eq, P.Show)
newtype Param2 = Param2 { unParam2 :: Text } deriving (P.Eq, P.Show)
newtype ParamBinary = ParamBinary { unParamBinary :: Binary } deriving (P.Eq, P.Show)
newtype ParamDate = ParamDate { unParamDate :: Date } deriving (P.Eq, P.Show)
newtype ParamDateTime = ParamDateTime { unParamDateTime :: DateTime } deriving (P.Eq, P.Show)
newtype ParamDouble = ParamDouble { unParamDouble :: Double } deriving (P.Eq, P.Show)
newtype ParamFloat = ParamFloat { unParamFloat :: Float } deriving (P.Eq, P.Show)
newtype ParamInteger = ParamInteger { unParamInteger :: Int } deriving (P.Eq, P.Show)
newtype ParamString = ParamString { unParamString :: Text } deriving (P.Eq, P.Show)
newtype Password = Password { unPassword :: Text } deriving (P.Eq, P.Show)
newtype PatternWithoutDelimiter = PatternWithoutDelimiter { unPatternWithoutDelimiter :: Text } deriving (P.Eq, P.Show)
newtype PetId = PetId { unPetId :: Integer } deriving (P.Eq, P.Show)
newtype Status = Status { unStatus :: [E'Status2] } deriving (P.Eq, P.Show)
newtype StatusText = StatusText { unStatusText :: Text } deriving (P.Eq, P.Show)
newtype Tags = Tags { unTags :: [Text] } deriving (P.Eq, P.Show)
newtype Username = Username { unUsername :: Text } deriving (P.Eq, P.Show)

-- * Auth Methods

-- ** AuthApiKeyApiKey
data AuthApiKeyApiKey =
  AuthApiKeyApiKey Text -- ^ secret
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthApiKeyApiKey where
  applyAuthMethod _ a@(AuthApiKeyApiKey secret) req =
    P.pure $
    if (P.typeOf a `P.elem` rAuthTypes req)
      then req `setHeader` toHeader ("api_key", secret)
           & L.over rAuthTypesL (P.filter (/= P.typeOf a))
      else req

-- ** AuthApiKeyApiKeyQuery
data AuthApiKeyApiKeyQuery =
  AuthApiKeyApiKeyQuery Text -- ^ secret
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthApiKeyApiKeyQuery where
  applyAuthMethod _ a@(AuthApiKeyApiKeyQuery secret) req =
    P.pure $
    if (P.typeOf a `P.elem` rAuthTypes req)
      then req `setQuery` toQuery ("api_key_query", Just secret)
           & L.over rAuthTypesL (P.filter (/= P.typeOf a))
      else req

-- ** AuthBasicHttpBasicTest
data AuthBasicHttpBasicTest =
  AuthBasicHttpBasicTest B.ByteString B.ByteString -- ^ username password
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthBasicHttpBasicTest where
  applyAuthMethod _ a@(AuthBasicHttpBasicTest user pw) req =
    P.pure $
    if (P.typeOf a `P.elem` rAuthTypes req)
      then req `setHeader` toHeader ("Authorization", T.decodeUtf8 cred)
           & L.over rAuthTypesL (P.filter (/= P.typeOf a))
      else req
    where cred = BC.append "Basic " (B64.encode $ BC.concat [ user, ":", pw ])

-- ** AuthOAuthPetstoreAuth
data AuthOAuthPetstoreAuth =
  AuthOAuthPetstoreAuth Text -- ^ secret
  deriving (P.Eq, P.Show, P.Typeable)

instance AuthMethod AuthOAuthPetstoreAuth where
  applyAuthMethod _ a@(AuthOAuthPetstoreAuth secret) req =
    P.pure $
    if (P.typeOf a `P.elem` rAuthTypes req)
      then req `setHeader` toHeader ("Authorization", "Bearer " <> secret) 
           & L.over rAuthTypesL (P.filter (/= P.typeOf a))
      else req



-- * Custom Mime Types

-- ** MimeJsonCharsetutf8

data MimeJsonCharsetutf8 = MimeJsonCharsetutf8 deriving (P.Typeable)

-- | @application/json; charset=utf-8@
instance MimeType MimeJsonCharsetutf8 where
  mimeType _ = Just $ P.fromString "application/json; charset=utf-8"
instance A.ToJSON a => MimeRender MimeJsonCharsetutf8 a where mimeRender _ = A.encode
instance A.FromJSON a => MimeUnrender MimeJsonCharsetutf8 a where mimeUnrender _ = A.eitherDecode

-- ** MimeXmlCharsetutf8

data MimeXmlCharsetutf8 = MimeXmlCharsetutf8 deriving (P.Typeable)

-- | @application/xml; charset=utf-8@
instance MimeType MimeXmlCharsetutf8 where
  mimeType _ = Just $ P.fromString "application/xml; charset=utf-8"
-- instance MimeRender MimeXmlCharsetutf8 T.Text where mimeRender _ = undefined
-- instance MimeUnrender MimeXmlCharsetutf8 T.Text where mimeUnrender _ = undefined


