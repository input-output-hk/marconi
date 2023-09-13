{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Module: Network.JsonRpc.Types

 Work with JSON-RPC protocol
-}
module Network.JsonRpc.Types (
  -- * API specification types
  RawJsonRpc,
  JsonRpc,
  JsonRpcNotification,
  JsonRpcContentType,

  -- * JSON-RPC messages
  Request (..),
  JsonRpcErr (..),
  JsonRpcResponse (..),
  UnusedRequestParams (..),

  -- ** Smart constructors for standard JSON-RPC errors
  mkJsonRpcParseErr,
  mkJsonRpcInvalidRequestErr,
  mkJsonRpcMethodNotFoundErr,
  mkJsonRpcInvalidParamsErr,
  mkJsonRpcInternalErr,

  -- * Type rewriting
  JsonRpcEndpoint,
) where

import Control.Applicative (liftA3, (<|>))
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Null, Object, String),
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isNothing)
import Data.OpenApi (
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiObject, OpenApiString),
  Referenced (Inline),
  ToSchema (declareNamedSchema),
  declareSchemaRef,
 )
import Data.OpenApi.Lens (enum_, properties, required, type_)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Read (decimal)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Network.HTTP.Media ((//))
import Servant.API (
  Accept (contentTypes),
  JSON,
  MimeRender (mimeRender),
  MimeUnrender (mimeUnrender),
  NoContent,
  Post,
  ReqBody,
  (:>),
 )
import Servant.OpenApi (HasOpenApi (toOpenApi))

-- | Client messages
data Request p = Request
  { method :: !String
  , params :: !p
  , requestId :: !(Maybe Word64)
  -- ^ omitted for notification type messages
  }
  deriving (Eq, Show)

instance (ToJSON p) => ToJSON (Request p) where
  toJSON (Request m p ix) =
    object
      . maybe id (onValue "id") ix
      $ [ "jsonrpc" .= ("2.0" :: String)
        , "method" .= m
        , "params" .= p
        ]
    where
      onValue n v = ((n .= v) :)

instance (FromJSON p) => FromJSON (Request p) where
  parseJSON = withObject "JsonRpc Request" $ \obj -> do
    ix <- obj .:? "id"
    m <- obj .: "method"
    p <- obj .: "params"
    v <- obj .: "jsonrpc"

    versionGuard v . pure $ Request m p ix

instance (ToSchema p) => ToSchema (Request p) where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef $ Proxy @(Maybe Word64)
    methodSchema <- declareSchemaRef $ Proxy @String
    let versionSchema =
          Inline $
            mempty
              & type_ ?~ OpenApiString
              & enum_ ?~ ["2.0"]
    let paramsSchema = Inline mempty
    return $
      NamedSchema (Just "JsonRpc_Request") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("id", idSchema)
               , ("jsonrpc", versionSchema)
               , ("method", methodSchema)
               , ("params", paramsSchema)
               ]
          & required
            .~ [ "jsonrpc"
               , "method"
               , "params"
               ]

-- | JSON-RPC supported version, 2.0 at this time
versionGuard :: Maybe String -> Parser a -> Parser a
versionGuard v x
  | v == Just "2.0" = x
  | isNothing v = x
  | otherwise = fail "unknown version"

-- | Server 'Ack' message
data JsonRpcResponse e r
  = Result !Word64 !r
  | Ack !Word64
  | Errors !(Maybe Word64) !(JsonRpcErr e)
  deriving (Eq, Show, Generic)

instance (ToSchema e, ToSchema r) => ToSchema (JsonRpcResponse e r)

data JsonRpcErr e = JsonRpcErr
  { errorCode :: !Int
  , errorMessage :: !String
  , errorData :: !(Maybe e)
  }
  deriving (Eq, Show, Generic)

instance (ToSchema e) => ToSchema (JsonRpcErr e)

-- | JSON-RPC error codes based on [JSONRPC Spec](https://www.jsonrpc.org/specification#error_object)
mkJsonRpcParseErr :: Maybe e -> JsonRpcErr e
mkJsonRpcParseErr = JsonRpcErr (-32700) "Parse error"

mkJsonRpcInvalidRequestErr :: Maybe e -> JsonRpcErr e
mkJsonRpcInvalidRequestErr = JsonRpcErr (-32600) "Invalid request"

mkJsonRpcMethodNotFoundErr :: Maybe e -> JsonRpcErr e
mkJsonRpcMethodNotFoundErr = JsonRpcErr (-32601) "Method not found"

mkJsonRpcInvalidParamsErr :: Maybe e -> JsonRpcErr e
mkJsonRpcInvalidParamsErr = JsonRpcErr (-32602) "Invalid params"

mkJsonRpcInternalErr :: Maybe e -> JsonRpcErr e
mkJsonRpcInternalErr = JsonRpcErr (-32603) "Internal error"

instance (FromJSON e, FromJSON r) => FromJSON (JsonRpcResponse e r) where
  parseJSON = withObject "Response" $ \obj -> do
    ix <- obj .: "id" <|> (obj .: "id" >>= parseDecimalString)
    version <- obj .:? "jsonrpc"
    result <- obj .:? "result"
    err <- obj .:? "error"
    versionGuard version $ pack ix result err
    where
      parseDecimalString = either fail (pure . fmap fst) . traverse decimal
      pack (Just ix) (Just r) Nothing = pure $ Result ix r
      pack ix Nothing (Just e) = Errors ix <$> parseErr e
      pack (Just ix) Nothing Nothing = pure $ Ack ix
      pack _ _ _ = fail "invalid response"
      parseErr =
        withObject "Error" $
          liftA3 JsonRpcErr <$> (.: "code") <*> (.: "message") <*> (.:? "data")

instance (ToJSON e, ToJSON r) => ToJSON (JsonRpcResponse e r) where
  toJSON (Result ix r) =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "result" .= r
      , "id" .= ix
      ]
  toJSON (Ack ix) =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "id" .= ix
      , "result" .= Null
      , "error" .= Null
      ]
  toJSON (Errors ix (JsonRpcErr c msg err)) =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "id" .= ix
      , "error" .= detail
      ]
    where
      detail =
        object
          [ "code" .= c
          , "message" .= msg
          , "data" .= err
          ]

data UnusedRequestParams = UnusedRequestParams
  deriving (Generic)

instance FromJSON UnusedRequestParams where
  parseJSON Null = pure UnusedRequestParams
  parseJSON (String "") = pure UnusedRequestParams
  parseJSON (Object o) | null o = pure UnusedRequestParams
  parseJSON _ = fail "The param value must be empty (use '{}', 'null' or empty string)"

instance ToJSON UnusedRequestParams where
  toJSON = const Null

instance ToSchema UnusedRequestParams

-- | A JSON RPC server handles any number of methods.
data RawJsonRpc api

instance (HasOpenApi api) => HasOpenApi (RawJsonRpc api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)

{- | JSON-RPC endpoint which respond with a result given a query.

 The type has the following type parameters:

   * @method@: the name of the JSON-RPC method
   * @p@: the `params` field type in the JSON-RPC request
   * @e@: the error type for the `result` field in the JSON-RPC response
   * @r@: the value type of the `result` field in the JSON-RPC response
-}
data JsonRpc (method :: Symbol) p e r

instance (ToSchema p, ToSchema e, ToSchema r) => HasOpenApi (JsonRpc m p e r) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (JsonRpcEndpoint (JsonRpc m p e r)))

-- | JSON-RPC endpoints which do not respond
data JsonRpcNotification (method :: Symbol) p

-- | JSON-RPC
type family JsonRpcEndpoint a where
  JsonRpcEndpoint (JsonRpc m p e r) =
    ReqBody '[JsonRpcContentType] (Request p) :> Post '[JsonRpcContentType] (JsonRpcResponse e r)
  JsonRpcEndpoint (JsonRpcNotification m p) =
    ReqBody '[JsonRpcContentType] (Request p) :> Post '[JsonRpcContentType] NoContent

-- | The JSON-RPC content type.
data JsonRpcContentType

instance Accept JsonRpcContentType where
  contentTypes _ = "application" // "json-rpc" :| ["application" // "json"]

instance (ToJSON a) => MimeRender JsonRpcContentType a where
  mimeRender _ = mimeRender (Proxy @JSON)

instance (FromJSON a) => MimeUnrender JsonRpcContentType a where
  mimeUnrender _ = mimeUnrender (Proxy @JSON)
