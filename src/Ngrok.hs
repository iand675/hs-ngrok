{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
-- |
-- Module : Ngrok
-- Copyright : (C) 2015 Ian Duncan
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Ian Duncan <ian@iankduncan.com>
-- Stability : provisional
--
-- Client library for managing the behavior of a local ngrok process.
-- Useful for inspecting running tunnels, starting new tunnels, stopping
-- existing tunnels, replaying HTTP requests, inspecting previously made
-- HTTP requests, and deleting recorded requests
-------------------------------------------------------------------------------

module Ngrok
  ( -- * Basic client initialization
    -- $setup
    NgrokConfig(..)
  , Ngrok
  , ngrok
  , defaultNgrokConfig
    -- * Creating new tunnels
  , NewTunnel
  , HasName(..)
  , HasAddr(..)
  , HasInspect(..)
  , HasAuth(..)
  , HasBindTls(..)
  , HasSubdomain(..)
  , HasHostname(..)
  , HasRemoteAddr(..)
  , HasCrt(..)
  , HasKey(..)
  , HasClientCas(..)
  , httpTunnel
  , tlsTunnel
  , tcpTunnel
    -- * Launch a new tunnel with the provided configuration.
  , startTunnel
    -- * Listing existing tunnels
  , listTunnels
  , TunnelsList(..)
    -- * Getting specific tunnel details
  , tunnelDetails
  , TunnelInfo(..)
    -- * Stopping existing tunnels
  , stopTunnel
    -- * Other types
  , TunnelName(..)
  , TunnelProtocol(..)
  , TunnelMetrics(..)
  , ConnMetrics(..)
  , HttpMetrics(..)
    {-
  , listCapturedRequests
  , replayCapturedRequest
  , deleteCapturedRequests
  , capturedRequestDetail
-}
    -- * Primitive access
  , ngrokManager
  , ngrokBaseRequest
  , customNgrokClient
  ) where

import Blaze.ByteString.Builder (toByteString)
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector)
import Network.HTTP.Client hiding (Request, method)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types
import Network.HTTP.Types.QueryLike

-- $setup
--
-- The following example demonstrates how to initialize a client,
-- stop existing tunnels, and launch a new tunnel called @hs-ngrok-experiment@ on
-- port 3000.
--
-- >>> :set -XOverloadedStrings
-- >>> import Ngrok
-- >>> n <- ngrok defaultNgrokClient
-- >>> ts <- listTunnels n
-- >>> mapM_ (stopTunnel n . tunnelInfoName) (tunnelsListTunnels ts)
-- >>> startTunnel n $ httpTunnel (TunnelName "hs-ngrok-experiment") "3000"

data NgrokConfig = NgrokConfig
                   { ngrokConfigBaseUrl :: String
                   }
                 deriving (Show)

-- | Opaque ngrok client handle. Used to communicate with the ngrok server
data Ngrok = Ngrok
             { ngrokManager     :: Manager
             , ngrokBaseRequest :: C.Request
             }

-- | Create an ngrok client handle from an arbitrary request and handle.
-- With this configuration, it is your responsibility to set any headers
-- or otherwise configure the base request appropriately.
customNgrokClient :: Manager -> C.Request -> Ngrok
customNgrokClient = Ngrok

-- | Default ngrok client configuration. Defaults to @127.0.0.1:4040@
defaultNgrokConfig :: NgrokConfig
defaultNgrokConfig = NgrokConfig "http://127.0.0.1:4040/api"

-- | Create new client handle
ngrok :: NgrokConfig -> IO Ngrok
ngrok c = do
  r <- parseUrl $ ngrokConfigBaseUrl c
  m <- newManager defaultManagerSettings
  return $ Ngrok m (r { requestHeaders = ("Content-Type", "application/json") : requestHeaders r })

newtype TunnelName = TunnelName Text
                   deriving (Show, ToJSON, FromJSON)

data NewTunnelProtocol = NewHttpTunnel
                       | NewTcpTunnel
                       | NewTlsTunnel
              deriving (Show)

instance ToJSON NewTunnelProtocol where
  toJSON p = String $ case p of
    NewHttpTunnel -> "http"
    NewTcpTunnel -> "tcp"
    NewTlsTunnel -> "tls"

instance FromJSON NewTunnelProtocol where
  parseJSON (String s) = case s of
    "http" -> return NewHttpTunnel
    "tcp" -> return NewTcpTunnel
    "tls" -> return NewTlsTunnel
    str -> fail $ show str
  parseJSON _ = mzero

data TunnelProtocol = Http | Https | Tcp | Tls
              deriving (Show)

instance ToJSON TunnelProtocol where
  toJSON p = String $ case p of
    Http -> "http"
    Https -> "https"
    Tcp -> "tcp"
    Tls -> "tls"

instance FromJSON TunnelProtocol where
  parseJSON (String s) = case s of
    "http" -> return Http
    "https" -> return Https
    "tcp" -> return Tcp
    "tls" -> return Tls
    str -> fail $ show str
  parseJSON _ = mzero

data BindTls = BindHttps | BindHttp | BindBoth

             deriving (Show)

instance ToJSON BindTls where
  toJSON b = case b of
    BindHttps -> Bool True
    BindHttp -> Bool False
    BindBoth -> String "both"

instance FromJSON BindTls where
  parseJSON (Bool b) = return $ if b then BindHttps else BindHttp
  parseJSON (String "both") = return BindBoth
  parseJSON _ = mzero

data NewTunnel = HttpTunnel
                 { newTunnelName       :: TunnelName
                 , newTunnelAddr       :: Text
                 , newTunnelInspect    :: Maybe Bool
                 , newTunnelAuth       :: Maybe (Text, Text)
                 , newTunnelBindTls    :: Maybe BindTls
                 , newTunnelSubdomain  :: Maybe Text
                 , newTunnelHostname   :: Maybe Text
                 }
               | TcpTunnel
                 { newTunnelName       :: TunnelName
                 , newTunnelAddr       :: Text
                 , newTunnelInspect    :: Maybe Bool
                 , newTunnelRemoteAddr :: Maybe Text
                 }
               | TlsTunnel
                 { newTunnelName       :: TunnelName
                 , newTunnelAddr       :: Text
                 , newTunnelInspect    :: Maybe Bool
                 , newTunnelSubdomain  :: Maybe Text
                 , newTunnelHostname   :: Maybe Text
                 , newTunnelCrt        :: Maybe FilePath
                 , newTunnelKey        :: Maybe FilePath
                 , newTunnelClientCas  :: Maybe FilePath
                 }
               deriving (Show)

makeFields ''NewTunnel

instance ToJSON NewTunnel where
  toJSON t = case t of
    (HttpTunnel n addr insp a tls sub host) ->
      object [ "proto" .= ("http" :: Text)
             , "name" .= n
             , "addr" .= addr
             , "inspect" .= insp
             , "auth" .= a
             , "bind_tls" .= tls
             , "subdomain" .= sub
             , "hostname" .= host
             ]
    (TcpTunnel n addr insp ra) ->
      object [ "proto" .= ("tcp" :: Text)
             , "name" .= n
             , "addr" .= addr
             , "inspect" .= insp
             , "remote_addr" .= ra
             ]
    (TlsTunnel n addr insp sub host c k cas) ->
      object [ "proto" .= ("tls" :: Text)
             , "name" .= n
             , "addr" .= addr
             , "inspect" .= insp
             , "subdomain" .= sub
             , "hostname" .= host
             , "crt" .= c
             , "key" .= k
             , "client_cas" .= cas
             ]

-- | Configuration for creating an http(s) tunnel
httpTunnel :: TunnelName -> Text -> NewTunnel
httpTunnel n t = HttpTunnel n t Nothing Nothing Nothing Nothing Nothing

-- | Configuration for creating a raw tcp tunnel
tcpTunnel :: TunnelName -> Text -> NewTunnel
tcpTunnel n t = TcpTunnel n t Nothing Nothing

-- | Configuration for creating a tls tunnel
tlsTunnel :: TunnelName -> Text -> NewTunnel
tlsTunnel n t = TlsTunnel n t Nothing Nothing Nothing Nothing Nothing Nothing

data TunnelConfig = TunnelConfig
                    { tunnelConfigAddr    :: Text
                    , tunnelConfigInspect :: Bool
                    }
                  deriving (Show)

makeFields ''TunnelConfig

data ConnMetrics = ConnMetrics
                   { connMetricsCount  :: Double
                   , connMetricsGauge  :: Double
                   , connMetricsRate1  :: Double
                   , connMetricsRate5  :: Double
                   , connMetricsRate15 :: Double
                   , connMetricsP50    :: Double
                   , connMetricsP90    :: Double
                   , connMetricsP95    :: Double
                   , connMetricsP99    :: Double
                   }
                 deriving (Show)

makeFields ''ConnMetrics

instance ToJSON ConnMetrics where
  toJSON m = object [ "count" .= connMetricsCount m
                    , "gauge" .= connMetricsGauge m
                    , "rate1" .= connMetricsRate1 m
                    , "rate5" .= connMetricsRate5 m
                    , "rate15" .= connMetricsRate15 m
                    , "p50" .= connMetricsP50 m
                    , "p90" .= connMetricsP90 m
                    , "p95" .= connMetricsP95 m
                    , "p99" .= connMetricsP99 m
                    ]

instance FromJSON ConnMetrics where
  parseJSON (Object o) = ConnMetrics
                         <$> o .: "count"
                         <*> o .: "gauge"
                         <*> o .: "rate1"
                         <*> o .: "rate5"
                         <*> o .: "rate15"
                         <*> o .: "p50"
                         <*> o .: "p90"
                         <*> o .: "p95"
                         <*> o .: "p99"
  parseJSON _ = mzero

data HttpMetrics = HttpMetrics
                   { httpMetricsCount  :: Double
                   , httpMetricsRate1  :: Double
                   , httpMetricsRate5  :: Double
                   , httpMetricsRate15 :: Double
                   , httpMetricsP50    :: Double
                   , httpMetricsP90    :: Double
                   , httpMetricsP95    :: Double
                   , httpMetricsP99    :: Double
                   }
                 deriving (Show)

makeFields ''HttpMetrics

instance ToJSON HttpMetrics where
  toJSON m = object [ "count" .= httpMetricsCount m
                    , "rate1" .= httpMetricsRate1 m
                    , "rate5" .= httpMetricsRate5 m
                    , "rate15" .= httpMetricsRate15 m
                    , "p50" .= httpMetricsP50 m
                    , "p90" .= httpMetricsP90 m
                    , "p95" .= httpMetricsP95 m
                    , "p99" .= httpMetricsP99 m
                    ]

instance FromJSON HttpMetrics where
  parseJSON (Object o) = HttpMetrics
                         <$> o .: "count"
                         <*> o .: "rate1"
                         <*> o .: "rate5"
                         <*> o .: "rate15"
                         <*> o .: "p50"
                         <*> o .: "p90"
                         <*> o .: "p95"
                         <*> o .: "p99"
  parseJSON _ = mzero

data TunnelMetrics = TunnelMetrics
                     { tunnelMetricsConns :: ConnMetrics
                     , tunnelMetricsHttp  :: HttpMetrics
                     }
                   deriving (Show)

makeFields ''TunnelMetrics

instance ToJSON TunnelMetrics where
  toJSON m = object [ "conns" .= tunnelMetricsConns m
                    , "http" .= tunnelMetricsHttp m
                    ]

instance FromJSON TunnelMetrics where
  parseJSON (Object o) = TunnelMetrics
                         <$> o .: "conns"
                         <*> o .: "http"
  parseJSON _ = mzero

data TunnelInfo = TunnelInfo
                  { tunnelInfoName      :: TunnelName
                  , tunnelInfoUri       :: Text
                  , tunnelInfoPublicUrl :: Text
                  , tunnelInfoProto     :: TunnelProtocol
                  , tunnelInfoConfig    :: Object -- TODO is this everything but the proto?
                  , tunnelInfoMetrics   :: TunnelMetrics
                  }
                deriving (Show)

makeFields ''TunnelInfo

instance ToJSON TunnelInfo where
  toJSON t = object [ "name" .= tunnelInfoName t
                    , "uri" .= tunnelInfoUri t
                    , "public_url" .= tunnelInfoPublicUrl t
                    , "proto" .= tunnelInfoProto t
                    , "config" .= tunnelInfoConfig t
                    , "metrics" .= tunnelInfoMetrics t
                    ]

instance FromJSON TunnelInfo where
  parseJSON (Object o) = TunnelInfo
                         <$> o .: "name"
                         <*> o .: "uri"
                         <*> o .: "public_url"
                         <*> o .: "proto"
                         <*> o .: "config"
                         <*> o .: "metrics"
  parseJSON _ = mzero

data TunnelsList = TunnelsList
                   { tunnelsListTunnels :: Vector TunnelInfo
                   , tunnelsListUri     :: Text
                   }
                 deriving (Show)

makeFields ''TunnelsList

instance ToJSON TunnelsList where
  toJSON l = object [ "tunnels" .= tunnelsListTunnels l
                    , "uri" .= tunnelsListUri l
                    ]

instance FromJSON TunnelsList where
  parseJSON (Object o) = TunnelsList
                         <$> o .: "tunnels"
                         <*> o .: "uri"
  parseJSON _ = mzero

newtype RequestId = RequestId Text
                  deriving (Show, ToJSON, FromJSON)

data Request = Request
               { requestMethod  :: Method
               , requestProto   :: TunnelProtocol
               , requestHeaders :: HashMap HeaderName (Vector ByteString)
               , requestUri     :: ByteString
               , requestRaw     :: Maybe ByteString
               }
             deriving (Show)

makeFields ''Request

data CapturedRequest = CapturedRequest
                       { capturedRequestUri        :: Text
                       , capturedRequest           :: RequestId
                       , capturedRequestTunnelName :: TunnelName
                       , capturedRequestRemoteAddr :: Text
                       , capturedRequestStart      :: UTCTime
                       , capturedRequestDuration   :: DiffTime
                       , capturedRequestRequest    :: Request
                       }
                     deriving (Show)

makeFields ''CapturedRequest

data CapturedRequests = CapturedRequests
                        { capturedRequestsUri      :: Text
                        , capturedRequestsRequests :: Vector CapturedRequest
                        }
                      deriving (Show)

makeFields ''CapturedRequests

get :: (QueryLike q, FromJSON a) => Ngrok -> [Text] -> q -> IO a
get (Ngrok m r) p q = do
  resp <- httpLbs (r { C.method = methodGet
                     , path = toByteString $ encodePathSegments p
                     , queryString = renderQuery True $ toQuery q
                     }) m
  respJSON resp

post :: ToJSON a => Ngrok -> [Text] -> a -> IO (C.Response L.ByteString)
post (Ngrok m r) p j = httpLbs (r { C.method = methodPost
                                  , path = toByteString $ encodePathSegments p
                                  , requestBody = RequestBodyLBS $ encode j
                                  }) m

post' :: QueryLike q => Ngrok -> [Text] -> q -> IO (C.Response L.ByteString)
post' = undefined

delete :: QueryLike q => Ngrok -> [Text] -> q -> IO ()
delete (Ngrok m r) p q = void $ httpLbs (r { C.method = methodDelete
                                           , path = toByteString $ encodePathSegments p
                                           , queryString = renderQuery True $ toQuery q
                                           }) m

data NgrokJsonError = NgrokJsonError String L.ByteString
                    deriving (Show)

instance Exception NgrokJsonError

respJSON :: FromJSON a => Response L.ByteString -> IO a
respJSON r = case eitherDecode (responseBody r) of
  Left e -> throw $ NgrokJsonError e (responseBody r)
  Right x -> return x

-- | Returns a list of running tunnels with status and metrics information.
listTunnels :: Ngrok -> IO TunnelsList
listTunnels n = get n ["api", "tunnels"] ([] :: [(ByteString, ByteString)])

-- | Dynamically starts a new tunnel on the ngrok client. The request body parameters are the same as those you would use to define the tunnel in the configuration file.
startTunnel :: Ngrok -> NewTunnel -> IO TunnelInfo
startTunnel n nt = post n ["api", "tunnels"] nt >>= respJSON

-- | Get status and metrics about the named running tunnel
tunnelDetails :: Ngrok -> TunnelName -> IO TunnelInfo
tunnelDetails n (TunnelName t) = get n ["api", "tunnels", t] ([] :: [(ByteString, ByteString)])

-- | Stop a running tunnel
stopTunnel :: Ngrok -> TunnelName -> IO ()
stopTunnel n (TunnelName t) = delete n ["api", "tunnels", t] ([] :: [(ByteString, ByteString)])

{-
-- | Returns a list of all HTTP requests captured for inspection. This will only return requests that are still in memory (ngrok evicts captured requests when their memory usage exceeds @inspect_db_size@)
listCapturedRequests :: Ngrok -> IO CapturedRequests
listCapturedRequests n = get n ["api", "requests", "http"] ([] :: [(ByteString, ByteString)])

-- | Replays a request against the local endpoint of a tunnel
replayCapturedRequest :: Ngrok -> RequestId -> Maybe TunnelName -> IO ()
replayCapturedRequest n (RequestId r) mtun =
  void $ post' n ["api", "requests", "http"] [Just ("id" :: ByteString, r), Just ("tunnel_name", undefined)]


-- | Deletes all captured requests
deleteCapturedRequests :: Ngrok -> IO ()
deleteCapturedRequests n = delete n ["api", "requests", "http"] ([] :: [(ByteString, ByteString)])

-- | Returns metadata and raw bytes of a captured request. The raw data is base64-encoded in the JSON response. The response value maybe null if the local server has not yet responded to a request.
capturedRequestDetail :: Ngrok -> RequestId -> IO CapturedRequest
capturedRequestDetail n (RequestId r) = get n ["api", "requests", "http", r] ([] :: [(ByteString, ByteString)])
-}
