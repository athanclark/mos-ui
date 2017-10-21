{-# LANGUAGE
    OverloadedStrings
  #-}

module Types.DBus where

import Monerodo.MoneroD (MoneroDLog)
import System.SystemD.Status (SystemDStatus)

import Data.Aeson (ToJSON (..), FromJSON (..), Value (..), encode, decode, (.:), (.=), object)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import DBus.Internal.Types (IsVariant (..), IsValue (..), Type (TypeString))


data Service
  = ServiceMoneroD

instance ToJSON Service where
  toJSON ServiceMoneroD = String "monerod"

instance FromJSON Service where
  parseJSON (String s) | s == "monerod" = pure ServiceMoneroD
                       | otherwise = fail "not a Service"
  parseJSON x = typeMismatch "Service" x

instance Show Service where
  show x = LT.unpack $ LT.decodeUtf8 $ encode x

data ControlInput
  = GetServiceState (Maybe Service)

instance FromJSON ControlInput where
  parseJSON (Object o) = do
    let parseGetServiceState = GetServiceState <$> o .: "getServiceState"
    parseGetServiceState
  parseJSON x = typeMismatch "ControlInput" x

instance ToJSON ControlInput where
  toJSON (GetServiceState mService) = object
    [ "getServiceState" .= mService
    ]

instance IsVariant ControlInput where
  toVariant x = toVariant $ LT.decodeUtf8 $ encode x
  fromVariant x = (decode . LT.encodeUtf8) =<< fromVariant x

instance IsValue ControlInput where
  typeOf _ = TypeString
  toValue x = toValue $ LT.decodeUtf8 $ encode x
  fromValue x = (decode . LT.encodeUtf8) =<< fromValue x



data ControlOutput
  = GotServiceState [SystemDStatus]

instance ToJSON ControlOutput where
  toJSON (GotServiceState xs) = object
    [ "gotServiceState" .= xs
    ]

instance FromJSON ControlOutput where
  parseJSON (Object o) = do
    let gotServiceState = GotServiceState <$> o .: "gotServiceState"
    gotServiceState
  parseJSON x = typeMismatch "ControlOutput" x

instance IsVariant ControlOutput where
  toVariant x = toVariant $ LT.decodeUtf8 $ encode x
  fromVariant x = (decode . LT.encodeUtf8) =<< fromVariant x

instance IsValue ControlOutput where
  typeOf _ = TypeString
  toValue x = toValue $ LT.decodeUtf8 $ encode x
  fromValue x = (decode . LT.encodeUtf8) =<< fromValue x



data SignalOutput
  = MoneroDLogSignal MoneroDLog

instance ToJSON SignalOutput where
  toJSON (MoneroDLogSignal l) = object ["monerod" .= l]

instance FromJSON SignalOutput where
  parseJSON (Object o) = MoneroDLogSignal <$> o .: "monerod"
  parseJSON x = typeMismatch "SignalOutput" x

instance IsVariant SignalOutput where
  toVariant x = toVariant $ LT.decodeUtf8 $ encode x
  fromVariant x = (decode . LT.encodeUtf8) =<< fromVariant x

instance IsValue SignalOutput where
  typeOf _ = TypeString
  toValue x = toValue $ LT.decodeUtf8 $ encode x
  fromValue x = (decode . LT.encodeUtf8) =<< fromValue x
