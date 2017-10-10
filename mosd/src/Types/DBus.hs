{-# LANGUAGE
    OverloadedStrings
  #-}

module Types.DBus where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (..), encode, decode)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text.Lazy.Encoding as LT
import DBus.Internal.Types (IsVariant (..), IsValue (..), Type (TypeString))


data ControlInput
  = Foo

instance FromJSON ControlInput where
  parseJSON (String s) | s == "foo" = pure Foo
                       | otherwise  = fail "Not a ControlInput"
  parseJSON x = typeMismatch "ControlInput" x

instance ToJSON ControlInput where
  toJSON Foo = String "foo"

instance IsVariant ControlInput where
  toVariant x = toVariant $ LT.decodeUtf8 $ encode x
  fromVariant x = (decode . LT.encodeUtf8) =<< fromVariant x

instance IsValue ControlInput where
  typeOf _ = TypeString
  toValue x = toValue $ LT.decodeUtf8 $ encode x
  fromValue x = (decode . LT.encodeUtf8) =<< fromValue x



data ControlOutput
  = Bar

instance ToJSON ControlOutput where
  toJSON Bar = String "bar"

instance FromJSON ControlOutput where
  parseJSON (String s) | s == "bar" = pure Bar
                       | otherwise  = fail "Not a ControlOutput"
  parseJSON x = typeMismatch "ControlOutput" x

instance IsVariant ControlOutput where
  toVariant x = toVariant $ LT.decodeUtf8 $ encode x
  fromVariant x = (decode . LT.encodeUtf8) =<< fromVariant x

instance IsValue ControlOutput where
  typeOf _ = TypeString
  toValue x = toValue $ LT.decodeUtf8 $ encode x
  fromValue x = (decode . LT.encodeUtf8) =<< fromValue x

