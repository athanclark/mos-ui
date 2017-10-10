module Types.DBus where

import Prelude
import Type.Proxy (Proxy (..))
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail, jsonParser)
import DBus.Signature (class IsVariant, toVariant, fromVariant, class IsValue, typeOf)




data AllInputs
  = ControlInput ControlInput

instance encodeJsonAllInputs :: EncodeJson AllInputs where
  encodeJson (ControlInput c) = encodeJson c

instance decodeJsonAllInputs :: DecodeJson AllInputs where
  decodeJson json = ControlInput <$> decodeJson json



data ControlInput
  = Foo

instance encodeJsonControlInput :: EncodeJson ControlInput where
  encodeJson Foo = encodeJson "foo"

instance decodeJsonControlInput :: DecodeJson ControlInput where
  decodeJson json = do
    s <- decodeJson json
    case s of
      _ | s == "foo" -> pure Foo
        | otherwise  -> fail "Not a ControlInput"

instance isVariantControlInput :: IsVariant ControlInput where
  toVariant = toVariant <<< show <<< encodeJson
  fromVariant v = do
    s <- fromVariant v
    case jsonParser s >>= decodeJson of
      Left _ -> Nothing
      Right x -> pure x

instance isValueControlInput :: IsValue ControlInput where
  typeOf Proxy = typeOf (Proxy :: Proxy String)


data ControlOutput
  = Bar

instance encodeJsonControlOutput :: EncodeJson ControlOutput where
  encodeJson Bar = encodeJson "bar"

instance decodeJsonControlOutput :: DecodeJson ControlOutput where
  decodeJson json = do
    s <- decodeJson json
    case s of
      _ | s == "bar" -> pure Bar
        | otherwise  -> fail "Not a ControlOutput"

instance isVariantControlOutput :: IsVariant ControlOutput where
  toVariant = toVariant <<< show <<< encodeJson
  fromVariant v = do
    s <- fromVariant v
    case jsonParser s >>= decodeJson of
      Left _ -> Nothing
      Right x -> pure x

instance isValueControlOutput :: IsValue ControlOutput where
  typeOf Proxy = typeOf (Proxy :: Proxy String)
