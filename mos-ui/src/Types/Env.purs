module Types.Env where

import Arguments (Args (..))
import Types.DBus (SignalOutput)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import DBus (DBUS, Client, connectSession)
import Queue (Queue, newQueue)
import Signal.Channel (CHANNEL)


newtype Env = Env
  { client :: Client
  , signalQueue :: Queue SignalOutput
  , development :: Boolean
  , monerodService :: String
  }

newtype EnvData = EnvData
  { development :: Boolean
  , monerodService :: String
  }

getEnvData :: EnvData -> {development :: Boolean, monerodService :: String}
getEnvData (EnvData x) = x

instance encodeJsonEnvData :: EncodeJson EnvData where
  encodeJson (EnvData {development,monerodService})
    =  "development" := development
    ~> "monerodService" := monerodService
    ~> jsonEmptyObject

instance decodeJsonEnvData :: DecodeJson EnvData where
  decodeJson json = do
    o <- decodeJson json
    development <- o .? "development"
    monerodService <- o .? "monerodService"
    pure $ EnvData
      {development,monerodService}

toEnvData :: Env -> EnvData
toEnvData (Env {development,monerodService}) = EnvData {development,monerodService}


mkEnv :: forall eff
       . Args
      -> Eff ( dbus :: DBUS
             , channel :: CHANNEL
             , exception :: EXCEPTION
             , ref :: REF
             | eff) Env
mkEnv (Args {development,monerodService}) = do
  mClient <- connectSession
  case mClient of
    Nothing -> throwException $ error "Couldn't connect to dbus!"
    Just client -> do
      signalQueue <- newQueue
      pure $ Env {development,client,signalQueue,monerodService}
