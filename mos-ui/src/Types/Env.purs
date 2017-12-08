module Types.Env where

import Arguments (Args (..))
import Types.DBus (SignalOutput)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import DBus (DBUS, Client, connectSession)
import Queue.Internal (Queue, newQueue, READ, WRITE)


newtype Env eff = Env
  { client :: Client
  , signalQueue :: Queue (read :: READ, write :: WRITE) eff SignalOutput
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

toEnvData :: forall eff. Env eff -> EnvData
toEnvData (Env {development,monerodService}) = EnvData {development,monerodService}



type Effects eff = (dbus :: DBUS, exception :: EXCEPTION, ref :: REF | eff)


mkEnv :: forall eff
       . Args
      -> Eff (Effects eff) (Env (Effects eff))
mkEnv (Args {development,monerodService}) = do
  mClient <- connectSession
  case mClient of
    Nothing -> throw "Couldn't connect to dbus!"
    Just client -> do
      signalQueue <- newQueue
      pure $ Env {development,client,signalQueue,monerodService}
