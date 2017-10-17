module Types.Env where

import Arguments (Args (..))
import Types.DBus (SignalOutput)

import Prelude
import Data.Maybe (Maybe (..))
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
  }

mkEnv :: forall eff
       . Args
      -> Eff ( dbus :: DBUS
             , channel :: CHANNEL
             , exception :: EXCEPTION
             , ref :: REF
             | eff) Env
mkEnv (Args {development}) = do
  mClient <- connectSession
  case mClient of
    Nothing -> throwException $ error "Couldn't connect to dbus!"
    Just client -> do
      signalQueue <- newQueue
      pure $ Env {development, client, signalQueue}
