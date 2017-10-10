module Types.Env where

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import DBus (DBUS, Client, connectSession)


newtype Env = Env
  { client :: Client
  }

mkEnv :: forall eff. Eff (dbus :: DBUS, exception :: EXCEPTION | eff) Env
mkEnv = do
  mClient <- connectSession
  case mClient of
    Nothing -> throwException $ error "Couldn't connect to dbus!"
    Just client ->
      pure $ Env {client}
