module Client where

import Client.Constants (monerodoBus, monerodoObject, monerodoControl, monerodoControlMethod, controlInput, controlOutput)
import Types (class MonadApp)
import Types.Env (Env (..))
import Types.DBus (ControlInput, ControlOutput)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (decodeJson, encodeJson)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import DBus (DBUS, getService, getInterface, call, nil, arg)
import Electron (ELECTRON)
import Electron.Main (registerAsyncHandler)



monerodoClient :: forall eff m
                . MonadApp ( exception :: EXCEPTION
                           , console :: CONSOLE
                           , dbus :: DBUS
                           , electron :: ELECTRON
                           | eff) m
               => m Unit
monerodoClient = do
  Env {client} <- ask
  liftEff $ registerAsyncHandler
    { channel: controlInput
    , handle: \{message,send} -> case decodeJson message of
        Left e -> warn $ "couldn't decode electron message: " <> show e
        Right (x :: ControlInput) -> runAff_ resolve $ do
          (r :: ControlOutput) <- call client monerodoBus monerodoObject monerodoControl monerodoControlMethod (nil `arg` x)
          liftEff $ send
            { channel: controlOutput
            , message: encodeJson r
            }
    }
  where
    resolve (Left e) = throwException $ error $ "got aff error: " <> show e
    resolve _        = pure unit
