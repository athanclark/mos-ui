module Client where

import Client.Constants (monerodoBus, monerodoObject, monerodoControl, monerodoControlMethod, monerodoSignalMethod, controlInput, controlOutput, signalOutput)
import Types (class MonadApp)
import Types.Env (Env (..))
import Types.DBus (ControlInput, ControlOutput, AllInputs (..), SignalOutput)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (Json, decodeJson, encodeJson, jsonParser, fromArray)
import Data.Traversable (traverse)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import DBus (DBUS, getService, getInterface, call, nil, arg, on)
import Electron (ELECTRON)
import Electron.Main (registerAsyncHandler)
import Node.Process (PROCESS)
import Queue (putQueue)
import Signal.Channel (CHANNEL)
import Unsafe.Coerce (unsafeCoerce)



monerodoClient :: forall eff m
                . MonadApp ( exception :: EXCEPTION
                           , console   :: CONSOLE
                           , dbus      :: DBUS
                           , electron  :: ELECTRON
                           , process   :: PROCESS
                           , ref       :: REF
                           , channel   :: CHANNEL
                           | eff) m
               => m Unit
monerodoClient = do
  Env {client,signalQueue} <- ask
  liftEff $ registerAsyncHandler
    { channel: controlInput
    , handle: \{message,send} -> case decodeJson message of
        Left e -> warn $ "couldn't decode electron message: " <> show e
        Right (x :: ControlInput) ->
          let resolve (Left e) = throwException e
              resolve _        = pure unit
          in  runAff_ resolve $ do
                (r :: ControlOutput) <- call client monerodoBus monerodoObject monerodoControl monerodoControlMethod (nil `arg` x)
                liftEff $ send
                  { channel: controlOutput
                  , message: encodeJson r
                  }
    }
  liftEff $ registerAsyncHandler
    { channel: signalOutput
    , handle: \{message,send} -> case decodeJson message of
        Left e -> warn $ "couldn't decode electron message: " <> show e
        Right (x :: Unit) ->
          let resolve (Left e) = throwException e
              resolve _        = pure unit
          in  runAff_ resolve $ do
                (r :: Array String) <- call client monerodoBus monerodoObject monerodoControl monerodoSignalMethod nil
                case traverse jsonParser r of
                  Left e -> liftEff $ warn e
                  Right rs ->
                    liftEff $ send
                      { channel: signalOutput
                      , message: fromArray rs
                      }
    }
  -- liftEff $ do
  --   case getService client monerodoBus of
  --     Nothing -> throwException $ error "Couldn't find monerodo bus"
  --     Just s -> do
  --       let resolve (Left e)  = throwException e
  --           resolve (Right i) = on i monerodoSignalMethod (putQueue signalQueue)
  --       runAff_ resolve (getInterface s monerodoObject monerodoControl)
