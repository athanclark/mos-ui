module Client where

import Client.Constants (monerodoBus, monerodoObject, monerodoControl, monerodoControlMethod, monerodoSignalMethod, controlInput, controlOutput, signalOutput, envOutput)
import Types (class MonadApp)
import Types.Env (Env (..), toEnvData)
import Types.DBus (ControlInput, ControlOutput, AllInputs (..), SignalOutput)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (Json, decodeJson, encodeJson, jsonParser, fromArray)
import Data.Traversable (traverse)
import Data.Functor.Singleton (liftBaseWith_)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Aff (runAff_, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throwException, try)
import DBus (DBUS, getService, getInterface, call, nil, arg, on)
import Electron (ELECTRON)
import Electron.Main (registerAsyncHandler)
import Node.Process (PROCESS)
import Queue (putQueue)
import Signal.Channel (CHANNEL)
import Unsafe.Coerce (unsafeCoerce)



monerodoClient :: forall eff stM m
                . MonadApp ( exception :: EXCEPTION
                           , console   :: CONSOLE
                           , dbus      :: DBUS
                           , electron  :: ELECTRON
                           , process   :: PROCESS
                           , ref       :: REF
                           , channel   :: CHANNEL
                           | eff) stM m
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
                (eR :: Either Error ControlOutput) <- attempt $ call client monerodoBus monerodoObject monerodoControl monerodoControlMethod (nil `arg` x)
                case eR of
                  Left e -> liftEff $ warn $ show e
                  Right r ->
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
                (eR :: Either Error (Array String)) <- attempt $ call client monerodoBus monerodoObject monerodoControl monerodoSignalMethod nil
                case eR of
                  Left e -> liftEff $ warn $ show e
                  Right r ->
                    case traverse jsonParser r of
                      Left e -> liftEff $ warn e
                      Right rs ->
                        liftEff $ do
                          r <- unsafeCoerceEff $ try $ send
                            { channel: signalOutput
                            , message: fromArray rs
                            }
                          case r of
                            Left e -> warn $ show e
                            Right _ -> pure unit
    }
  liftBaseWith_ \runInBase -> registerAsyncHandler
    { channel: envOutput
    , handle: \{message,send} -> case decodeJson message of
        Left e -> warn $ "couldn't decode electron message: " <> show e
        Right (x :: Unit) -> do
          env <- runInBase ask
          liftEff $ send
            { channel: envOutput
            , message: encodeJson (toEnvData env)
            }
    }
  -- liftEff $ do
  --   case getService client monerodoBus of
  --     Nothing -> throwException $ error "Couldn't find monerodo bus"
  --     Just s -> do
  --       let resolve (Left e)  = throwException e
  --           resolve (Right i) = on i monerodoSignalMethod (putQueue signalQueue)
  --       runAff_ resolve (getInterface s monerodoObject monerodoControl)
