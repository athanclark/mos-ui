{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , NamedFieldPuns
  #-}

module Daemon.Methods where

import Types (MonadApp)
import Types.Env (Env (..))
import Types.DBus (Service (..), ControlInput (..), ControlOutput (..), SignalOutput)
import System.SystemD.Status (getServiceStatus)

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (ask)
import Control.Logging (log')
import Control.Concurrent (threadDelay)


control :: MonadApp stM m => ControlInput -> m ControlOutput
control (GetServiceState mService) = do
  log' $ "Getting service status for: " <> fromMaybe "all" (T.pack . show <$> mService)
  let allServices = case mService of
                      Nothing -> [ServiceMoneroD]
                      Just s -> [s]
  xs <- forM allServices $ \service -> do
    serviceName <- case service of
      ServiceMoneroD -> do
        Env{envMoneroDService} <- ask
        pure envMoneroDService
    eStatus <- liftIO $ getServiceStatus serviceName
    case eStatus of
      Left e -> log' (T.pack $ show e) >> throwM e
      Right x -> pure x
  log' $ "Service states: " <> T.pack (show xs)
  pure (GotServiceState xs)


signals :: MonadApp stM m => IORef (V.Vector SignalOutput) -> m (V.Vector SignalOutput)
signals pendingSignalsRef = liftIO $ do
  xs <- readIORef pendingSignalsRef
  writeIORef pendingSignalsRef V.empty
  pure xs
