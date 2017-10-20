{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Daemon.Methods where

import Types (MonadApp)
import Types.DBus (ControlInput (..), ControlOutput (..), SignalOutput)
import Data.Text (Text)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)
import Control.Logging (log')
import Control.Concurrent (threadDelay)


control :: MonadApp stM m => ControlInput -> m ControlOutput
control Foo = do
  log' "Control Called!"
  liftIO $ threadDelay 500000
  pure Bar


signals :: MonadApp stM m => IORef (V.Vector SignalOutput) -> m (V.Vector SignalOutput)
signals pendingSignalsRef = liftIO $ do
  xs <- readIORef pendingSignalsRef
  writeIORef pendingSignalsRef V.empty
  pure xs
