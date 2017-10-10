{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Daemon.Methods where

import Types (MonadApp)
import Types.DBus (ControlInput (..), ControlOutput (..))
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Logging (log')
import Control.Concurrent (threadDelay)


control :: MonadApp m => ControlInput -> m ControlOutput
control Foo = do
  log' "Control Called!"
  liftIO $ threadDelay 500000
  pure Bar
