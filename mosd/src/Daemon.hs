{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , FlexibleContexts
  , ScopedTypeVariables
  , RankNTypes
  #-}

module Daemon where

import Daemon.Constants (monerodoBus, monerodoObject, monerodoControl, monerodoControlMethod, monerodoSignalMethod)
import Daemon.Methods (control)
import Types (MonadApp, MonerodoException (..))
import Types.Env (Env (..))
import Types.DBus (SignalOutput (..))
import Monerodo.MoneroD (WithTimestamp (..), MoneroDLog (MoneroDOther), parseLogStream, parseConfigStream)

import Data.Singleton.Class (runSingleton)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Control.Monad (forever, forM_, unless)
import Control.Monad.Catch (bracket_, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Logging (withStderrLogging, warn', log')
import Control.Concurrent (threadDelay)
import System.File.Follow (follow)
import Path (toFilePath, parent)
import DBus (signalBody, toVariant, signal)
import DBus.Client (requestName, RequestNameReply (..), releaseName, ReleaseNameReply (..), nameDoNotQueue, export, autoMethod, emit)


daemon :: forall stM m. MonadApp stM m => m ()
daemon = withStderrLogging $ bracket_ getName giveName $ do
  Env{envClient,envMoneroDLogFile,envMoneroDConfigFile,envINotify} <- ask
  liftBaseWith $ \runInBase -> export envClient monerodoObject
    [ autoMethod monerodoControl monerodoControlMethod (\x -> runSingleton <$> runInBase (control x))
    ]
  log' $ "Exported Object " <> T.pack (show monerodoObject)
      <> " with Interface " <> T.pack (show monerodoControl)
      <> " and Method " <> T.pack (show monerodoControlMethod)
  tz <- liftIO getCurrentTimeZone

  follow envINotify envMoneroDLogFile $ \latestLog -> do
    logs <- parseLogStream tz latestLog
    forM_ logs $ \(WithTimestamp _ log) -> case log of
      MoneroDOther _ -> pure ()
      _ -> liftIO $ emit envClient $ (signal monerodoObject monerodoControl monerodoSignalMethod)
              { signalBody = [toVariant $ MoneroDLogSignal log]
              }
    log' "Emitting monerod signal"

  liftIO $ forever $ threadDelay 50000
  where
    getName = do
      Env{envClient} <- ask
      r <- liftIO $ requestName envClient monerodoBus [nameDoNotQueue]
      case r of
        NamePrimaryOwner -> pure ()
        NameInQueue -> throwM DBusNameNotAcquired
        NameExists -> throwM DBusNameNotAcquired
        NameAlreadyOwner -> pure ()
      log' $ "Obtained name " <> T.pack (show monerodoBus)
    giveName = do
      Env{envClient} <- ask
      r <- liftIO $ releaseName envClient monerodoBus
      case r of
        NameReleased -> pure ()
        NameNonExistent -> warn' "Monerodo bus name not existent somehow..."
        NameNotOwner -> throwM DBusNameNotAcquired
      log' $ "Released name " <> T.pack (show monerodoBus)
