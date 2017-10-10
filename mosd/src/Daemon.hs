{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , FlexibleContexts
  #-}

module Daemon where

import Daemon.Constants (monerodoBus, monerodoObject, monerodoControl, monerodoControlMethod)
import Daemon.Methods (control)
import Types (MonadApp, MonerodoException (..))
import Types.Env (Env (..))
import Data.Singleton.Class (runSingleton)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Control.Monad (forever)
import Control.Monad.Catch (bracket_, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Logging (withStderrLogging, warn', log')
import Control.Concurrent (threadDelay)
import DBus.Client (requestName, RequestNameReply (..), releaseName, ReleaseNameReply (..), nameDoNotQueue, export, autoMethod)


daemon :: MonadApp m => m ()
daemon = withStderrLogging $ bracket_ getName giveName $ do
  Env{envClient} <- ask
  liftBaseWith $ \runInBase -> export envClient monerodoObject
    [ autoMethod monerodoControl monerodoControlMethod (\x -> runSingleton <$> runInBase (control x))
    ]
  log' $ "Exported Object " <> T.pack (show monerodoObject)
      <> " with Interface " <> T.pack (show monerodoControl)
      <> " and Method " <> T.pack (show monerodoControlMethod)
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
