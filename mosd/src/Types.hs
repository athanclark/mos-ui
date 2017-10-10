{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , DeriveGeneric
  #-}

module Types where

import Types.Env (Env)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Control.Monad.Reader (ReaderT (..), MonadReader)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl)
import qualified Control.Monad.Trans.Control as M
import Data.Singleton.Class (Extractable)
import Control.Exception (Exception)
import GHC.Generics (Generic)


type AppM = ReaderT Env IO

runAppM :: Env -> AppM a -> IO a
runAppM e x = runReaderT x e

type MonadApp m =
  ( MonadReader Env m
  , MonadMask m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m (Compose Identity Identity)
  , M.MonadBaseControl IO m
  )



data MonerodoException
  = DBusNameNotAcquired
  deriving (Show, Eq, Generic)

instance Exception MonerodoException
