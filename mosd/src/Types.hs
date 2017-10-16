{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , DeriveGeneric
  , MultiParamTypeClasses
  #-}

module Types where

import Types.Env (Env)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Compose (Compose (..))
import Data.Singleton.Class (Extractable)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT (..), MonadReader)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl (..), MonadTransControl (..), defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Control.Monad.Trans.Resource.Internal (ResourceT (..))
import qualified Control.Monad.Trans.Control as M
import Control.Exception (Exception)
import GHC.Generics (Generic)


type AppM = ReaderT Env (ResourceT IO)

runAppM :: Env -> AppM a -> IO a
runAppM e x = runResourceT (runReaderT x e)

type MonadApp stM m =
  ( MonadReader Env m
  , MonadMask m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m stM
  , Extractable stM
  , M.MonadBaseControl IO m
  , MonadResource m
  )

instance MonadTransControl ResourceT Identity where
  liftWith f = ResourceT $ \r -> f $ \(ResourceT t) -> Identity <$> t r
  restoreT mx = lift $ runIdentity <$> mx

instance MonadBaseControl b m stM => MonadBaseControl b (ResourceT m) (Compose stM Identity) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM



data MonerodoException
  = DBusNameNotAcquired
  deriving (Show, Eq, Generic)

instance Exception MonerodoException
