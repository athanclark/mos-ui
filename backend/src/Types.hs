{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  #-}

module Types where

import Types.Env (Env)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control.Aligned (MonadBaseControl)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Catch (MonadThrow)


type MonadApp m =
  ( MonadBaseControl IO m (Compose Identity Identity)
  , MonadIO m
  , MonadReader Env m
  , MonadThrow m
  )

type AppM = ReaderT Env IO

runAppM :: Env -> AppM a -> IO a
runAppM e x = runReaderT x e
