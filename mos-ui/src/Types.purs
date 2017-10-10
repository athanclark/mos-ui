module Types where

import Types.Env (Env)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadReader)


type AppM eff = ReaderT Env (Eff eff)

runAppM :: forall eff a. Env -> AppM eff a -> Eff eff a
runAppM e x = runReaderT x e

class ( MonadEff eff m
      , MonadReader Env m
      ) <= MonadApp eff m

instance monadApp ::  ( MonadEff eff m
                      , MonadReader Env m
                      ) => MonadApp eff m
