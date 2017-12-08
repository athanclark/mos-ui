module Types where

import Types.Env (Env)

import Data.Functor.Singleton (class SingletonFunctor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Trans.Control (class MonadBaseControl)


type AppM eff = ReaderT (Env eff) (Eff eff)

runAppM :: forall eff a. Env eff -> AppM eff a -> Eff eff a
runAppM e x = runReaderT x e

class ( MonadEff eff m
      , MonadReader (Env eff) m
      , MonadBaseControl (Eff eff) m stM
      , SingletonFunctor stM
      ) <= MonadApp eff stM m

instance monadApp ::  ( MonadEff eff m
                      , MonadReader (Env eff) m
                      , MonadBaseControl (Eff eff) m stM
                      , SingletonFunctor stM
                      ) => MonadApp eff stM m
