{-# LANGUAGE
    NamedFieldPuns
  #-}

module Types.Env where

import DBus.Client (Client, connectSession)


data Env = Env
  { envClient :: Client
  }


mkEnv :: IO Env
mkEnv = do
  envClient <- connectSession
  pure Env{envClient}
