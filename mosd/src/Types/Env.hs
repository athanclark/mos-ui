{-# LANGUAGE
    NamedFieldPuns
  #-}

module Types.Env where

import DBus.Client (Client)
import Path (Path, Abs, File)
import System.INotify (INotify, killINotify)

data Env = Env
  { envClient :: Client
  , envMoneroDLogFile :: Path Abs File
  , envMoneroDConfigFile :: Path Abs File
  , envINotify :: INotify
  }


releaseEnv :: Env -> IO ()
releaseEnv Env{envINotify} = do
  killINotify envINotify
