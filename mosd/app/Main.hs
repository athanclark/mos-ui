module Main where

import Types (runAppM)
import Types.Env (mkEnv)
import Daemon (daemon)


main :: IO ()
main = do
  env <- mkEnv
  runAppM env daemon
