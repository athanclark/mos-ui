module Main where

import Main.Options (args, getArgs, mkEnv)
import Types (runAppM)
import Types.Env (releaseEnv)
import Daemon (daemon)

import Data.Monoid ((<>))
import Control.Exception (bracket)
import Options.Applicative (execParser, fullDesc, progDesc, header, helper, info)
import System.Posix.User (getLoginName)


main :: IO ()
main = do
  username <- getLoginName
  bracket (mkEnv =<< getArgs =<< execParser (opts username)) releaseEnv $
    \env -> runAppM env daemon
  where
    opts u = info (helper <*> args u) $ fullDesc <> progDesc desc <> header head'
    desc = "Start the daemon"
    head' = "mosd - Monerodo OS Daemon"
