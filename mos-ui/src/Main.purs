module Main where

import Client (monerodoClient)
import Types (runAppM)
import Types.Env (mkEnv)

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)

import Electron (ELECTRON, openWindow)
import Node.Process (PROCESS, cwd)
import DBus (DBUS, connectSession, getService)


main :: forall eff
      . Eff ( console :: CONSOLE
            , electron :: ELECTRON
            , process :: PROCESS
            , exception :: EXCEPTION
            , dbus :: DBUS
            | eff) Unit
main = do
  log "Hello sailor!"
  env <- mkEnv
  runAppM env monerodoClient
  wd <- cwd
  openWindow
    { file: wd <> "/index.html"
    , width: 1024
    , height: 768
    , devTools: false
    , whenLoaded: \{send} ->
        pure unit
    }
