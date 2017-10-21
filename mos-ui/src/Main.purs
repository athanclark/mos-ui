module Main where

import Arguments (args)
import Client (monerodoClient)
import Client.Constants (signalOutput)
import Types (runAppM)
import Types.Env (Env (..), mkEnv, toEnvData)

import Prelude
import Data.Argonaut (encodeJson)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)

import Electron (ELECTRON, openWindow)
import Node.Process (PROCESS, cwd)
import DBus (DBUS, connectSession, getService)
import Queue (onQueue)
import Signal.Channel (CHANNEL)


main :: forall eff
      . Eff ( console   :: CONSOLE
            , electron  :: ELECTRON
            , process   :: PROCESS
            , exception :: EXCEPTION
            , dbus      :: DBUS
            , ref       :: REF
            , channel   :: CHANNEL
            | eff) Unit
main = do
  as <- args
  log "Starting Monerodo OS User Interface"
  env@Env{development,signalQueue} <- mkEnv as
  runAppM env monerodoClient
  wd <- cwd
  openWindow
    { file: wd <> "/index.html"
    , width: 1024
    , height: 768
    , devTools: development
    , whenLoaded: \{send} ->
        onQueue signalQueue \x ->
          send {channel: signalOutput, message: encodeJson x}
          -- FIXME rip out
    }
