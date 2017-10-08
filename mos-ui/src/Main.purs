module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Electron (ELECTRON, openWindow)
import Node.Process (PROCESS, cwd)


main :: forall eff
      . Eff ( console :: CONSOLE
            , electron :: ELECTRON
            , process :: PROCESS
            | eff) Unit
main = do
  log "Hello sailor!"
  wd <- cwd
  openWindow {file: wd <> "/index.html", width: 800, height: 600, devTools: false}
