module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Pux (start, noEffects, FoldP)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.Markup (Markup, text)
import Signal (Signal)
import Signal.Channel (CHANNEL)


data State = State

initialState :: State
initialState = State

data Event = Event

foldp :: forall eff. FoldP State Event eff
foldp Event State = noEffects State

view :: forall handler. State -> Markup handler
view State = text ":D"

inputs :: Array (Signal Event)
inputs = []


main :: forall e
      . Eff ( console   :: CONSOLE
            , channel   :: CHANNEL
            , exception :: EXCEPTION
            | e) Unit
main = do
  log "Hello sailor!"

  app <- start
    {initialState, foldp, view, inputs}
  renderToDOM "#app" app.markup app.input
