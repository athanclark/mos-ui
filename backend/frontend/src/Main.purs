module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T
import React.DOM as R
import DOM (DOM)


spec :: T.Spec _ Unit _ Unit
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit
    render :: T.Render Unit _ Unit
    render _ _ _ _ = [R.text "!"]


main :: forall eff
      . Eff ( console :: CONSOLE
            , dom     :: DOM
            | eff) Unit
main = do
  T.defaultMain spec unit unit
