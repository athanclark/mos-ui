module Main where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import DOM (DOM)


data State = State

initialState :: State
initialState = State

data Action = Action


render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.text "yooo"
  , R.button [RP.onClick \_ -> dispatch Action] [R.text "click"]
  ]

performAction :: T.PerformAction _ State _ Action
performAction Action _ _ = do
  lift $ liftEff $ log "clicked"
  void $ T.cotransform $ \State -> State


spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render


main :: forall eff
      . Eff ( console :: CONSOLE
            , dom :: DOM
            | eff
            ) Unit
main = do
  log "Hello sailor!"

  T.defaultMain spec initialState unit
