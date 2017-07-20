module Content.Pool where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM


data State = State

initialState :: State
initialState = State

data Action = Action


spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ State _ Action
    performAction _ _ _ = pure unit

    render :: T.Render State _ Action
    render dispatch _ _ _ =
      [ R.text "Pool"
      ]
