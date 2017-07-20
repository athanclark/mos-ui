module Tasks.Task where

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM


data State
  = TaskFinished
  | TaskRunning Number

initialState :: State
initialState = TaskRunning 0.0

data Action
  = Close


spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ State _ Action
    performAction Close _ _ = pure unit

    render :: T.Render State _ Action
    render dispatch _ _ _ =
      [ R.div [RP.className "column"]
          [ R.p [] [R.text "Task"]
          ]
      ]
