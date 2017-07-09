module Tasks where

import Template (Hideable (HideSidebar))

import Prelude
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM


data State = State

initialState = State

data Action = Action

spec :: T.Spec _ State _ (Hideable Action)
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit
    render :: T.Render State _ (Hideable Action)
    render dispatch _ _ _ =
      [ R.h2  [ RP.className "ui dividing header"
              , RP.style {color: "#fff", borderBottom: "1px solid white"}
              ] [R.text "Tasks"]
      , R.button [ RP.className "ui button"
                 , RP.onClick \_ -> dispatch HideSidebar
                 ] [R.text "Hide Sidebar"]
      ]
