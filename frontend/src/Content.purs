module Content where

import Template (Showable (ShowSidebar))

import Prelude
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM


data State = State

initialState = State

data Action = Action

spec :: T.Spec _ State _ (Showable Action)
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit
    render :: T.Render State _ (Showable Action)
    render dispatch _ _ _ =
      [ R.text ":D"
      , R.button [ RP.onClick \_ -> dispatch ShowSidebar ] [ R.text "Show Sidebar" ]
      ]
