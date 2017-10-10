module Spec.Page.XmrStak where

import Prelude
import Thermite as T
import React.DOM as R
import MaterialUI.Typography as Typography
import MaterialUI.Typography (typography)
import MaterialUI.Divider (divider)


type State = Unit

initialState = unit

type Action = Unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction _ _ _ = pure unit
    render :: T.Render State _ Action
    render dispatch state props children =
      [ typography
        { "type": Typography.headline
        } [R.text "xmr-stak"]
      , typography
        { "type": Typography.display1
        } [R.text "Status"]
      , divider {}
      , typography
        { "type": Typography.body1
        } [R.em [] [R.text "TODO"]]
      , typography
        { "type": Typography.display1
        } [R.text "Config"]
      , divider {}
      , typography
        { "type": Typography.body1
        } [R.em [] [R.text "TODO"]]
      ]
