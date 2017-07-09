module Content where

import Template (Showable (ShowSidebar))

import Prelude
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM


data State
  = NodeState
  | MiningState
  | PoolState
  | WalletState
  | ExplorerState
  | PaymentState

data Action = Action


spec :: T.Spec _ State _ (Showable Action)
spec = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ State _ (Showable Action)
    performAction _ _ _ = pure unit

    render :: T.Render State _ (Showable Action)
    render dispatch _ state _ = (case state of
      NodeState     -> [R.text "Node"]
      MiningState   -> [R.text "Mining"]
      PoolState     -> [R.text "Pool"]
      WalletState   -> [R.text "Wallet"]
      ExplorerState -> [R.text "Explorer"]
      PaymentState  -> [R.text "Payment"]
      ) <>
      [ R.button [ RP.className "ui button"
                 , RP.onClick \_ -> dispatch ShowSidebar
                 ] [R.text "Show Sidebar"]
      ]
