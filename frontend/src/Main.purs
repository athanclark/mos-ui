module Main where

import Template (SIDEBAR, _Showable)
import Template as Template
import Tabular as Tabular
import Content as Content
import Content.Node as Node
import Content.Mining as Mining
import Content.Pool as Pool
import Content.Wallet as Wallet
import Content.Explorer as Explorer
import Content.Payment as Payment
import Tasks as Tasks

import Prelude
import Data.Lens.Lens (Lens', lens)
import Data.Lens.Prism (Prism', prism')
import Data.Maybe (Maybe (..))
import Data.Seek (Seek (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import DOM (DOM)


type State =
  { template :: Template.State (Tabular.State Content.State) Tasks.State
  }

initialState :: State
initialState =
  { template : Template.initialState tabs Tasks.initialState
  }
  where
    tabs = Seek
      { prefix : []
      , cursor :
            { label : "Node"
            , page  : Content.NodeState Node.initialState
            }
      , suffix :
          [ { label : "Mining"
            , page  : Content.MiningState Mining.initialState
            }
          , { label : "Pool"
            , page  : Content.PoolState Pool.initialState
            }
          , { label : "Wallet"
            , page  : Content.WalletState Wallet.initialState
            }
          , { label : "Explorer"
            , page  : Content.ExplorerState Explorer.initialState
            }
          , { label : "Payment"
            , page  : Content.PaymentState Payment.initialState
            }
          ]
      }

data Action
  = TemplateAction (Template.Action (Tabular.Action Content.Action) Tasks.Action)

_template :: Lens' State (Template.State (Tabular.State Content.State) Tasks.State)
_template = lens _.template (_ {template = _})

_TemplateAction :: Prism' Action (Template.Action (Tabular.Action Content.Action) Tasks.Action)
_TemplateAction = prism' make get
  where
    make = TemplateAction
    get (TemplateAction x) = Just x


spec :: forall eff props
      . T.Spec ( console :: CONSOLE
               , sidebar :: SIDEBAR
               | eff) State props Action
spec = T.focus _template _TemplateAction
     $ Template.spec (Tabular.spec Content.spec) Tasks.spec


main :: forall eff
      . Eff ( console :: CONSOLE
            , sidebar :: SIDEBAR
            , dom :: DOM
            | eff
            ) Unit
main = do
  log "Hello sailor!"

  T.defaultMain spec initialState unit
