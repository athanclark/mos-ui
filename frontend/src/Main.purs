module Main where

import Template (SIDEBAR)
import Template as Template
import Content as Content
import Tasks as Tasks

import Prelude
import Data.Lens.Lens (Lens', lens)
import Data.Lens.Prism (Prism', prism')
import Data.Maybe (Maybe (..))
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
  { template :: Template.State Content.State Tasks.State
  }

initialState :: State
initialState =
  { template : Template.initialState Content.initialState Tasks.initialState
  }

data Action
  = TemplateAction (Template.Action Content.Action Tasks.Action)

_template :: Lens' State (Template.State Content.State Tasks.State)
_template = lens _.template (_ {template = _})

_TemplateAction :: Prism' Action (Template.Action Content.Action Tasks.Action)
_TemplateAction = prism' make get
  where
    make = TemplateAction
    get (TemplateAction x) = Just x


spec :: forall eff props
      . T.Spec ( console :: CONSOLE
               , sidebar :: SIDEBAR
               | eff) State props Action
spec = T.focus _template _TemplateAction $ Template.spec Content.spec Tasks.spec -- T.simpleSpec performAction render
  -- where
  --   performAction :: T.PerformAction _ State props Action
  --   performAction Action _ _ = do
  --     liftEff $ log "clicked"
  --     void $ T.cotransform $ \State -> State

  --   render :: T.Render State props Action
  --   render dispatch props state children =
  --     [ R.div [RP.className "pusher"]
  --         [ R.text "yooo"
  --         , R.button [RP.onClick \_ -> dispatch Action] [R.text "click"]
  --         ]
  --     , R.div [ RP.className "ui sidebar inverted vertical menu"
  --             , RP._id "tasks"
  --             ] []
  --     ]


main :: forall eff
      . Eff ( console :: CONSOLE
            , sidebar :: SIDEBAR
            , dom :: DOM
            | eff
            ) Unit
main = do
  log "Hello sailor!"

  T.defaultMain spec initialState unit
