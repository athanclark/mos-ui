module Spec.Page.MoneroD where

import System.SystemD.Status (LoadedState (..))
import Types.DBus (SignalOutput (..))
import Monerodo.MoneroD (MoneroDLog (..))

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Int (toNumber)
import Thermite as T
import React.DOM as R
import MaterialUI.Typography as Typography
import MaterialUI.Typography (typography)
import MaterialUI.Divider (divider)
import MaterialUI.LinearProgress (linearProgress)
import MaterialUI.LinearProgress as LinearProgress


type State =
  { syncHeight :: Maybe (Tuple Int Int)
  , loadedState :: LoadedState
  }

initialState :: LoadedState -> State
initialState loadedState =
  { syncHeight: Nothing
  , loadedState
  }

data Action
  = GotSignal SignalOutput

spec :: T.Spec _ State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotSignal (MoneroDLogSignal log) -> case log of
        SyncProgress {amount,total} -> void $ T.cotransform _ { syncHeight = Just (Tuple amount total) }
        SyncNewTopBlock {current,top} -> void $ T.cotransform _ { syncHeight = Just (Tuple current top) }
        _ -> pure unit


    render :: T.Render State _ Action
    render dispatch props state children =
      [ typography
        { "type": Typography.headline
        } [R.text "monerod"]
      , typography
        { "type": Typography.display1
        } [R.text "Status"]
      , divider {}
      , linearProgress
          { mode: LinearProgress.determinate
          , value: case state.syncHeight of
                      Nothing -> 0.0
                      Just (Tuple n d) -> toNumber (n / d)
          }
      , typography
        { "type": Typography.body1
        } [R.em [] [ R.text $ case state.syncHeight of
                        Nothing -> case state.loadedState of
                          Loaded -> "running but not syncing"
                          NotFound -> "not found"
                        Just (Tuple current all) -> show current <> " / " <> show all
                   ]]
      , typography
        { "type": Typography.display1
        } [R.text "Config"]
      , divider {}
      , typography
        { "type": Typography.body1
        } [R.em [] [R.text "TODO"]]
      ]
