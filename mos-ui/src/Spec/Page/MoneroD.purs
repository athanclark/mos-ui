module Spec.Page.MoneroD where

import System.SystemD.Status (SystemDStatus (..), ActiveState (..), LoadedState (..))
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
  , systemdStatus :: Maybe (SystemDStatus)
  }

initialState :: State
initialState =
  { syncHeight: Nothing
  , systemdStatus: Nothing
  }

data Action
  = GotSignal SignalOutput
  | GotStatus SystemDStatus

spec :: T.Spec _ State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotSignal (MoneroDLogSignal log) -> case log of
        SyncProgress {amount,total} -> void $ T.cotransform _ { syncHeight = Just (Tuple amount total) }
        SyncNewTopBlock {current,top} -> void $ T.cotransform _ { syncHeight = Just (Tuple current top) }
        _ -> pure unit
      GotStatus s -> void $ T.cotransform _ { systemdStatus = Just s }


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
                      Just (Tuple n d) -> (toNumber n / toNumber d) * 100.0
          }
      , typography
        { "type": Typography.body1
        } [R.em [] [ R.text $ case state.systemdStatus of
                        Nothing -> "not connected to mosd"
                        Just (SystemDStatus {loadedState,activeState}) -> case activeState of
                          Failed -> "failed"
                          Inactive -> "inactive"
                          Active -> case state.syncHeight of
                            Nothing -> case loadedState of
                              Loaded -> "loaded"
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
