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
import MaterialUI.Chip (chip)
import MaterialUI.Avatar (avatar)
import MaterialUI.Icons.Brightness3 (brightness3Icon)
import MaterialUI.Icons.ErrorOutline (errorOutlineIcon)
import MaterialUI.Icons.CheckCircle (checkCircleIcon)


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
      ] <> status <>
      [ typography
        { "type": Typography.display1
        } [R.text "Config"]
      , divider {}
      , typography
        { "type": Typography.body1
        } [R.em [] [R.text "TODO"]]
      ]
      where
        status = case state.systemdStatus of
          Nothing -> [ chip { label: R.text "not connected to mosd"
                            , avatar: avatar {} [errorOutlineIcon]
                            }
                     ]
          Just (SystemDStatus {loadedState,activeState}) -> case activeState of
            Failed -> [ chip { avatar: avatar {} [errorOutlineIcon]
                             , label: R.text "failed"
                             }
                      ]
            Inactive -> [ chip { avatar: avatar {} [brightness3Icon]
                               , label: R.text "inactive"
                               }
                        ]
            Active -> case state.syncHeight of
              Nothing -> [ linearProgress {mode: LinearProgress.determinate, value: 0.0}
                         , chip { avatar: avatar {} [checkCircleIcon]
                                , label: R.text "running"
                                }
                         ]
              Just (Tuple current all) ->
                [ linearProgress
                    { mode: LinearProgress.determinate
                    , value: (toNumber current / toNumber all) * 100.0
                    }
                , chip { avatar: avatar {} [checkCircleIcon]
                       , label: R.text $ "Sync Height: " <> show current <> " / " <> show all
                       }
                ]
        mkBody x = typography {"type": Typography.body1} x
