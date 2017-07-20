module Content.Node where

import Prelude
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Int as Int
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import Unsafe.Coerce (unsafeCoerce)


type State =
  { path :: String
  , on :: Boolean
  , logPath :: Maybe String
  , dataDirPath :: Maybe String
  , maxConcurrency :: Maybe Int
  , enforceDnsCheckpointing :: Boolean
  , limitRate ::
      { up :: Maybe Number
      , down :: Maybe Number
      }
  , rpcBindPort :: Int
  }

initialState :: State
initialState =
  { path : "/usr/bin/monerod"
  , on : false
  , logPath : Nothing
  , dataDirPath : Nothing
  , maxConcurrency : Nothing
  , enforceDnsCheckpointing : false
  , limitRate :
      { up : Nothing
      , down : Nothing
      }
  , rpcBindPort : 18081
  }

data Action
  = ChangePath String
  | ChangeOn Boolean
  | ChangeLogPath String
  | ChangeDataDirPath String
  | ChangeMaxConcurrency (Maybe Int)
  | ChangeEnforceDnsCheckpointing Boolean
  | ChangeLimitRateUp (Maybe Number)
  | ChangeLimitRateDown (Maybe Number)
  | ChangeRpcBindPort Int


spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ State _ Action
    performAction action _ _ = case action of
      ChangePath p ->
        void $ T.cotransform $ _ { path = p }
      ChangeOn o ->
        void $ T.cotransform $ _ { on = o }
      ChangeLogPath l ->
        void $ T.cotransform $ _ { logPath = if l == "" then Nothing else Just l }
      ChangeDataDirPath d ->
        void $ T.cotransform $ _ { dataDirPath = if d == "" then Nothing else Just d }
      ChangeMaxConcurrency mI ->
        void $ T.cotransform $ _ { maxConcurrency = mI }
      ChangeEnforceDnsCheckpointing d ->
        void $ T.cotransform $ _ { enforceDnsCheckpointing = d }
      ChangeLimitRateUp mU ->
        void $ T.cotransform $ \s -> s { limitRate = s.limitRate { up = mU } }
      ChangeLimitRateDown mD ->
        void $ T.cotransform $ \s -> s { limitRate = s.limitRate { down = mD } }
      ChangeRpcBindPort r ->
        void $ T.cotransform $ _ { rpcBindPort = r }

    render :: T.Render State _ Action
    render dispatch _ {path,on,logPath,dataDirPath,maxConcurrency,enforceDnsCheckpointing,limitRate,rpcBindPort} _ =
      [ R.div [RP.className "ui toggle checkbox"]
          [ R.input [ RP._type "checkbox"
                    , RP.onChange \e -> dispatch $ ChangeOn $ getChecked e
                    , RP.checked on
                    ] []
          , R.label [] [R.text "Turn on"]
          ]
      , R.div [RP.className $ "ui grid segment" <> if on then "" else " disabled"]
          [ oneRow
              [ R.h4 [RP.className "ui header"] [R.text "Executable Location"]
              , R.div [RP.className "ui fluid input"]
                  [ R.input [ RP._type "text"
                            , RP.onChange \e -> dispatch $ ChangePath $ getValue e
                            , RP.value path
                            ] []
                  ]
              ]
          , oneRow
              [ R.h4 [RP.className "ui header"] [R.text "Log File Location"]
              , R.div [RP.className "ui fluid input"]
                  [ R.input [ RP._type "text"
                            , RP.onChange \e -> dispatch $ ChangeLogPath $ getValue e
                            , RP.value $ fromMaybe "" logPath
                            , RP.placeholder "/path/to/logfile.log"
                            ] []
                  ]
              ]
          , oneRow
              [ R.h4 [RP.className "ui header"] [R.text "Data Directory Location"]
              , R.div [RP.className "ui fluid labeled input"]
                  [ R.input [ RP._type "text"
                            , RP.onChange \e -> dispatch $ ChangeDataDirPath $ getValue e
                            , RP.value $ fromMaybe "" dataDirPath
                            , RP.placeholder "/path/to/monerod/"
                            ] []
                  ]
              ]
          , oneRow
              [ R.h4 [RP.className "ui header"] [R.text "Max Concurrency"]
              , R.div [RP.className "ui toggle checkbox"]
                  [ R.input [ RP._type "checkbox"
                            , RP.onChange \e -> if getChecked e
                                                then dispatch $ ChangeMaxConcurrency $ Just 1
                                                else dispatch $ ChangeMaxConcurrency Nothing
                            , RP.checked $ maxConcurrency /= Nothing
                            ] []
                  , R.label [] [R.text "Limit concurrent connections"]
                  ]
              , R.div [RP.className $ "ui fluid input" <> case maxConcurrency of
                          Nothing -> " disabled"
                          _       -> ""]
                  [ R.input [ RP._type "number"
                            , RP.step "1"
                            , RP.min "1"
                            , RP.value $ show $ fromMaybe 1 maxConcurrency
                            , RP.onChange \e -> dispatch $ ChangeMaxConcurrency $ Int.fromString $ getValue e
                            ] []
                  ]
              ]
          , oneRow
              [ R.h4 [RP.className "ui header"] [R.text "Enforce DNS Checkpointing"]
              , R.div [RP.className "ui toggle checkbox"]
                  [ R.input [ RP._type "checkbox"
                            , RP.onChange \e -> dispatch $ ChangeEnforceDnsCheckpointing $ getChecked e
                            , RP.checked enforceDnsCheckpointing
                            ] []
                  , R.label [] [R.text ""]
                  ]
              ]
          , R.div [RP.className "row"]
              [ R.div [RP.className "sixteen wide column"]
                  [ R.h4 [RP.className "ui header"] [R.text "Limit Rate"]
                  ]
              , R.div [RP.className "eight wide column"]
                  [ R.h5 [RP.className "ui header"] [R.text "Up"]
                  ]
              ]
          ]
      ]


getValue e = (unsafeCoerce e).target.value

getChecked e = (unsafeCoerce e).target.checked


oneRow x =
  R.div [RP.className "one column row"] [R.div [RP.className "column"] x]
