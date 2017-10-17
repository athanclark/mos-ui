module Spec where

import Spec.Page.MoneroD as MoneroD
import Spec.Page.XmrStak as XmrStak
import Types.DBus (ControlInput (..), ControlOutput (..), AllInputs (..))
import Client.Constants (controlInput, controlOutput, signalOutput)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut (encodeJson, decodeJson)
import Data.Traversable (traverse_)
import Data.Time.Duration (Milliseconds (..))
import Data.Lens (Lens', lens, Prism', prism', (^.))
import Data.Lens.Record (prop)
import Data.Symbol (SProxy (..))
import Control.Monad.Aff (runAff_, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, warn, log)
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM (render)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import Electron (ELECTRON)
import Electron.Renderer (registerAsyncHandler, send)
import Queue (newQueue, putQueue, onQueue)
import Signal.Channel (CHANNEL)
import MaterialUI.MuiThemeProvider (muiThemeProvider, createMuiTheme)
import MaterialUI.AppBar (appBar)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Types (createStyles)
import MaterialUI.Paper (paper)
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT, injectTapEvent)



data Page
  = MoneroD MoneroD.State
  | XmrStak XmrStak.State

_MoneroD :: Prism' Page MoneroD.State
_MoneroD = prism' MoneroD $ case _ of
  MoneroD x -> Just x
  _         -> Nothing

_XmrStak :: Prism' Page XmrStak.State
_XmrStak = prism' XmrStak $ case _ of
  XmrStak x -> Just x
  _         -> Nothing


type State =
  { currentPage :: Page
  }

_currentPage :: Lens' State Page
_currentPage = lens _.currentPage (_ {currentPage = _})

initialState :: State
initialState =
  { currentPage: MoneroD MoneroD.initialState
  }

data NavAction
  = ClickedMoneroD
  | ClickedXmrStak

data PageAction
  = MoneroDAction MoneroD.Action
  | XmrStakAction XmrStak.Action

_MoneroDAction :: Prism' PageAction MoneroD.Action
_MoneroDAction = prism' MoneroDAction $ case _ of
  MoneroDAction x -> Just x
  _               -> Nothing

_XmrStakAction :: Prism' PageAction MoneroD.Action
_XmrStakAction = prism' XmrStakAction $ case _ of
  XmrStakAction x -> Just x
  _               -> Nothing

data Action
  = NavAction NavAction
  | IpcAction AllInputs
  | PageAction PageAction

_PageAction :: Prism' Action PageAction
_PageAction = prism' PageAction $ case _ of
  PageAction p -> Just p
  _            -> Nothing


spec :: T.Spec _ State _ Action
spec = T.simpleSpec ( performAction
                   <> (moneroD ^. T._performAction)
                   <> (xmrStak ^. T._performAction)
                    ) render
  where
    moneroD = T.focus _currentPage (_PageAction <<< _MoneroDAction) $ T.split _MoneroD MoneroD.spec
    xmrStak = T.focus _currentPage (_PageAction <<< _XmrStakAction) $ T.split _XmrStak XmrStak.spec

    performAction action props state = case action of
      IpcAction i ->
        liftEff $ log $ "got IPC: " <> show i
      NavAction navAction -> case navAction of
        ClickedMoneroD -> void $ T.cotransform $ _ {currentPage = MoneroD MoneroD.initialState}
        ClickedXmrStak -> void $ T.cotransform $ _ {currentPage = XmrStak XmrStak.initialState}
      _ -> pure unit

    render :: T.Render State _ Action
    render dispatch props state@{currentPage} children =
      [ appBar {}
        [ toolbar {}
          [ typography
            { "type": Typography.title
            , color: Typography.inheritColor
            , style: createStyles {flex: 1}
            } [R.text "Monerodo"]
          , button
            { color: Button.contrast
            , onTouchTap: mkEffFn1 \_ -> dispatch (NavAction ClickedMoneroD)
            , disabled: case currentPage of
                MoneroD _ -> true
                _         -> false
            } [R.text "monerod"]
          , button
            { color: Button.contrast
            , onTouchTap: mkEffFn1 \_ -> dispatch (NavAction ClickedXmrStak)
            , disabled: case currentPage of
                XmrStak _ -> true
                _         -> false
            } [R.text "xmr-stak"]
          ]
        ]
      , paper
        { style: createStyles
          { marginTop: "5em"
          , marginLeft: "auto"
          , marginRight: "auto"
          , padding: "1em"
          }
        } $ case currentPage of
          MoneroD _ -> (moneroD ^. T._render) dispatch props state children
          XmrStak _ -> (xmrStak ^. T._render) dispatch props state children
      ]
      where
        template content =
          [ muiThemeProvider {theme: createMuiTheme unit}
              (R.div [] content)
          ]



main :: forall eff
      . Eff ( dom :: DOM
            , ref :: REF
            , injectTapEvent :: INJECT_TAP_EVENT
            , channel :: CHANNEL
            , electron :: ELECTRON
            , console :: CONSOLE
            | eff) Unit
main = do
  injectTapEvent

  window' <- window
  controlQueue <- newQueue
  signalQueue <- newQueue

  registerAsyncHandler
    { channel: controlOutput
    , handle: \{message} -> do
        case decodeJson message of
          Left e -> warn $ "Couldn't decode electron ipc message: " <> show e
          Right x -> putQueue controlQueue (IpcAction (ControlOutput x))
    }

  registerAsyncHandler
    { channel: signalOutput
    , handle: \{message} -> do
        case decodeJson message of
          Left e -> warn $ "Couldn't decode electron ipc signal: " <> show e
          Right x -> putQueue signalQueue (IpcAction (SignalOutput x))
    }

  let props = unit
      {spec: reactSpec, dispatcher} = T.createReactSpec
        spec initialState
      reactSpec' = reactSpec
        { componentDidMount = \this -> do
            onQueue signalQueue (dispatcher this)
            onQueue controlQueue (dispatcher this)
            reactSpec.componentDidMount this
        }
      component = R.createClass reactSpec'

  traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document window'
