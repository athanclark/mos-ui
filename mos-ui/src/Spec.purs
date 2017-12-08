module Spec where

import Spec.Page.MoneroD as MoneroD
import Spec.Page.XmrStak as XmrStak
import Types.Env (EnvData, getEnvData)
import Types.DBus (Service (..), ControlInput (..), ControlOutput (..), AllInputs (..), SignalOutput)
import Client.Constants (controlInput, controlOutput, signalOutput, envOutput)
import System.SystemD.Status (SystemDStatus (..), LoadedState (..), ActiveState (..))

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Argonaut (encodeJson, decodeJson)
import Data.Traversable (traverse_)
import Data.Time.Duration (Milliseconds (..))
import Data.Lens (Lens', lens, Prism', prism', (^.))
import Data.Lens.Record (prop)
import Data.Symbol (SProxy (..))
import Data.Array as Array
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (runAff_, delay, sequential, parallel)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
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

data PendingPage
  = MoneroDPage
  | XmrStakPage

type State =
  { currentPage :: Page
  , pendingPage :: Maybe PendingPage
  , env :: EnvData
  }

_currentPage :: Lens' State Page
_currentPage = lens _.currentPage (_ {currentPage = _})

initialState :: EnvData -> State
initialState env =
  { currentPage: MoneroD MoneroD.initialState
  , pendingPage: Just MoneroDPage
  , env
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

_XmrStakAction :: Prism' PageAction XmrStak.Action
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
  IpcAction (SignalOutput o) -> Just (MoneroDAction $ MoneroD.GotSignal o)
  _            -> Nothing


spec :: T.Spec _ State Unit Action
spec = T.simpleSpec ( performAction
                   <> (moneroD ^. T._performAction)
                   <> (xmrStak ^. T._performAction)
                    ) render
  where
    moneroD = T.focus _currentPage (_PageAction <<< _MoneroDAction) $ T.split _MoneroD MoneroD.spec
    xmrStak = T.focus _currentPage (_PageAction <<< _XmrStakAction) $ T.split _XmrStak XmrStak.spec

    performAction action props state = case action of
      NavAction navAction -> case navAction of
        ClickedMoneroD -> do
          liftEff $ send {channel: controlInput, message: encodeJson (GetServiceState $ Just ServiceMoneroD)}
          void $ T.cotransform $ _ {pendingPage = Just MoneroDPage, currentPage = MoneroD MoneroD.initialState}
        ClickedXmrStak -> void $ T.cotransform $ _ {currentPage = XmrStak XmrStak.initialState}
      IpcAction (ControlOutput a@(GotServiceState xs)) -> do
        case Array.head xs of
          Nothing -> do
            liftEff $ warn "Empty Service States!"
            void $ T.cotransform $ _ {pendingPage = Nothing}
          Just s@(SystemDStatus {name,loadedState,activeState})
            | name == (getEnvData state.env).monerodService -> do
                liftEff $ log $ "got monerod service! " <> show s
                case state.pendingPage of
                  Just MoneroDPage -> void $ T.cotransform _ {pendingPage = Nothing}
                  _ -> pure unit
                (moneroD ^. T._performAction) (PageAction $ MoneroDAction $ MoneroD.GotStatus s) props state
            | otherwise ->
                liftEff $ warn $ "Got service status for non-pending service: " <> show s
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
            , electron :: ELECTRON
            , console :: CONSOLE
            | eff) Unit
main = do
  injectTapEvent

  window' <- window
  controlQueue <- newQueue
  signalQueue <- newQueue
  envQueue <- newQueue

  registerAsyncHandler
    { channel: controlOutput
    , handle: \{message} -> do
        case decodeJson message of
          Left e -> warn $ "Couldn't decode electron ipc message: " <> show e
          Right x -> do
            putQueue controlQueue (IpcAction (ControlOutput x))
    }

  registerAsyncHandler
    { channel: signalOutput
    , handle: \{message} -> do
        case decodeJson message of
          Left e -> warn $ "Couldn't decode electron ipc signal: " <> show e <> ", " <> show message
          Right (xs :: Array SignalOutput) -> do
            traverse_ (\x -> putQueue signalQueue (IpcAction (SignalOutput x))) xs
    }

  registerAsyncHandler
    { channel: envOutput
    , handle: \{message} -> do
        case decodeJson message of
          Left e -> warn $ "Couldn't decode electron ipc env: " <> show e
          Right (x :: EnvData) -> do
            putQueue envQueue x
    }

  send {channel: envOutput, message: encodeJson unit}
  onQueue envQueue $ \env -> do
    r <- try $ do

      let props = unit
          {spec: reactSpec, dispatcher} = T.createReactSpec
            spec (initialState env)
          reactSpec' = reactSpec
            { componentDidMount = \this -> do
                unsafeCoerceEff $ onQueue signalQueue (unsafeCoerceEff <<< dispatcher this)
                unsafeCoerceEff $ onQueue controlQueue (unsafeCoerceEff <<< dispatcher this)
                reactSpec.componentDidMount this
            }
          component = R.createClass reactSpec'

      traverse_ (render (R.createFactory component props) <<< htmlElementToElement) =<< body =<< document window'

      let resolve (Left e) = warn (show e)
          resolve (Right (Tuple _ _)) = pure unit
      runAff_ resolve $ sequential $
        Tuple <$> parallel (forever $ do
                    liftEff $ do
                      send  { channel: signalOutput
                            , message: encodeJson unit
                            }
                    delay (Milliseconds 300.0)
                  )
              <*> parallel (forever $ do
                    liftEff $ do
                      send  { channel: controlInput
                            , message: encodeJson $ GetServiceState $ Just ServiceMoneroD
                            }
                    delay (Milliseconds 1000.0)
                  )
    case r of
      Left e -> warn $ show e
      Right _ -> pure unit
