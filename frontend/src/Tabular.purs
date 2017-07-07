module Tabular where

import Template (Showable (..))

import Data.Seek (Seek, toIndex, toArray, _inIndex, cursorIndex, current)
import Data.Lens.Getter ((^.))
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Lens (Lens', lens)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Array as Array

import Prelude
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM



type State a = Seek {label :: String, page :: a}

data Action a
  = ClickedTab Int
  | PageAction Int a


_PageAction :: forall a. Prism' (Action a) (Tuple Int a)
_PageAction = prism' (\(Tuple idx x) -> PageAction idx x) get
  where
    get (PageAction idx x) = Just (Tuple idx x)
    get _ = Nothing

_PageAction' :: forall a. Int -> Prism' (Showable (Action a)) (Showable a)
_PageAction' idx = prism' make get
  where
    make :: Showable a -> Showable (Action a)
    make ShowSidebar = ShowSidebar
    make (Showable x) = Showable (PageAction idx x)

    get :: Showable (Action a) -> Maybe (Showable a)
    get ShowSidebar = Just ShowSidebar
    get (Showable (PageAction idx' x))
      | idx == idx' = Just (Showable x)
      | otherwise   = Nothing
    get _ = Nothing


_inPageIndex :: forall a. Int -> Lens' (State a) a
_inPageIndex idx = _inIndex idx <<< pageLens
  where
    pageLens = lens _.page (_ {page = _})


spec :: forall pageState pageAction props
      . T.Spec _ pageState props (Showable pageAction)
     -> T.Spec _ (State pageState) props (Showable (Action pageAction))
spec pageSpec = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ (State pageState) props (Showable (Action pageAction))
    performAction (Showable a) props state = case a of
      ClickedTab idx -> void $ T.cotransform $ toIndex idx
      PageAction idx pageAction ->
        let pageSpec' :: T.Spec _ (State pageState) props (Showable (Action pageAction))
            pageSpec' = T.focus (_inPageIndex idx) (_PageAction' idx) pageSpec
        in  (pageSpec' ^. T._performAction) (Showable a) props state
    performAction _ _ _ = pure unit

    render :: T.Render (State pageState) props (Showable (Action pageAction))
    render dispatch props state children =
      [ R.div [RP.className "ui top attached tabular menu"] $ Array.mapWithIndex
          (\idx {label,page} ->
            R.a [ RP.className $ "item" <>
                    if idx == cursorIndex state
                      then " active"
                      else ""
                , RP.onClick \_ -> dispatch $ Showable $ ClickedTab idx
                ] [R.text label]
          ) (toArray state)
      , R.div [RP.className "ui bottom attached segment"] $
          let idx = cursorIndex state
              pageSpec' :: T.Spec _ (State pageState) props (Showable (Action pageAction))
              pageSpec' = T.focus (_inPageIndex idx)
                                  (_PageAction' idx) pageSpec
          in  (pageSpec' ^. T._render) dispatch props state children
      ]
