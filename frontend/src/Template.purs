module Template where

import Prelude

import Data.Lens.Lens (Lens', lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Prism (Prism', prism')
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.Eff.Console (CONSOLE)
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM



type State content sidebar =
  { content :: content
  , sidebar :: sidebar
  }

initialState :: forall content sidebar. content -> sidebar -> State content sidebar
initialState content sidebar = {content, sidebar}

data Action content sidebar
  = ContentAction content
  | SidebarAction sidebar
  | ShowSidebarAction
  | HideSidebarAction


foreign import data SIDEBAR :: Effect

foreign import sidebarShow :: forall eff. EffFn1 (sidebar :: SIDEBAR | eff) String Unit
foreign import sidebarHide :: forall eff. EffFn1 (sidebar :: SIDEBAR | eff) String Unit


data Showable a = ShowSidebar | Showable a
data Hideable a = HideSidebar | Hideable a


_content :: forall content sidebar. Lens' (State content sidebar) content
_content = lens _.content (_ {content = _})

_sidebar :: forall content sidebar. Lens' (State content sidebar) sidebar
_sidebar = lens _.sidebar (_ {sidebar = _})

_Show :: forall content sidebar. Prism' (Action content sidebar) (Showable content)
_Show = prism' make get
  where
    make ShowSidebar = ShowSidebarAction
    make (Showable content) = ContentAction content

    get ShowSidebarAction = Just ShowSidebar
    get (ContentAction content) = Just (Showable content)
    get _ = Nothing

_Hide :: forall content sidebar. Prism' (Action content sidebar) (Hideable sidebar)
_Hide = prism' make get
  where
    make HideSidebar = HideSidebarAction
    make (Hideable sidebar) = SidebarAction sidebar

    get HideSidebarAction = Just HideSidebar
    get (SidebarAction sidebar) = Just (Hideable sidebar)
    get _ = Nothing



spec :: forall eff props contentState sidebarState contentAction sidebarAction
      . T.Spec ( console :: CONSOLE
               , sidebar :: SIDEBAR
               | eff) contentState props (Showable contentAction)
     -> T.Spec ( console :: CONSOLE
               , sidebar :: SIDEBAR
               | eff) sidebarState props (Hideable sidebarAction)
     -> T.Spec ( console :: CONSOLE
               , sidebar :: SIDEBAR
               | eff) (State contentState sidebarState) props (Action contentAction sidebarAction)
spec contentSpec sidebarSpec = T.simpleSpec performAction render
  where
    performAction :: T.PerformAction _ (State contentState sidebarState) props (Action contentAction sidebarAction)
    performAction ShowSidebarAction props state = do
      liftEff $ runEffFn1 sidebarShow "#tasks"
      ((contentSpec' <> sidebarSpec') ^. T._performAction) ShowSidebarAction props state
    performAction HideSidebarAction props state = do
      liftEff $ runEffFn1 sidebarHide "#tasks"
      ((contentSpec' <> sidebarSpec') ^. T._performAction) HideSidebarAction props state
    performAction a props state =
      ((contentSpec' <> sidebarSpec') ^. T._performAction) a props state

    render :: T.Render (State contentState sidebarState) props (Action contentAction sidebarAction)
    render dispatch props state children =
      [ R.div [RP.className "pusher"] $
          (contentSpec' ^. T._render) dispatch props state children
      , R.div [ RP.className "ui right sidebar inverted vertical menu"
              , RP._id "tasks"
              ] $
          (sidebarSpec' ^. T._render) dispatch props state children
      ]

    contentSpec' :: T.Spec _ (State contentState sidebarState) props (Action contentAction sidebarAction)
    contentSpec' = T.focus _content _Show contentSpec
    sidebarSpec' :: T.Spec _ (State contentState sidebarState) props (Action contentAction sidebarAction)
    sidebarSpec' = T.focus _sidebar _Hide sidebarSpec
