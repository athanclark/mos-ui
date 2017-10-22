module System.SystemD.Status where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject, fail)
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Unsafe (unsafePerformEff)


data LoadedState
  = Loaded
  | NotFound

instance encodeJsonLoadedState :: EncodeJson LoadedState where
  encodeJson Loaded = encodeJson "loaded"
  encodeJson NotFound = encodeJson "not-found"

instance decodeJsonLoadedState :: DecodeJson LoadedState where
  decodeJson json = do
    s <- decodeJson json
    case s of
      _ | s == "loaded" -> pure Loaded
        | s == "not-found" -> pure NotFound
        | otherwise -> fail "Not a LoadedState"

data ActiveState
  = Active
  | Failed
  | Inactive

instance encodeJsonActiveState :: EncodeJson ActiveState where
  encodeJson Active = encodeJson "active"
  encodeJson Failed = encodeJson "failed"
  encodeJson Inactive = encodeJson "inactive"

instance decodeJsonActiveState :: DecodeJson ActiveState where
  decodeJson json = do
    s <- decodeJson json
    case s of
      _ | s == "active" -> pure Active
        | s == "failed" -> pure Failed
        | s == "inactive" -> pure Inactive
        | otherwise -> fail "Not a ActiveState"


newtype SystemDStatus = SystemDStatus
  { name :: String
  , description :: String
  , loadedState :: LoadedState
  , loadedStateExtra :: String
  , activeState :: ActiveState
  , activeStateSince :: DateTime
  }

instance showSystemDStatus :: Show SystemDStatus where
  show x = show (encodeJson x)

instance encodeJsonSystemDStatus :: EncodeJson SystemDStatus where
  encodeJson (SystemDStatus {name,description,loadedState,loadedStateExtra,activeState,activeStateSince})
    =  "name" := name
    ~> "description" := description
    ~> "loadedState" := loadedState
    ~> "loadedStateExtra" := loadedStateExtra
    ~> "activeState" := activeState
    ~> "activeStateSince" := unsafePerformEff (JSDate.toISOString $ JSDate.fromDateTime activeStateSince)
    ~> jsonEmptyObject

instance decodeJsonSystemDStatus :: DecodeJson SystemDStatus where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    description <- o .? "description"
    loadedState <- o .? "loadedState"
    loadedStateExtra <- o .? "loadedStateExtra"
    activeState <- o .? "activeState"
    activeStateSince' <- o .? "activeStateSince"
    case JSDate.toDateTime $ unsafePerformEff (JSDate.parse activeStateSince') of
      Nothing -> fail "couldn't parse iso8601"
      Just activeStateSince ->
        pure $ SystemDStatus
          {name,description,loadedState,loadedStateExtra,activeState,activeStateSince}
