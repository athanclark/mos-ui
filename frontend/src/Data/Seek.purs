module Data.Seek where

import Prelude
import Data.Array as Array
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))

import Data.Generic (class Generic, gShow)


newtype Seek a = Seek
  { prefix :: Array a
  , cursor :: a
  , suffix :: Array a
  }

derive instance genericSeek :: Generic a => Generic (Seek a)

instance showSeek :: (Generic a, Show a) => Show (Seek a) where
  show = gShow



next :: forall a. Seek a -> Seek a
next x@(Seek {prefix, cursor, suffix}) = case Array.uncons suffix of
  Just {head,tail} ->
    Seek
      { prefix : prefix <> [cursor]
      , cursor : head
      , suffix : tail
      }
  _ -> x


prev :: forall a. Seek a -> Seek a
prev x@(Seek {prefix, cursor, suffix}) = case Array.unsnoc prefix of
  Just {init,last} ->
    Seek
      { prefix : init
      , cursor : last
      , suffix : [cursor] <> suffix
      }
  _ -> x


toIndex :: forall a. Int -> Seek a -> Seek a
toIndex idx x@(Seek {prefix, cursor, suffix})
  | idx == Array.length prefix = x
  | idx <= 0 = case Array.uncons prefix of
      Just {head,tail} ->
        Seek
          { prefix : []
          , cursor : head
          , suffix : tail <> [cursor] <> suffix
          }
      _ -> x
  | idx < Array.length prefix =
      let prefix' = Array.take idx prefix
      in  case Array.uncons (Array.drop idx prefix) of
            Just {head,tail} ->
              Seek
                { prefix : prefix'
                , cursor : head
                , suffix : tail <> [cursor] <> suffix
                }
            _ -> x
  | otherwise =
      let sLen = Array.length prefix + 1 + Array.length suffix
      in  if idx >= sLen
            then case Array.unsnoc suffix of
              Just {init,last} ->
                Seek
                  { prefix : prefix <> [cursor] <> init
                  , cursor : last
                  , suffix : []
                  }
              _ -> x
            else let idx' = idx - Array.length prefix
                     suffix' = Array.drop idx' suffix
                 in  case Array.unsnoc (Array.take idx' suffix) of
                       Just {init,last} ->
                          Seek
                            { prefix : prefix <> [cursor] <> init
                            , cursor : last
                            , suffix : suffix'
                            }
                       _ -> x
