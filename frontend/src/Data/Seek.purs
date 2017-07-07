module Data.Seek where

import Prelude
import Data.Array as Array
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..), fromMaybe)

import Data.Generic (class Generic, gShow)
import Data.Lens.Lens (Lens', lens)


newtype Seek a = Seek
  { prefix :: Array a
  , cursor :: a
  , suffix :: Array a
  }

derive instance genericSeek :: Generic a => Generic (Seek a)

instance showSeek :: (Generic a, Show a) => Show (Seek a) where
  show = gShow


mapIndex :: forall a. Int -> (a -> a) -> Seek a -> Seek a
mapIndex idx f (Seek o@{prefix, cursor, suffix})
  | idx == Array.length prefix = Seek (o { cursor = f cursor })
  | idx < Array.length prefix = Seek (o {prefix = Array.mapWithIndex (\i x -> if i == idx then f x else x) prefix})
  | otherwise = Seek (o {suffix = Array.mapWithIndex (\i x -> if i == (idx - Array.length prefix) then f x else x) suffix})


getIndex :: forall a. Int -> Seek a -> Maybe a
getIndex idx (Seek {prefix, cursor, suffix})
  | idx == Array.length prefix = Just cursor
  | idx < Array.length prefix = Array.index prefix idx
  | otherwise = Array.index suffix (idx - Array.length prefix)


_inIndex :: forall a. Int -> Lens' (Seek a) a
_inIndex idx = lens get set
  where
    get :: Seek a -> a
    get (Seek {prefix, cursor, suffix})
      | idx == Array.length prefix = cursor
      | idx <= 0 = case Array.uncons prefix of
          Just {head} -> head
          _           -> cursor
      | idx < Array.length prefix = case Array.index prefix idx of
          Just x -> x
          _      -> cursor
      | otherwise =
          let idx' = idx - Array.length prefix
          in  case Array.index suffix idx' of
            Just x -> x
            _      -> cursor

    set :: Seek a -> a -> Seek a
    set (Seek {prefix, cursor, suffix}) x
      | idx == Array.length prefix = Seek {prefix, cursor: x, suffix}
      | idx < Array.length prefix = Seek {prefix: fromMaybe prefix $ Array.insertAt idx x prefix, cursor, suffix}
      | otherwise = Seek {prefix, cursor, suffix: fromMaybe suffix $ Array.insertAt (idx - Array.length prefix) x suffix}


toArray :: forall a. Seek a -> Array a
toArray (Seek {prefix, cursor, suffix}) = prefix <> [cursor] <> suffix


cursorIndex :: forall a. Seek a -> Int
cursorIndex (Seek {prefix}) = Array.length prefix


current :: forall a. Seek a -> a
current (Seek {cursor}) = cursor


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
