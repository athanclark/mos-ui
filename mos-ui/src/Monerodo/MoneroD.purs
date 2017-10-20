module Monerodo.MoneroD where

import Prelude
import Data.Either (Either (..))
import Data.URI.Authority (Authority, parser)
import Data.URI.Authority as Auth
import Data.String as String
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.?))
import Control.Alternative ((<|>))
import Text.Parsing.StringParser (runParser)


data MoneroDLog
  = SyncProgress
      { amount   :: Int
      , total    :: Int
      , host     :: Authority
      , peer     :: Int
      , polarity :: SyncPolarity
      }
  | SyncNewTopBlock
      { host     :: Authority
      , polarity :: SyncPolarity
      , peer     :: Int
      , top      :: Int
      , current  :: Int
      , behind   :: Int
      , days     :: Int
      }
  | MoneroDOther String

instance encodeJsonMoneroDLog :: EncodeJson MoneroDLog where
  encodeJson (MoneroDOther _) = encodeJson ""
  encodeJson (SyncNewTopBlock {host,polarity,peer,top,current,behind,days})
    =  "host" := (String.drop 2 $ Auth.print host)
    ~> "polarity" := polarity
    ~> "peer" := peer
    ~> "top" := top
    ~> "current" := current
    ~> "behind" := behind
    ~> "days" := days
    ~> jsonEmptyObject
  encodeJson (SyncProgress {amount,total,host,peer,polarity})
    =  "amount" := amount
    ~> "total" := total
    ~> "host" := (String.drop 2 $ Auth.print host)
    ~> "peer" := peer
    ~> "polarity" := polarity
    ~> jsonEmptyObject

instance decodeJsonMoneroDLog :: DecodeJson MoneroDLog where
  decodeJson json = do
    o <- decodeJson json
    parseProgress o <|> parseNewTopBlock o
    where
      parseProgress o = do
        o' <- o .? "progress"
        amount <- o' .? "amount"
        total <- o' .? "total"
        host' <- o' .? "host"
        host <- case runParser parser ("//" <> host') of
          Left e -> Left (show e)
          Right x -> pure x
        peer <- o' .? "peer"
        polarity <- o' .? "polarity"
        pure $ SyncProgress
          { amount
          , host
          , total
          , peer
          , polarity
          }
      parseNewTopBlock o = do
        o' <- o .? "newTopBlock"
        top <- o' .? "top"
        peer <- o' .? "peer"
        host' <- o' .? "host"
        host <- case runParser parser ("//" <> host') of
          Left e -> Left (show e)
          Right x -> pure x
        days <- o' .? "days"
        behind <- o' .? "behind"
        current <- o' .? "current"
        polarity <- o' .? "polarity"
        pure $ SyncNewTopBlock
          { polarity
          , current
          , behind
          , days
          , host
          , peer
          , top
          }

data SyncPolarity = INC | OUT

instance encodeJsonSyncPolarity :: EncodeJson SyncPolarity where
  encodeJson INC = encodeJson "INC"
  encodeJson OUT = encodeJson "OUT"

instance decodeJsonSyncPolarity :: DecodeJson SyncPolarity where
  decodeJson json = do
    s <- decodeJson json
    case s of
      _ | s == "INC" -> pure INC
        | s == "OUT" -> pure OUT
        | otherwise -> Left "Not a SyncPolarity"
