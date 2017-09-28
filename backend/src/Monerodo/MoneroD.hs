{-# LANGUAGE
    OverloadedStrings
  #-}

module Monerodo.MoneroD where

import Data.Attoparsec.Text (Parser, string)
import Data.Text (Text, pack)
import Data.Conduit (Producer, (=$=), ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.Binary as B
import Data.Conduit.Text (decode, utf8)
import Control.Applicative (many)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)



data MoneroDConfig
  = MaxConcurrency Int
  | DataDir FilePath
  | EnforceDnsCheckpointing Bool
  | MaxThreadsPrepBlocks Int
  | FastBlockSync Bool
  | BlockSyncSize Int
  | P2pBindPort Int
  | P2pBindIp String
  | P2pExternalPort Int
  | HideMyPort Bool
  | NoIgd Bool
  | Offline Bool
  | MaxOutPeers Int
  | LimitRateUp Int
  | LimitRateDown Int
  | LimitRate Int
  | RpcBindPort Int
  | RpcBindIp String
  | RestrictedRpc Bool
  | RpcLogin String
  | ConfirmExternalBind Bool

instance Show MoneroDConfig where
  show (MaxConcurrency x) = "max-concurrency=" ++ show x
  show (DataDir x) = "data-dir=" ++ x
  show (EnforceDnsCheckpointing x) = "enforce-dns-checkpointing=" ++ show (if x then 1 else 0)
  show (MaxThreadsPrepBlocks x) = "prep-blocks-threads=" ++ show x
  show (FastBlockSync x) = "fast-block-sync=" ++ show (if x then 1 else 0)
  show (BlockSyncSize x) = "block-sync-size=" ++ show x
  show (P2pBindPort x) = "p2p-bind-port=" ++ show x
  show (P2pBindIp x) = "p2p-bind-ip=" ++ x
  show (P2pExternalPort x) = "p2p-external-port=" ++ show x
  show (HideMyPort x) = "hide-my-port=" ++ show (if x then 1 else 0)
  show (NoIgd x) = "no-igd=" ++ show (if x then 1 else 0)
  show (Offline x) = "offline=" ++ show (if x then 1 else 0)
  show (MaxOutPeers x) = "out-peers=" ++ show x
  show (LimitRateUp x) = "limit-rate-up=" ++ show x
  show (LimitRateDown x) = "limit-rate-down=" ++ show x
  show (LimitRate x) = "limit-rate=" ++ show x
  show (RpcBindPort x) = "rpc-bind-port=" ++ show x
  show (RpcBindIp x) = "rpc-bind-ip=" ++ show x
  show (RestrictedRpc x) = "restricted-rpc=" ++ show (if x then 1 else 0)
  show (RpcLogin x) = "rpc-login=" ++ x
  show (ConfirmExternalBind x) = "confirm-external-bind=" ++ show (if x then 1 else 0)

mkConfig :: [MoneroDConfig] -> String
mkConfig xs = unlines $ show <$> xs



data MoneroDLog
  = Foo
  deriving (Show)

logParser :: Parser MoneroDLog
logParser =
  where
    parseMaxConcurrency = do
      string "max-concurrency="
      MaxConcurrency <$> decimal
    parseDataDir = do
      string "data-dir="
      DataDir <$> 

parseLogStream :: ( MonadThrow m
                  , MonadResource m
                  )
               => FilePath -> m [MoneroDLog]
parseLogStream f = sourceFile f =$= B.lines =$= decode utf8 $$ sinkParser (many logParser)
