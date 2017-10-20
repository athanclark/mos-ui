{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  , Rank2Types
  , FlexibleContexts
  #-}

module Monerodo.MoneroD where

import Types (MonadApp)

import Data.Attoparsec.Text (Parser, string, decimal, sepBy, endOfLine, eitherP, takeWhile1, char, parseOnly, endOfInput)
import Data.Attoparsec.Path (absFilePath)
import Data.Attoparsec.IP (ipv4, ipv6)
import Data.Attoparsec.Time (localTime)
import Data.Text (Text, pack, unpack)
import Data.Conduit (Producer, (=$=), ($$))
import Data.Conduit.Attoparsec (sinkParser, sinkParserEither, ParseError)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.Binary as B
import qualified Data.ByteString as BS
import Data.Conduit.Text (decode, utf8)
import Data.Char (isControl, isSpace)
import Data.URI.Auth (URIAuth, parseURIAuth)
import Data.Time (UTCTime)
import Data.Time.LocalTime (TimeZone, localTimeToUTC)
import Data.Vector (Vector)
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.=), object, Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative (optional, (<|>), many)
import Control.Alternative.Vector (manyV)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import Text.Read (readMaybe)
import Path (Path, Abs, File, toFilePath)
import Net.Types (IPv4, IPv6)
import qualified Net.IPv4.Text as IPv4
import qualified Net.IPv6.Text as IPv6


instance ToJSON URIAuth where
  toJSON x = toJSON $ show x

instance FromJSON URIAuth where
  parseJSON (String x) = case parseOnly parseURIAuth x of
    Left e -> fail e
    Right x -> pure x
  parseJSON x = typeMismatch "URIAuth" x


data MoneroDConfig
  = MaxConcurrency Int
  | DataDir (Path Abs File)
  | EnforceDnsCheckpointing Bool
  | MaxThreadsPrepBlocks Int
  | FastBlockSync Bool
  | BlockSyncSize Int
  | P2pBindPort Int
  | P2pBindIp (Either IPv4 IPv6)
  | P2pExternalPort Int
  | HideMyPort Bool
  | NoIgd Bool
  | Offline Bool
  | MaxOutPeers Int
  | LimitRateUp Int
  | LimitRateDown Int
  | LimitRate Int
  | RpcBindPort Int
  | RpcBindIp (Either IPv4 IPv6)
  | RestrictedRpc Bool
  | RpcLogin String (Maybe String)
  | ConfirmExternalBind Bool

instance Show MoneroDConfig where
  show (MaxConcurrency x) = "max-concurrency=" ++ show x
  show (DataDir x) = "data-dir=" ++ toFilePath x
  show (EnforceDnsCheckpointing x) = "enforce-dns-checkpointing=" ++ show (if x then 1 else 0)
  show (MaxThreadsPrepBlocks x) = "prep-blocks-threads=" ++ show x
  show (FastBlockSync x) = "fast-block-sync=" ++ show (if x then 1 else 0)
  show (BlockSyncSize x) = "block-sync-size=" ++ show x
  show (P2pBindPort x) = "p2p-bind-port=" ++ show x
  show (P2pBindIp x) = "p2p-bind-ip=" ++ unpack (either IPv4.encode IPv6.encode x)
  show (P2pExternalPort x) = "p2p-external-port=" ++ show x
  show (HideMyPort x) = "hide-my-port=" ++ show (if x then 1 else 0)
  show (NoIgd x) = "no-igd=" ++ show (if x then 1 else 0)
  show (Offline x) = "offline=" ++ show (if x then 1 else 0)
  show (MaxOutPeers x) = "out-peers=" ++ show x
  show (LimitRateUp x) = "limit-rate-up=" ++ show x
  show (LimitRateDown x) = "limit-rate-down=" ++ show x
  show (LimitRate x) = "limit-rate=" ++ show x
  show (RpcBindPort x) = "rpc-bind-port=" ++ show x
  show (RpcBindIp x) = "rpc-bind-ip=" ++ unpack (either IPv4.encode IPv6.encode x)
  show (RestrictedRpc x) = "restricted-rpc=" ++ show (if x then 1 else 0)
  show (RpcLogin u p) = "rpc-login=" ++ u ++ maybe "" (':':) p
  show (ConfirmExternalBind x) = "confirm-external-bind=" ++ show (if x then 1 else 0)

mkConfig :: [MoneroDConfig] -> String
mkConfig xs = unlines $ show <$> xs


configParser :: Parser MoneroDConfig
configParser
   =  parseMaxConcurrency
  <|> parseDataDir
  <|> parseEnforceDnsCheckpointing
  <|> parseMaxThreadsPrep
  <|> parseFastBlockSync
  <|> parseBlockSyncSize
  <|> parseP2PBindPort
  <|> parseP2PBindIp
  <|> parseP2PExternalPort
  <|> parseHideMyPort
  <|> parseNoIgd
  <|> parseOffline
  <|> parseMaxOutPeers
  <|> parseLimitRateUp
  <|> parseLimitRateDown
  <|> parseLimitRate
  <|> parseRpcBindPort
  <|> parseRpcBindIp
  <|> parseRestrictedRpc
  <|> parseRpcLogin
  <|> parseConfirmExternalBind
  where
    parseMaxConcurrency = do
      string "max-concurrency="
      MaxConcurrency <$> decimal
    parseDataDir = do
      string "data-dir="
      DataDir <$> absFilePath
    parseEnforceDnsCheckpointing = do
      string "enforce-dns-checkpointing="
      (EnforceDnsCheckpointing . (/= 0)) <$> decimal
    parseMaxThreadsPrep = do
      string "prep-blocks-threads="
      MaxThreadsPrepBlocks <$> decimal
    parseFastBlockSync = do
      string "fast-block-sync="
      (FastBlockSync . (/= 0)) <$> decimal
    parseBlockSyncSize = do
      string "block-sync-size="
      BlockSyncSize <$> decimal
    parseP2PBindPort = do
      string "p2p-bind-port="
      P2pBindPort <$> decimal
    parseP2PBindIp = do
      string "p2p-bind-ip="
      P2pBindIp <$> eitherP ipv4 ipv6
    parseP2PExternalPort = do
      string "p2p-external-port="
      P2pExternalPort <$> decimal
    parseHideMyPort = do
      string "hide-my-port="
      (HideMyPort . (/= 0)) <$> decimal
    parseNoIgd = do
      string "no-igd="
      (NoIgd . (/= 0)) <$> decimal
    parseOffline = do
      string "offline="
      (Offline . (/= 0)) <$> decimal
    parseMaxOutPeers = do
      string "out-peers="
      MaxOutPeers <$> decimal
    parseLimitRateUp = do
      string "limit-rate-up="
      LimitRateUp <$> decimal
    parseLimitRateDown = do
      string "limit-rate-down="
      LimitRateDown <$> decimal
    parseLimitRate = do
      string "limit-rate="
      LimitRate <$> decimal
    parseRpcBindPort = do
      string "rpc-bind-port="
      RpcBindPort <$> decimal
    parseRpcBindIp = do
      string "rpc-bind-ip="
      RpcBindIp <$> eitherP ipv4 ipv6
    parseRestrictedRpc = do
      string "restricted-rpc="
      (RestrictedRpc . (/= 0)) <$> decimal
    parseRpcLogin = do
      string "rpc-login="
      u <- unpack <$> takeWhile1 (\x -> not (isControl x || isSpace x))
      mp <- let pass = do
                  void $ char ':'
                  (Just . unpack) <$> takeWhile1 (\x -> not (isControl x || isSpace x))
                bad = pure Nothing
            in  pass <|> bad
      pure (RpcLogin u mp)
    parseConfirmExternalBind = do
      string "confirm-external-bind="
      (ConfirmExternalBind . (/= 0)) <$> decimal

parseConfigStream ::  ( MonadThrow m
                      , MonadResource m
                      )
                  => FilePath -> m [MoneroDConfig]
parseConfigStream f = sourceFile f =$= decode utf8 $$ sinkParser (configParser `sepBy` endOfLine)


data WithTimestamp a = WithTimestamp
  { withTimestampTime  :: UTCTime
  , withTimestampValue :: a
  }
  deriving (Show)

withTimestamp :: TimeZone -> Parser a -> Parser (WithTimestamp a)
withTimestamp timeZone content = do
  t <- localTime
  c <- content
  pure WithTimestamp
    { withTimestampTime = localTimeToUTC timeZone t
    , withTimestampValue = c
    }

data SyncPolarity = INC | OUT
  deriving (Show, Read)

instance ToJSON SyncPolarity where
  toJSON x = toJSON (show x)

instance FromJSON SyncPolarity where
  parseJSON (String s) = case readMaybe $ unpack s of
    Nothing -> fail "Not a SyncPolarity"
    Just p -> pure p
  parseJSON x = typeMismatch "SyncPolarity" x

syncPolarity :: Parser SyncPolarity
syncPolarity =
  let inc = INC <$ string "INC"
      out = OUT <$ string "OUT"
  in  inc <|> out

peer :: Parser Int
peer = do
  void $ char '['
  void $ string "P2P"
  n <- decimal
  void $ char ']'
  pure n

data MoneroDLog
  = SyncProgress
      { syncProgressAmount   :: Int
      , syncProgressTotal    :: Int
      , syncProgressHost     :: URIAuth
      , syncProgressPeer     :: Int
      , syncProgressPolarity :: SyncPolarity
      }
  | SyncNewTopBlock
      { syncNewTopBlockHost     :: URIAuth
      , syncNewTopBlockPolarity :: SyncPolarity
      , syncNewTopBlockPeer     :: Int
      , syncNewTopBlockTop      :: Int
      , syncNewTopBlockCurrent  :: Int
      , syncNewTopBlockBehind   :: Int
      , syncNewTopBlockDays     :: Int
      }
  | MoneroDOther Text
  deriving (Show)

instance ToJSON MoneroDLog where
  toJSON (MoneroDOther _) = String ""
  toJSON (SyncProgress{..}) = object
    [ "progress" .= object
      [ "amount" .= syncProgressAmount
      , "total" .= syncProgressTotal
      , "host" .= syncProgressHost
      , "peer" .= syncProgressPeer
      , "polarity" .= syncProgressPolarity
      ]
    ]
  toJSON (SyncNewTopBlock{..}) = object
    [ "newTopBlock" .= object
      [ "host" .= syncNewTopBlockHost
      , "polarity" .= syncNewTopBlockPolarity
      , "peer" .= syncNewTopBlockPeer
      , "top" .= syncNewTopBlockTop
      , "current" .= syncNewTopBlockCurrent
      , "behind" .= syncNewTopBlockBehind
      , "days" .= syncNewTopBlockDays
      ]
    ]

instance FromJSON MoneroDLog where
  parseJSON (Object o) = parseProgress <|> parseNewTopBlock
    where
      parseProgress = do
        o' <- o .: "progress"
        syncProgressAmount <- o' .: "amount"
        syncProgressTotal <- o' .: "total"
        syncProgressHost <- o' .: "host"
        syncProgressPeer <- o' .: "peer"
        syncProgressPolarity <- o' .: "polarity"
        pure SyncProgress
          { syncProgressAmount
          , syncProgressHost
          , syncProgressTotal
          , syncProgressPeer
          , syncProgressPolarity
          }
      parseNewTopBlock = do
        o' <- o .: "newTopBlock"
        syncNewTopBlockTop <- o' .: "top"
        syncNewTopBlockPeer <- o' .: "peer"
        syncNewTopBlockHost <- o' .: "host"
        syncNewTopBlockDays <- o' .: "days"
        syncNewTopBlockBehind <- o' .: "behind"
        syncNewTopBlockCurrent <- o' .: "current"
        syncNewTopBlockPolarity <- o' .: "polarity"
        pure SyncNewTopBlock
          { syncNewTopBlockPolarity
          , syncNewTopBlockCurrent
          , syncNewTopBlockBehind
          , syncNewTopBlockDays
          , syncNewTopBlockHost
          , syncNewTopBlockPeer
          , syncNewTopBlockTop
          }
  parseJSON x = typeMismatch "MoneroDLog" x


data MoneroDLogType = ERROR | WARN | INFO
  deriving (Show)

monerodLogType :: Parser MoneroDLogType
monerodLogType =
  let info = INFO <$ string "INFO"
      warn = WARN <$ string "WARN"
      error' = ERROR <$ string "ERROR"
  in  info <|> warn <|> error'

data NetScope = DNS | P2P | CN
  deriving (Show)

netScope :: Parser NetScope
netScope =
  let dns = DNS <$ string ".dns"
      p2p = P2P <$ string ".p2p"
      cn  = CN  <$ string ".cn"
  in  dns <|> p2p <|> cn

data MoneroDLogScope = Global | Net (Maybe NetScope)
  deriving (Show)

monerodLogScope :: Parser MoneroDLogScope
monerodLogScope =
  let global = Global <$ string "global"
      netDNS = do
        _ <- string "net"
        Net <$> optional netScope
  in  global <|> netDNS

newtype MoneroDLogOrigin = MoneroDLogOrigin Text
  deriving (Show)

monerodLogOrigin :: Parser MoneroDLogOrigin
monerodLogOrigin =
  MoneroDLogOrigin <$> takeWhile1 (/= '\t')

monerodHostPolarity :: Parser (URIAuth, SyncPolarity)
monerodHostPolarity = do
  void $ char '['
  h <- parseURIAuth
  void $ char ' '
  p <- syncPolarity
  void $ char ']'
  pure (h,p)

monerodLog :: Parser MoneroDLog
monerodLog = do
  void $ char '\t'
  p <- peer
  void $ char '\t'
  type' <- monerodLogType
  void $ char ' '
  void $ char '\t'
  s <- monerodLogScope
  void $ char '\t'
  o <- monerodLogOrigin
  void $ char '\t'
  let syncProgress = do
        void $ string "\ESC[1;33m"
        (h,po) <- monerodHostPolarity
        void $ string "  Synced "
        soFar <- decimal
        void $ char '/'
        total <- decimal
        void $ string "\ESC[0m"
        pure SyncProgress
          { syncProgressAmount = soFar
          , syncProgressTotal = total
          , syncProgressHost = h
          , syncProgressPolarity = po
          , syncProgressPeer = p
          }
      syncNewTopBlock = do
        (h,po) <- monerodHostPolarity
        void $ string " Sync data returned a new top block candidate: "
        soFar <- decimal
        void $ string " -> "
        total <- decimal
        void $ string " [Your node is "
        behind <- decimal
        void $ string " blocks ("
        days <- decimal
        void $ string " days) behind]"
        pure SyncNewTopBlock
          { syncNewTopBlockBehind = behind
          , syncNewTopBlockCurrent = soFar
          , syncNewTopBlockTop = total
          , syncNewTopBlockDays = days
          , syncNewTopBlockHost = h
          , syncNewTopBlockPeer = p
          , syncNewTopBlockPolarity = po
          }
      other = MoneroDOther <$> takeWhile1 (/= '\n')
  syncProgress <|> syncNewTopBlock <|> other

parseLogStream :: MonadApp stM m => TimeZone -> Producer m BS.ByteString -> m (Either ParseError [WithTimestamp MoneroDLog])
parseLogStream tz x = x =$= decode utf8 $$ sinkParserEither (many (withTimestamp tz monerodLog))
