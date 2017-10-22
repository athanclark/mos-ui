{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , NamedFieldPuns
  #-}

module System.SystemD.Status where

import Prelude hiding (print)
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Text (Text, replace)
import Data.Char (isSpace)
import Data.Conduit ((=$=))
import Data.Conduit.Attoparsec (sinkParserEither, ParseError)
import Data.Conduit.Process (sourceCmdWithConsumer)
import Data.Conduit.Text (decode, utf8)
import Data.Conduit.Combinators (print)
import Data.Attoparsec.Text (Parser, char, string, takeWhile1, endOfLine, skipWhile, parseOnly, (<?>))
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Time (zonedTime)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String, Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Control.Monad (void)
import Control.Applicative ((<|>))


data LoadedState
  = Loaded
  | NotFound
  deriving (Show)

instance ToJSON LoadedState where
  toJSON Loaded = String "loaded"
  toJSON NotFound = String "not-found"

instance FromJSON LoadedState where
  parseJSON (String x) | x == "loaded" = pure Loaded
                       | x == "not-found" = pure NotFound
                       | otherwise = fail "not a LoadedState"
  parseJSON x = typeMismatch "LoadedState" x

loadedState :: Parser (LoadedState, Text)
loadedState = do
  _ <- string "   Loaded: " <?> "loaded init"
  let loaded = Loaded <$ string "loaded (" <?> "loaded token"
      notFound = NotFound <$ string "not-found (" <?> "not-found token"
  l <- loaded <|> notFound
  r <- takeWhile1 (/= ')')
  void $ char ')'
  endOfLine
  pure (l,r)

loadedStateTest :: Text
loadedStateTest = "   Loaded: loaded (/lib/systemd/system/wpa_supplicant.service; disabled; vendor preset: enabled)"

data ActiveState
  = Inactive
  | Failed
  | Active
  deriving (Show)

instance ToJSON ActiveState where
  toJSON Inactive = String "inactive"
  toJSON Active = String "active"
  toJSON Failed = String "failed"

instance FromJSON ActiveState where
  parseJSON (String x) | x == "inactive" = pure Inactive
                       | x == "active" = pure Active
                       | x == "failed" = pure Failed
                       | otherwise = fail "not a ActiveState"
  parseJSON x = typeMismatch "ActiveState" x

activeState :: Parser (ActiveState, Maybe UTCTime)
activeState = do
  _ <- string "   Active: " <?> "initial active"
  let active = do
        _ <- string "active (running) " <?> "actually active"
        _ <- string "since "
        _ <- A.take 4
        r <- takeWhile1 (/= ';')
        _ <- takeWhile1 (/= '\n')
        case parseOnly zonedTime (replaceTZ r) of
          Left e -> fail e
          Right time ->
            pure (Active, Just (zonedTimeToUTC time))
      failed = do
        _ <- string "failed (Result: exit-code) " <?> "actually failed"
        _ <- string "since "
        _ <- A.take 4
        r <- takeWhile1 (/= ';')
        _ <- takeWhile1 (/= '\n')
        case parseOnly zonedTime (replaceTZ r) of
          Left e -> fail e
          Right time ->
            pure (Failed, Just (zonedTimeToUTC time))
      inactive = do
        _ <- string "inactive (dead)\n" <?> "actually inactive"
        pure (Inactive, Nothing)
  active <|> failed <|> inactive
  where
    replaceTZ x = foldr (\(k,v) acc -> replace k v acc) x timezones
    timezones =
      [ ("ACDT"   , "+10:30")
      , ("ACST"   , "+09:30")
      , ("ACWT"   , "+08:45")
      , ("ADT"    , "-03:00")
      , ("ACT"    , "-05:00")
      , ("AEDT"   , "+11:00")
      , ("AEST"   , "+10:00")
      , ("AFT"    , "+04:30")
      , ("AKDT"   , "-08:00")
      , ("AKST"   , "-09:00")
      , ("ALMT"   , "+06:00")
      , ("AMT"    , "-04:00")
      , ("AMST"   , "-03:00")
      , ("ANAT"   , "+12:00")
      , ("ANAST"  , "+12:00")
      , ("AQTT"   , "+05:00")
      , ("ART"    , "-03:00")
      , ("AST"    , "-04:00")
      , ("AWDT"   , "+09:00")
      , ("AWST"   , "+08:00")
      , ("AZOT"   , "-01:00")
      , ("AZOST"  , "+00:00")
      , ("AZT"    , "+04:00")
      , ("AZST"   , "+05:00")
      , ("BNT"    , "+08:00")
      , ("BDT"    , "+06:00")
      , ("BOT"    , "-04:00")
      , ("BRT"    , "-03:00")
      , ("BRST"   , "-02:00")
      , ("BST"    , "+01:00")
      , ("BTT"    , "+06:00")
      , ("CAST"   , "+08:00")
      , ("CAT"    , "+02:00")
      , ("CCT"    , "+06:30")
      , ("CDT"    , "-05:00")
      , ("CEDT"   , "+02:00")
      , ("CEST"   , "+02:00")
      , ("CET"    , "+01:00")
      , ("CHADT"  , "+13:45")
      , ("CHAST"  , "+12:45")
      , ("CHOT"   , "+08:00")
      , ("CHOST"  , "+09:00")
      , ("CHsT"   , "+10:00")
      , ("CHUT"   , "+10:00")
      , ("CIT"    , "+08:00")
      , ("CKT"    , "-10:00")
      , ("CLST"   , "-03:00")
      , ("CLT"    , "-04:00")
      , ("COT"    , "-05:00")
      , ("CST"    , "-06:00")
      , ("CVT"    , "-01:00")
      , ("CWST"   , "+08:45")
      , ("CXT"    , "+07:00")
      , ("DAVT"   , "+07:00")
      , ("DDUT"   , "+10:00")
      , ("EASST"  , "-05:00")
      , ("EAST"   , "-06:00")
      , ("EAT"    , "+03:00")
      , ("ECT"    , "-05:00")
      , ("EDT"    , "-04:00")
      , ("EEDT"   , "+03:00")
      , ("EEST"   , "+03:00")
      , ("EET"    , "+02:00")
      , ("EGT"    , "-01:00")
      , ("EGST"   , "+00:00")
      , ("EST"    , "-05:00")
      , ("EIT"    , "+09:00")
      , ("FET"    , "+03:00")
      , ("FJT"    , "+12:00")
      , ("FJST"   , "+13:00")
      , ("FKST"   , "-03:00")
      , ("FKT"    , "-04:00")
      , ("FNT"    , "-02:00")
      , ("GALT"   , "-06:00")
      , ("GAMT"   , "-09:00")
      , ("GET"    , "+04:00")
      , ("GFT"    , "-03:00")
      , ("GILT"   , "+12:00")
      , ("GMT"    , "+00:00")
      , ("GST"    , "-02:00")
      , ("GYT"    , "-04:00")
      , ("HADT"   , "-09:00")
      , ("HAST"   , "-10:00")
      , ("HKT"    , "+08:00")
      , ("HOVT"   , "+07:00")
      , ("HOVST"  , "+08:00")
      , ("HST"    , "-10:00")
      , ("ICT"    , "+07:00")
      , ("IDT"    , "+03:00")
      , ("IOT"    , "+06:00")
      , ("IRDT"   , "+04:30")
      , ("IRKT"   , "+08:00")
      , ("IRKST"  , "+09:00")
      , ("IRST"   , "+03:30")
      , ("IST"    , "+05:30")
      , ("JST"    , "+09:00")
      , ("KGT"    , "+06:00")
      , ("KOST"   , "+11:00")
      , ("KRAT"   , "+07:00")
      , ("KRAST"  , "+08:00")
      , ("KST"    , "+09:00")
      , ("KUYT"   , "+04:00")
      , ("LHDT"   , "+11:00")
      , ("LHST"   , "+10:30")
      , ("LINT"   , "+14:00")
      , ("MAGT"   , "+10:00")
      , ("MAGST"  , "+12:00")
      , ("MART"   , "-09:30")
      , ("MAWT"   , "+05:00")
      , ("MDT"    , "-06:00")
      , ("MeST"   , "-08:00")
      , ("MHT"    , "+12:00")
      , ("MIST"   , "+11:00")
      , ("MMT"    , "+06:30")
      , ("MSD"    , "+04:00")
      , ("MSK"    , "+03:00")
      , ("MST"    , "-07:00")
      , ("MUT"    , "+04:00")
      , ("MVT"    , "+05:00")
      , ("MYT"    , "+08:00")
      , ("NCT"    , "+11:00")
      , ("NDT"    , "-02:30")
      , ("NFT"    , "+11:30")
      , ("NOVT"   , "+06:00")
      , ("NOVST"  , "+07:00")
      , ("NPT"    , "+05:45")
      , ("NRT"    , "+12:00")
      , ("NST"    , "-03:30")
      , ("NT"     , "-03:30")
      , ("NUT"    , "-11:00")
      , ("NZDT"   , "+13:00")
      , ("NZST"   , "+12:00")
      , ("OMST"   , "+06:00")
      , ("OMSST"  , "+07:00")
      , ("ORAT"   , "+05:00")
      , ("PDT"    , "-07:00")
      , ("PET"    , "-05:00")
      , ("PETT"   , "+12:00")
      , ("PETST"  , "+12:00")
      , ("PGT"    , "+10:00")
      , ("PHT"    , "+08:00")
      , ("PHOT"   , "+13:00")
      , ("PKT"    , "+05:00")
      , ("PMDT"   , "-02:00")
      , ("PMST"   , "-03:00")
      , ("PONT"   , "+11:00")
      , ("PST"    , "-08:00")
      , ("PWT"    , "+09:00")
      , ("PYT"    , "-04:00")
      , ("PYST"   , "-03:00")
      , ("QYZT"   , "+06:00")
      , ("RET"    , "+04:00")
      , ("ROTT"   , "-03:00")
      , ("SAKT"   , "+10:00")
      , ("SAKST"  , "+12:00")
      , ("SAMT"   , "+04:00")
      , ("SAST"   , "+02:00")
      , ("SBT"    , "+11:00")
      , ("SCT"    , "+04:00")
      , ("SGT"    , "+08:00")
      , ("SRT"    , "-03:00")
      , ("SLT"    , "+05:30")
      , ("SLST"   , "+05:30")
      , ("SRET"   , "+11:00")
      , ("SST"    , "-11:00")
      , ("SYOT"   , "+03:00")
      , ("TAHT"   , "-10:00")
      , ("TFT"    , "+05:00")
      , ("TJT"    , "+05:00")
      , ("TKT"    , "+13:00")
      , ("TLT"    , "+09:00")
      , ("TMT"    , "+05:00")
      , ("TOT"    , "+13:00")
      , ("TRUT"   , "+10:00")
      , ("TVT"    , "+12:00")
      , ("ULAT"   , "+08:00")
      , ("ULAST"  , "+09:00")
      , ("UTC"    , "+00:00")
      , ("UYST"   , "-02:00")
      , ("UYT"    , "-03:00")
      , ("UZT"    , "+05:00")
      , ("VET"    , "-04:30")
      , ("VLAT"   , "+10:00")
      , ("VLAST"  , "+11:00")
      , ("VOLT"   , "+04:00")
      , ("VUT"    , "+11:00")
      , ("WAKT"   , "+12:00")
      , ("WAT"    , "+01:00")
      , ("WART"   , "-04:00")
      , ("WAST"   , "+02:00")
      , ("WDT"    , "+09:00")
      , ("WEDT"   , "+01:00")
      , ("WEST"   , "+01:00")
      , ("WET"    , "+00:00")
      , ("WFT"    , "+12:00")
      , ("WGT"    , "-03:00")
      , ("WGST"   , "-02:00")
      , ("WIB"    , "+07:00")
      , ("WIT"    , "+09:00")
      , ("WITA"   , "+08:00")
      , ("WST"    , "+08:00")
      , ("WT"     , "+00:00")
      , ("YAKT"   , "+09:00")
      , ("YAKST"  , "+10:00")
      , ("YAP"    , "+10:00")
      , ("YEKT"   , "+05:00")
      , ("YEKST"  , "+06:00")
      ]

activeStateTest :: Text
activeStateTest = "   Active: active (running) since Sun 2017-10-15 18:35:25 MDT; 3 days ago"


data SystemDStatus = SystemDStatus
  { systemdStatusName :: Text
  , systemdStatusDescription :: Text
  , systemdStatusLoadedState :: LoadedState
  , systemdStatusLoadedStateExtra :: Text
  , systemdStatusActiveState :: ActiveState
  , systemdStatusActiveStateSince :: Maybe UTCTime
  } deriving (Show)

instance ToJSON SystemDStatus where
  toJSON SystemDStatus{..} = object
    [ "name" .= systemdStatusName
    , "description" .= systemdStatusDescription
    , "loadedState" .= systemdStatusLoadedState
    , "loadedStateExtra" .= systemdStatusLoadedStateExtra
    , "activeState" .= systemdStatusActiveState
    , "activeStateSince" .= systemdStatusActiveStateSince
    ]

instance FromJSON SystemDStatus where
  parseJSON (Object o) = do
    systemdStatusName <- o .: "name"
    systemdStatusDescription <- o .: "description"
    systemdStatusLoadedState <- o .: "loadedState"
    systemdStatusLoadedStateExtra <- o .: "loadedStateExtra"
    systemdStatusActiveState <- o .: "activeState"
    systemdStatusActiveStateSince <- o .: "activeStateSince"
    pure SystemDStatus
      { systemdStatusName
      , systemdStatusDescription
      , systemdStatusLoadedState
      , systemdStatusLoadedStateExtra
      , systemdStatusActiveState
      , systemdStatusActiveStateSince
      }
  parseJSON x = typeMismatch "SystemDStatus" x

systemdStatus :: Parser SystemDStatus
systemdStatus = do
  _ <- string "● " <?> "initial dot"
  name <- takeWhile1 (not . isSpace)
  _ <- string " - " <?> "name-description breaker"
  description <- takeWhile1 (/= '\n')
  void $ char '\n'
  (loadedState',loadedStateExtra) <- loadedState
  (activeState',activeStateExtra) <- activeState
  skipWhile (const True)
  pure SystemDStatus
    { systemdStatusName = name
    , systemdStatusDescription = description
    , systemdStatusLoadedState = loadedState'
    , systemdStatusLoadedStateExtra = loadedStateExtra
    , systemdStatusActiveState = activeState'
    , systemdStatusActiveStateSince = activeStateExtra
    }

getServiceStatus :: String -> IO (Either ParseError SystemDStatus)
getServiceStatus service =
  snd <$> sourceCmdWithConsumer ("systemctl status " ++ service) (decode utf8 =$= sinkParserEither systemdStatus)
