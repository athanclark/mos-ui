{-# LANGUAGE
    NamedFieldPuns
  , DeriveGeneric
  #-}

module Main.Options where

import Types.Env (Env (..))

import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly)
import Data.Attoparsec.Path (absFilePath)
import Data.Monoid ((<>))
import Control.Exception (Exception, throw)
import DBus.Client (connectSession)
import Options.Applicative (Parser, long, value, showDefault, help, strOption)
import Path (Path, Abs, File)
import GHC.Generics (Generic)
import System.INotify (initINotify)


data ArgsImpl = ArgsImpl
  { argsImplMoneroDLogFile :: FilePath
  , argsImplMoneroDConfigFile :: FilePath
  , argsImplMoneroDService :: String
  }


args :: String -> Parser ArgsImpl
args username
  =  ArgsImpl <$> strOption moneroDLog
              <*> strOption moneroDConfig
              <*> strOption moneroDService
  where
    moneroDLog
      =  long "monerod-log-file"
      <> value ("/home/" ++ username ++ "/.bitmonero/bitmonero.log")
      <> help "Absolute path to the file monerod logs to."
      <> showDefault
    moneroDConfig
      =  long "monerod-config-file"
      <> value ("/home/" ++ username ++ "/.bitmonero/bitmonero.conf")
      <> help "Absolute path to the monerod config file."
      <> showDefault
    moneroDService
      =  long "monerod-service"
      <> value "monerod.service"
      <> help "Name of the installed systemd service for monerod."
      <> showDefault


data Args = Args
  { argsMoneroDLogFile :: Path Abs File
  , argsMoneroDConfigFile :: Path Abs File
  , argsMoneroDService :: String
  }

data ArgsException
  = MoneroDLogFileParseFailure String
  | MoneroDConfigFileParseFailure String
  deriving (Generic, Show)

instance Exception ArgsException

getArgs :: ArgsImpl -> IO Args
getArgs ArgsImpl{ argsImplMoneroDLogFile
                , argsImplMoneroDConfigFile
                , argsImplMoneroDService
                } = do
  argsMoneroDLogFile <- case parseOnly absFilePath (T.pack argsImplMoneroDLogFile) of
    Left e -> throw (MoneroDLogFileParseFailure e)
    Right x -> pure x
  argsMoneroDConfigFile <- case parseOnly absFilePath (T.pack argsImplMoneroDConfigFile) of
    Left e -> throw (MoneroDConfigFileParseFailure e)
    Right x -> pure x
  pure Args
    { argsMoneroDLogFile
    , argsMoneroDConfigFile
    , argsMoneroDService = argsImplMoneroDService
    }


mkEnv :: Args -> IO Env
mkEnv Args{ argsMoneroDLogFile
          , argsMoneroDConfigFile
          , argsMoneroDService
          } = do
  envClient <- connectSession
  envINotify <- initINotify
  pure Env
    { envClient
    , envMoneroDLogFile = argsMoneroDLogFile
    , envMoneroDConfigFile = argsMoneroDConfigFile
    , envMoneroDService = argsMoneroDService
    , envINotify
    }
