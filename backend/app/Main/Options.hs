{-# LANGUAGE
    NamedFieldPuns
  #-}

module Main.Options where

import Types.Env (Env (..))
import Data.Monoid ((<>))
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.URI.Auth (URIAuth (..), parseURIAuth)
import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Strict.Maybe as S
import Options.Applicative (Parser, strOption, option, long, short, help, value, showDefault, metavar, auto)
import Control.Exception (ioError)
import System.IO.Error (userError)


data Args = Args
  { argsHost :: String
  , argsBound :: Word16
  -- , argsTls :: Bool
  }

args :: Parser Args
args = Args <$> strOption hostMod
            <*> option auto boundMod
  where
    hostMod = long "host"
           <> help "The hostname and port to bind to"
           <> value "localhost:3000"
           <> showDefault
           <> metavar "HOST"
    boundMod = long "bound-port"
            <> short 'b'
            <> help "The port exposed to the outside world, if different from the bound port"
            <> value 3000
            <> showDefault
            <> metavar "PORT"
    -- tlsMod = long "tls"
    --       <> help "Assume the proxy to the outside world is behind an ssl connection"


makeEnv :: Args -> IO (Env, Int)
makeEnv Args{argsHost,argsBound} = do
  URIAuth mu h mp <- case parseOnly (parseURIAuth <* endOfInput) (T.pack argsHost) of
    Left e -> ioError (userError e)
    Right h -> pure h
  pure
    ( Env
        { envHost = URIAuth mu h (S.Just argsBound)
        }
    , S.maybe 3000 fromIntegral mp
    )
