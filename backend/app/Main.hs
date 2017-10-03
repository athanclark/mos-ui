{-# LANGUAGE
    NamedFieldPuns
  #-}

module Main where

import Main.Options (Args (..), args, makeEnv)
import Types (runAppM)
import Types.Env (Env (..))
import Server (mainApp)
import Data.Monoid ((<>))
import Data.URI.Auth (URIAuth (..))
import qualified Data.Strict.Maybe as S
import Options.Applicative (helper, info, fullDesc, header, progDesc, execParser)
import Network.Wai.Trans (runApplicationT)
import Network.Wai.Handler.Warp (runEnv)


main :: IO ()
main = do
  let prog = mconcat
        [ fullDesc
        , header "mos-ui - user-interface for Monerodo OS"
        , progDesc "run server over HOST as PORT"
        ]
  (env,port) <- makeEnv =<< execParser (info (helper <*> args) prog)

  runEnv (fromIntegral port) (runApplicationT (runAppM env) mainApp)
