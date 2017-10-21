module Arguments where

import Prelude
import Node.Yargs.Applicative (runY, flag, yarg)
import Node.Yargs.Setup (YargsSetup, usage, help)
import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (fold)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)


argsSetup :: YargsSetup
argsSetup = fold
  [ usage "$0 [-d]"
  , help "help" "Monerodo OS"
  ]

newtype Args = Args
  { development :: Boolean
  , monerodService :: String
  }

arg :: forall eff. Boolean -> String -> Eff eff Args
arg development monerodService =
  pure $ Args {development,monerodService}


args :: forall eff
      . Eff ( console :: CONSOLE
            , exception :: EXCEPTION
            | eff) Args
args = runY argsSetup $
  arg <$> flag "d" ["development"] (Just "Open Chrome dev tools")
      <*> yarg "monerod-service" [] (Just "monerod systemd service name") (Left "monerod.service") false
