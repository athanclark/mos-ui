module Arguments where

import Prelude
import Node.Yargs.Applicative (runY, flag)
import Node.Yargs.Setup (YargsSetup, usage, help)
import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (fold)
import Data.Maybe (Maybe (..))
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
  }

arg :: forall eff. Boolean -> Eff eff Args
arg development = pure $ Args {development}


args :: forall eff
      . Eff ( console :: CONSOLE
            , exception :: EXCEPTION
            | eff) Args
args = runY argsSetup $
  arg <$> flag "d" ["development"] (Just "Open Chrome dev tools")
