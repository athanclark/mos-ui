module Types.Env where

import Data.URI.Auth (URIAuth)


data Env = Env
  { envHost :: URIAuth
  }
