{-# LANGUAGE
    TemplateHaskell
  #-}

module Server.Assets where

import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)


frontend :: ByteString
frontend = $(embedFile "frontend/index.js")
