{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module Types.Links where

import Path.Extended (ToPath (..), ToLocation (..), Abs, File, parseAbsFile, addFileExt, fromPath)


data AssetLinks
  = FrontendLink



instance ToPath AssetLinks Abs File where
  toPath FrontendLink = parseAbsFile "/static/frontend"

instance ToLocation AssetLinks Abs File where
  toLocation FrontendLink = (addFileExt "js" . fromPath) <$> toPath FrontendLink
