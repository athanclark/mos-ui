{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Server where

import Server.Routes (routes)
import Types (MonadApp)

import Network.Wai.Middleware.ContentType (textOnly)
import Network.Wai.Trans (ApplicationT)
import Network.HTTP.Types (status404)
import Web.Routes.Nested (route)


mainApp :: MonadApp m => ApplicationT m
mainApp = route routes defApp


defApp :: MonadApp m => ApplicationT m
defApp req resp = resp $ textOnly "404!" status404 []
