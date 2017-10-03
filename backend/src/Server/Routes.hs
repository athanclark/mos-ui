{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Server.Routes where

import Server.Templates (template)
import Server.Assets (frontend)
import Types (MonadApp)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import Web.Routes.Nested (RouterT, match, matchGroup, matchHere, matchGroup, action, get, lucid, bytestring, l_, (</>), o_)
import Network.Wai.Trans (MiddlewareT)
import Network.Wai.Middleware.ContentType (FileExt (JavaScript))


routes :: MonadApp m => RouterT (MiddlewareT m) sec m ()
routes = do
  matchHere $ action $ get $ lucid $ template ()

  -- Assets
  matchGroup (l_ "static" </> o_) $
    matchOn JavaScript "frontend" $ LBS.fromStrict frontend


matchOn :: MonadApp m
        => FileExt        -- ^ file extension
        -> Text         -- ^ filename
        -> LBS.ByteString -- ^ file content
        -> RouterT (MiddlewareT m) sec m ()
matchOn e f = match (l_ f </> o_) . action . get . bytestring e
