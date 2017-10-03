{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  , OverloadedStrings
  , NamedFieldPuns
  #-}

module Server.Templates where

import Types (MonadApp)
import Types.Env (Env (..))
import Types.Links (AssetLinks (FrontendLink))
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Strict.Maybe as S
import qualified Data.Strict.Tuple as S
import Data.Markup (deploy, JavaScript (..), Remote (..))
import Data.URI (URI (..))
import Data.Url (runAbsoluteUrlT)
import Data.List.Split (splitOn)
import Control.Monad.Trans (lift)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (ask)
import Lucid (HtmlT, Attribute, ToHtml, toHtml)
import Path.Extended (toLocation, getQuery, getFragment)
import Web.Page.Types (WebPage (..))
import qualified Web.Page.Lucid as WebPage



template :: forall m a. (MonadApp m, ToHtml m a) => a -> HtmlT m ()
template content =
  let page = (def :: WebPage (HtmlT m ()) Text [Attribute])
                { pageTitle = "MOS UI"
                , bodyScripts = do
                  Env{envHost} <- ask
                  let mkUriLoc loc =
                        URI (S.Just "http")
                          True
                          envHost
                          (V.fromList $ fmap T.pack $ splitOn "/" $ dropWhile (== '/') $ show loc)
                          ( V.fromList $
                              map (\(l,r) ->
                                    T.pack l S.:!:
                                    maybe S.Nothing (S.Just . T.pack) r)
                              (getQuery loc)
                          )
                          (maybe S.Nothing (S.Just . T.pack) (getFragment loc))
                  hoist (`runAbsoluteUrlT` mkUriLoc) $
                    deploy JavaScript Remote =<< lift (toLocation FrontendLink)
                }
  in  WebPage.template page (toHtml content)
