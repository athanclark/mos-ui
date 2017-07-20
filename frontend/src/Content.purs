module Content where

import Content.Node as Node
import Content.Mining as Mining
import Content.Pool as Pool
import Content.Wallet as Wallet
import Content.Explorer as Explorer
import Content.Payment as Payment
import Template (Showable (ShowSidebar), _mkShow, _Showable)

import Prelude

import Data.Lens.Prism (Prism', prism')
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe (..))
import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM


data State
  = NodeState Node.State
  | MiningState Mining.State
  | PoolState Pool.State
  | WalletState Wallet.State
  | ExplorerState Explorer.State
  | PaymentState Payment.State

_node :: Prism' State Node.State
_node = prism' NodeState $ case _ of
  NodeState x -> Just x
  _           -> Nothing

_mining :: Prism' State Mining.State
_mining = prism' MiningState $ case _ of
  MiningState x -> Just x
  _           -> Nothing

_pool :: Prism' State Pool.State
_pool = prism' PoolState $ case _ of
  PoolState x -> Just x
  _           -> Nothing

_wallet :: Prism' State Wallet.State
_wallet = prism' WalletState $ case _ of
  WalletState x -> Just x
  _           -> Nothing

_explorer :: Prism' State Explorer.State
_explorer = prism' ExplorerState $ case _ of
  ExplorerState x -> Just x
  _           -> Nothing

_payment :: Prism' State Payment.State
_payment = prism' PaymentState $ case _ of
  PaymentState x -> Just x
  _           -> Nothing


data Action
  = NodeAction Node.Action
  | MiningAction Mining.Action
  | PoolAction Pool.Action
  | WalletAction Wallet.Action
  | ExplorerAction Explorer.Action
  | PaymentAction Payment.Action

_Node :: Prism' Action Node.Action
_Node = prism' NodeAction $ case _ of
  NodeAction x -> Just x
  _            -> Nothing

_Mining :: Prism' Action Mining.Action
_Mining = prism' MiningAction $ case _ of
  MiningAction x -> Just x
  _           -> Nothing

_Pool :: Prism' Action Pool.Action
_Pool = prism' PoolAction $ case _ of
  PoolAction x -> Just x
  _           -> Nothing

_Wallet :: Prism' Action Wallet.Action
_Wallet = prism' WalletAction $ case _ of
  WalletAction x -> Just x
  _           -> Nothing

_Explorer :: Prism' Action Explorer.Action
_Explorer = prism' ExplorerAction $ case _ of
  ExplorerAction x -> Just x
  _           -> Nothing

_Payment :: Prism' Action Payment.Action
_Payment = prism' PaymentAction $ case _ of
  PaymentAction x -> Just x
  _           -> Nothing


spec :: T.Spec _ State _ (Showable Action)
spec = T.simpleSpec performAction render
  where
    nodeSpec     = T.split _node     $ T.match (_mkShow _Node     <<< _Showable)     Node.spec
    miningSpec   = T.split _mining   $ T.match (_mkShow _Mining   <<< _Showable)   Mining.spec
    poolSpec     = T.split _pool     $ T.match (_mkShow _Pool     <<< _Showable)     Pool.spec
    walletSpec   = T.split _wallet   $ T.match (_mkShow _Wallet   <<< _Showable)   Wallet.spec
    explorerSpec = T.split _explorer $ T.match (_mkShow _Explorer <<< _Showable) Explorer.spec
    paymentSpec  = T.split _payment  $ T.match (_mkShow _Payment  <<< _Showable)  Payment.spec

    performAction :: T.PerformAction _ State _ (Showable Action)
    performAction =
      (nodeSpec <> miningSpec <> poolSpec <> walletSpec <> explorerSpec <> paymentSpec) ^. T._performAction

    render :: T.Render State _ (Showable Action)
    render dispatch props state children =
      ((nodeSpec <> miningSpec <> poolSpec <> walletSpec <> explorerSpec <> paymentSpec) ^. T._render) dispatch props state children
