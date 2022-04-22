module Seabug
  ( module Seabug.CallContract
  , connectWallet
  , getWalletLovelace
  )
  where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import QueryM (callNami)
import Seabug.CallContract
import Serialization.Types (Value)
import Types.Value (valueToCoin')
import Wallet (Wallet(..), mkNamiWalletAff)

connectWallet :: Effect (Promise Wallet)
connectWallet = fromAff mkNamiWalletAff

getWalletLovelace :: Effect (Promise BigInt)
getWalletLovelace = fromAff $ do
  (Nami nami) <- mkNamiWalletAff
  balance <- callNami nami _.getBalance
  case balance of 
    Just bal -> pure $ valueToCoin' bal
    Nothing -> pure Nothing

