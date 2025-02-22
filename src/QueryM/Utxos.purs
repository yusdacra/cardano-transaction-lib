-- | A module for `QueryM` queries related to utxos.
module QueryM.Utxos
  ( filterUnusedUtxos
  , utxosAt
  , getWalletBalance
  ) where

import Prelude

import Address (addressToOgmiosAddress)
import Cardano.Types.Transaction (TransactionOutput, UtxoM(UtxoM))
import Cardano.Types.Value (Value)
import Control.Monad.Logger.Trans (LoggerT)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Reader.Trans (ReaderT, asks)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap, over)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Helpers as Helpers
import QueryM (QueryM, getWalletAddress, getWalletCollateral, mkOgmiosRequest)
import QueryM.Ogmios as Ogmios
import Serialization.Address (Address)
import TxOutput (ogmiosTxOutToTransactionOutput, txOutRefToTransactionInput)
import Types.Transaction (TransactionInput)
import Types.UsedTxOuts (UsedTxOuts, isTxOutRefUsed)
import Wallet (Wallet(Gero, Nami, KeyWallet))

--------------------------------------------------------------------------------
-- UtxosAt
--------------------------------------------------------------------------------

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Cardano.Transaction.Types`.
-- | Results may vary depending on `Wallet` type.
utxosAt :: Address -> QueryM (Maybe UtxoM)
utxosAt addr = asks _.wallet >>= maybe (allUtxosAt addr) (utxosAtByWallet addr)
  where
  -- Add more wallet types here:
  utxosAtByWallet :: Address -> Wallet -> QueryM (Maybe UtxoM)
  utxosAtByWallet address = case _ of
    Nami _ -> cip30UtxosAt address
    Gero _ -> cip30UtxosAt address
    KeyWallet _ -> allUtxosAt address

  -- Gets all utxos at an (internal) Address in terms of (internal)
  -- Cardano.Transaction.Types.
  allUtxosAt :: Address -> QueryM (Maybe UtxoM)
  allUtxosAt = addressToOgmiosAddress >>> getUtxos
    where
    utxosAt' :: Ogmios.OgmiosAddress -> QueryM Ogmios.UtxoQR
    utxosAt' addr' = mkOgmiosRequest Ogmios.queryUtxosAtCall _.utxo addr'

    getUtxos :: Ogmios.OgmiosAddress -> QueryM (Maybe UtxoM)
    getUtxos address = convertUtxos <$> utxosAt' address

    convertUtxos :: Ogmios.UtxoQR -> Maybe UtxoM
    convertUtxos (Ogmios.UtxoQR utxoQueryResult) =
      let
        out'
          :: Array
               ( Maybe TransactionInput /\ Maybe
                   TransactionOutput
               )
        out' = Map.toUnfoldable utxoQueryResult
          <#> bimap
            txOutRefToTransactionInput
            ogmiosTxOutToTransactionOutput

        out
          :: Maybe
               ( Array
                   ( TransactionInput /\
                       TransactionOutput
                   )
               )
        out = out' <#> bisequence # sequence
      in
        wrap <<< Map.fromFoldable <$> out

  cip30UtxosAt :: Address -> QueryM (Maybe UtxoM)
  cip30UtxosAt address = getWalletCollateral >>= maybe
    (liftEffect $ throw "CIP-30 wallet missing collateral")
    \collateral' -> do
      let collateral = unwrap collateral'
      utxos' <- allUtxosAt address
      pure (over UtxoM (Map.delete collateral.input) <$> utxos')

--------------------------------------------------------------------------------
-- Used Utxos helpers
--------------------------------------------------------------------------------

filterUnusedUtxos :: UtxoM -> QueryM UtxoM
filterUnusedUtxos (UtxoM utxos) = withTxRefsCache $
  UtxoM <$> Helpers.filterMapWithKeyM
    (\k _ -> not <$> isTxOutRefUsed (unwrap k))
    utxos

withTxRefsCache
  :: forall (m :: Type -> Type) (a :: Type)
   . ReaderT UsedTxOuts (LoggerT Aff) a
  -> QueryM a
withTxRefsCache f = withReaderT (_.usedTxOuts) f

getWalletBalance
  :: QueryM (Maybe Value)
getWalletBalance = do
  asks _.wallet >>= map join <<< traverse case _ of
    Nami wallet -> liftAff $ wallet.getBalance wallet.connection
    Gero wallet -> liftAff $ wallet.getBalance wallet.connection
    KeyWallet _ -> do
      -- Implement via `utxosAt`
      mbAddress <- getWalletAddress
      map join $ for mbAddress \address -> do
        utxosAt address <#> map
          -- Combine `Value`s
          (fold <<< map _.amount <<< map unwrap <<< Map.values <<< unwrap)
