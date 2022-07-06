module BalanceTx.Internal.Monad
  ( TxBalancingState
  , TxBalancingStateM
  , coinsPerUtxoByte
  , costModels
  , logTx
  , _fees
  , _redeemersTxIns
  , _transaction
  , _txBody
  , _unattachedUnbalancedTx
  , _unbalancedTx
  ) where

import Prelude

import BalanceTx.Internal.Error (BalanceTxError)
import Cardano.Types.Transaction
  ( Costmdls
  , Redeemer
  , Transaction
  , TxBody
  , Utxo
  )
import Cardano.Types.Transaction (_body) as Transaction
import Cardano.Types.Value (Coin)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Logger.Class (trace) as Logger
import Control.Monad.Logger.Trans (LoggerT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT, get)
import Data.Array (foldMap) as Array
import Data.BigInt (BigInt)
import Data.Traversable (traverse_)
import Data.Lens (Lens', lens')
import Data.Lens.Getter ((^.), use)
import Data.Lens.Record (prop) as Lens
import Data.Log.Tag (tag)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import QueryM (DefaultQueryConfig)
import Type.Proxy (Proxy(Proxy))
import Types.ScriptLookups (UnattachedUnbalancedTx(UnattachedUnbalancedTx))
import Types.Transaction (TransactionInput)
import Types.UnbalancedTransaction (UnbalancedTx)
import Types.UnbalancedTransaction (_transaction) as UnbalancedTransaction

type TxBalancingState =
  { transaction :: UnattachedUnbalancedTx
  , fees :: BigInt
  }

type TxBalancingStateM (a :: Type) =
  StateT TxBalancingState
    (ExceptT BalanceTxError (ReaderT DefaultQueryConfig (LoggerT Aff)))
    a

--------------------------------------------------------------------------------
-- PParams Getters
--------------------------------------------------------------------------------

coinsPerUtxoByte :: TxBalancingStateM Coin
coinsPerUtxoByte = _.coinsPerUtxoByte <<< unwrap <$> asks _.pparams

costModels :: TxBalancingStateM Costmdls
costModels = _.costModels <<< unwrap <$> asks _.pparams

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

-- | Logs a `Transaction` without returning it.
logTx :: String -> Utxo -> TxBalancingStateM Unit
logTx msg utxos = get >>= \state -> do
  let body = get state ^. _txBody
  traverse_ (Logger.trace (tag msg mempty))
    [ "Input Value: " <> show (Utils.getInputValue utxos body)
    , "Output Value: " <> show (Array.foldMap utxos body)
    , "Fees: " <> show (state.fees)
    ]

--------------------------------------------------------------------------------
-- `TxBalancingState` Lenses
--------------------------------------------------------------------------------

_fees :: Lens' TxBalancingState BigInt
_fees = Lens.prop (Proxy :: Proxy "fees")

_unattachedUnbalancedTx :: Lens' TxBalancingState UnattachedUnbalancedTx
_unattachedUnbalancedTx = Lens.prop (Proxy :: Proxy "transaction")

_unbalancedTx :: Lens' TxBalancingState UnbalancedTx
_unbalancedTx = _unattachedUnbalancedTx <<<
  lens' \(UnattachedUnbalancedTx rec@{ unbalancedTx }) ->
    unbalancedTx /\
      \ubTx -> UnattachedUnbalancedTx rec { unbalancedTx = ubTx }

_transaction :: Lens' TxBalancingState Transaction
_transaction = _unbalancedTx <<< UnbalancedTransaction._transaction

_txBody :: Lens' TxBalancingState TxBody
_txBody = _transaction <<< Transaction._body

_redeemersTxIns
  :: Lens' TxBalancingState (Array (Redeemer /\ Maybe TransactionInput))
_redeemersTxIns = _unattachedUnbalancedTx <<<
  lens' \(UnattachedUnbalancedTx rec@{ redeemersTxIns }) ->
    redeemersTxIns /\
      \rdmrs -> UnattachedUnbalancedTx rec { redeemersTxIns = rdmrs }
