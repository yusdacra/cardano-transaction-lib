module BalanceTx.Monad
  ( TxBalancingState
  , TxBalancingStateM
  ) where

import BalanceTx.Error (BalanceTxError)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Logger.Trans (LoggerT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Data.BigInt (BigInt)
import Effect.Aff (Aff)
import QueryM (DefaultQueryConfig)
import Types.ScriptLookups (UnattachedUnbalancedTx)

type TxBalancingState =
  { unbalancedTx :: UnattachedUnbalancedTx
  , fees :: BigInt
  }

type TxBalancingStateM (a :: Type) =
  StateT TxBalancingState
    (ExceptT BalanceTxError (ReaderT DefaultQueryConfig (LoggerT Aff)))
    a
