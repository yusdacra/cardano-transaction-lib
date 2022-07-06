module BalanceTx.Error
  ( BalanceTxError(..)
  , Actual(..)
  , BalanceNonAdaOutsError(..)
  , BalanceTxInsError(..)
  , CalculateMinFeeError(..)
  , CannotMinusError(..)
  , CollectTxInsError(..)
  , Expected(..)
  , GetPublicKeyTransactionInputError(..)
  , ImpossibleError(..)
  , ReturnAdaChangeError(..)
  ) where

import Cardano.Types.Value (Value)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import QueryM (ClientError)
import ReindexRedeemers (ReindexErrors)

-- These derivations may need tweaking when testing to make sure they are easy
-- to read, especially with generic show vs newtype show derivations.
data BalanceTxError
  = BalanceNonAdaOutsError' BalanceNonAdaOutsError
  | BalanceTxInsError' BalanceTxInsError
  | CalculateMinFeeError' CalculateMinFeeError
  | CouldNotGetCollateralError
  | CouldNotGetUtxosError
  | CouldNotGetWalletAddressError
  | GetPublicKeyTransactionInputError' GetPublicKeyTransactionInputError
  | ReturnAdaChangeError' ReturnAdaChangeError

derive instance Generic BalanceTxError _

instance Show BalanceTxError where
  show = genericShow

--------------------------------------------------------------------------------
-- BalanceNonAdaOutsError
--------------------------------------------------------------------------------

data BalanceNonAdaOutsError
  = InputsCannotBalanceNonAdaTokens
  | BalanceNonAdaOutsCannotMinus CannotMinusError

derive instance Generic BalanceNonAdaOutsError _

instance Show BalanceNonAdaOutsError where
  show = genericShow

--------------------------------------------------------------------------------
-- BalanceTxInsError
--------------------------------------------------------------------------------

data BalanceTxInsError
  = InsufficientTxInputs Expected Actual
  | BalanceTxInsCannotMinus CannotMinusError

derive instance Generic BalanceTxInsError _

instance Show BalanceTxInsError where
  show = genericShow

--------------------------------------------------------------------------------
-- CalculateMinFeeError
--------------------------------------------------------------------------------

data CalculateMinFeeError
  = CalculateMinFeeError ClientError
  | ReindexRedeemersError ReindexErrors

derive instance Generic CalculateMinFeeError _

instance Show CalculateMinFeeError where
  show = genericShow

--------------------------------------------------------------------------------
-- GetPublicKeyTransactionInputError
--------------------------------------------------------------------------------

data GetPublicKeyTransactionInputError = CannotConvertScriptOutputToTxInput

derive instance Generic GetPublicKeyTransactionInputError _

instance Show GetPublicKeyTransactionInputError where
  show = genericShow

--------------------------------------------------------------------------------
-- ReturnAdaChangeError
--------------------------------------------------------------------------------

data ReturnAdaChangeError
  = ReturnAdaChangeError String
  | ReturnAdaChangeImpossibleError String ImpossibleError
  | ReturnAdaChangeCalculateMinFee CalculateMinFeeError

derive instance Generic ReturnAdaChangeError _

instance Show ReturnAdaChangeError where
  show = genericShow

--------------------------------------------------------------------------------
-- Expected / Actual
--------------------------------------------------------------------------------

newtype Expected = Expected Value

derive instance Generic Expected _
derive instance Newtype Expected _

instance Show Expected where
  show = genericShow

newtype Actual = Actual Value

derive instance Generic Actual _
derive instance Newtype Actual _

instance Show Actual where
  show = genericShow

--------------------------------------------------------------------------------
-- CannotMinusError
--------------------------------------------------------------------------------

data CannotMinusError = CannotMinus Actual

derive instance Generic CannotMinusError _

instance Show CannotMinusError where
  show = genericShow

--------------------------------------------------------------------------------
-- CollectTxInsError
--------------------------------------------------------------------------------

data CollectTxInsError = CollectTxInsInsufficientTxInputs BalanceTxInsError

derive instance Generic CollectTxInsError _

instance Show CollectTxInsError where
  show = genericShow

--------------------------------------------------------------------------------
-- ImpossibleError
--------------------------------------------------------------------------------

-- | Represents that an error reason should be impossible
data ImpossibleError = Impossible

derive instance Generic ImpossibleError _

instance Show ImpossibleError where
  show = genericShow
