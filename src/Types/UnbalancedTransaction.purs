module Types.UnbalancedTransaction
  ( PaymentPubKey(..)
  , PaymentPubKeyHash(..)
  , PubKey(..)
  , PubKeyHash(..)
  , ScriptOutput(..)
  , StakeKeyHash(..)
  , StakePubKeyHash(..)
  , TxOutRef(..)
  , UnbalancedTx(..)
  , emptyUnbalancedTx
  , payPubKeyHash
  , pubKeyHash
  , scriptOutputToTxOutput
  , utxoIndexToUtxo
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map, empty)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)
import Serialization.Address (addressFromBytes)
import Serialization.Hash
  ( Ed25519KeyHash
  , ed25519KeyHashFromBytes
  , scriptHashToBytes
  )
import Types.ByteArray (ByteArray(ByteArray))
import Types.PlutusData (DatumHash)
import Types.Scripts (ValidatorHash)
import Types.Transaction
  ( Transaction
  , TransactionInput
  , TransactionOutput
  , Utxo
  )
import Types.Value (Value)

newtype PubKey = PubKey ByteArray

derive instance Generic PubKey _
derive instance Newtype PubKey _
derive newtype instance Eq PubKey

instance Show PubKey where
  show = genericShow

newtype PaymentPubKey = PaymentPubKey PubKey

derive instance Generic PaymentPubKey _
derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey

instance Show PaymentPubKey where
  show = genericShow

newtype ScriptOutput = ScriptOutput
  { validatorHash :: ValidatorHash
  , value :: Value
  , datumHash :: DatumHash
  }

derive instance Newtype ScriptOutput _

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance Ord PubKeyHash

instance Show PubKeyHash where
  show = genericShow

payPubKeyHash :: PaymentPubKey -> PaymentPubKeyHash
payPubKeyHash (PaymentPubKey pk) = pubKeyHash pk

-- Is this safe? If we start with a PubKey, we should be guaranteed a hash
-- (even if via ByteArray)
pubKeyHash :: PubKey -> PaymentPubKeyHash
pubKeyHash (PubKey bytes) =
  wrap $ wrap $ unsafePartial fromJust $ ed25519KeyHashFromBytes bytes

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show = genericShow

newtype StakeKeyHash = StakeKeyHash Ed25519KeyHash

derive instance Generic StakeKeyHash _
derive instance Newtype StakeKeyHash _
derive newtype instance Eq StakeKeyHash
derive newtype instance Ord StakeKeyHash

instance Show StakeKeyHash where
  show = genericShow

newtype StakePubKeyHash = StakePubKeyHash StakeKeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance Ord StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow

-- Use Plutus' name to assist with copy & paste from Haskell to Purescript.
-- | Transaction inputs reference some other transaction's outputs.
type TxOutRef = TransactionInput

-- | An unbalanced transaction. It needs to be balanced and signed before it
-- | can be submitted to the ledeger.
-- | Resembles `UnbalancedTx` from `plutus-apps`.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction
  , utxoIndex :: Map TxOutRef ScriptOutput
  }

derive instance Newtype UnbalancedTx _

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedTx { transaction: mempty, utxoIndex: empty }

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/src/Ledger.Constraints.OffChain.html#fromScriptOutput
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/src/Ledger.Tx.html#toTxOut
-- Combining these two into one function skipping Chain Index
-- | Converts a `ScriptOutput` to a `TransactionOutput` with potential failure
scriptOutputToTxOutput :: ScriptOutput -> Maybe TransactionOutput
scriptOutputToTxOutput (ScriptOutput { validatorHash, value, datumHash }) = do
  address <- validatorHash # unwrap # scriptHashToBytes >>> addressFromBytes
  pure $ wrap { address, amount: value, data_hash: pure datumHash }

-- | Converts a utxoIndex from `UnbalancedTx` to `Utxo` with potential failure
utxoIndexToUtxo :: Map TxOutRef ScriptOutput -> Maybe Utxo
utxoIndexToUtxo = map scriptOutputToTxOutput >>> sequence
