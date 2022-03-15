module Test.UsedTxOuts (suite) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array (any, singleton, uncons)
import Data.Foldable (all)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.UInt (UInt)
import Effect.Class.Console (log)
import Mote (test, group, only)
import Partial.Unsafe (unsafePartial)
import Test.Fixtures
  ( mkSampleTx
  , mkTxInput
  , txFixture1
  )
import Test.Spec.Assertions (shouldReturn)
import TestM (TestPlanM)
import Types.Transaction (Transaction, TransactionHash)
import UsedTxOuts
  ( isTxOutRefUsed
  , lockTransactionInputs
  , newUsedTxOuts
  , unlockTransactionInputs
  , unlockTxOutRefs
  )

buildSampleTransaction
  :: { tx :: Transaction
     , usedTxOutRefs :: Array { transaction_id :: TransactionHash, index :: UInt }
     , unusedTxOutRefs :: Array { transaction_id :: TransactionHash, index :: UInt }
     }
buildSampleTransaction =
  let
    usedTxOutRefs =
      [ mkTxInput { txId: "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", ix: 0 }
      , mkTxInput { txId: "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", ix: 1 }
      , mkTxInput { txId: "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d4c9754cc", ix: 0 }
      , mkTxInput { txId: "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d4c9754cc", ix: 1 }
      , mkTxInput { txId: "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d4c9754cc", ix: 2 }
      , mkTxInput { txId: "c9e9c15d4f16a7948d3736c93aa79034621d51dccc4df5d31c7d34aa0b3240cc", ix: 1 }
      ]
    unusedTxOutRefs =
      [ mkTxInput { txId: "a69fdfbdab33eeb05143bf3b05591679fb104157fa68fe3f4268631aedf5e607", ix: 0 }
      , mkTxInput { txId: "a69fdfbdab33eeb05143bf3b05591679fb104157fa68fe3f4268631aedf5e607", ix: 1 }
      , mkTxInput { txId: "a2cb07ae236c4d8ee549d1bf3ad3818d691a5a7c53c1065276bfbb0ac332ccd7", ix: 0 }
      ]
  in
    { tx: mkSampleTx txFixture1 (_ { inputs = usedTxOutRefs })
    , usedTxOutRefs: unwrap <$> usedTxOutRefs
    , unusedTxOutRefs: unwrap <$> unusedTxOutRefs
    }

suite :: TestPlanM Unit
suite =
  only $ group "UsedTxOuts api tests" do

    let
      { tx, usedTxOutRefs, unusedTxOutRefs } = buildSampleTransaction
      anyTxOutsLocked txos = runReaderT (any identity <$> traverse isTxOutRefUsed txos)
      allTxOutsLocked txos = runReaderT (all identity <$> traverse isTxOutRefUsed txos)

    group "UsedTxOuts cache properly locks and unlocks tx txouts" $ do
      newUsedTxOuts >>= \outs -> do
        -- starts empty
        test "TxOuts should start unlocked" do
          anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) outs `shouldReturn` false

          -- lock
          runReaderT (lockTransactionInputs tx) outs
        test "All usedTxOuts should be now locked" do
          allTxOutsLocked usedTxOutRefs outs `shouldReturn` true
        test "None of the unused should be locked" do
          anyTxOutsLocked unusedTxOutRefs outs `shouldReturn` false

          -- unlock
          runReaderT (unlockTransactionInputs tx) outs
        test "All TxOuts should be now unlocked" do
          anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) outs `shouldReturn` false

    group "UsedTxOuts cache properly locks and unlock selected txouts" $ do
      newUsedTxOuts >>= \outs -> do
        -- starts empty
        test "TxOuts should start unlocked" do
          anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) outs `shouldReturn` false

        -- lock
          runReaderT (lockTransactionInputs tx) outs
        test "All usedTxOuts should be now locked" do
          allTxOutsLocked usedTxOutRefs outs `shouldReturn` true
        test "None of the unused should be locked" do
          anyTxOutsLocked unusedTxOutRefs outs `shouldReturn` false

        -- unlock unused
          runReaderT (unlockTxOutRefs unusedTxOutRefs) outs
        test "All usedTxOuts should be now locked" do
          allTxOutsLocked usedTxOutRefs outs `shouldReturn` true
        test "None of the unused should be locked" do
          anyTxOutsLocked unusedTxOutRefs outs `shouldReturn` false

        -- unlock used
          runReaderT (unlockTxOutRefs usedTxOutRefs) outs
        test "All txouts should be now unlocked" do
          anyTxOutsLocked (usedTxOutRefs <> unusedTxOutRefs) outs `shouldReturn` false

        -- lock
          runReaderT (lockTransactionInputs tx) outs
        test "All usedTxOuts should be now locked" do
          allTxOutsLocked usedTxOutRefs outs `shouldReturn` true

        -- unlock 'head'
        let { head, tail } = unsafePartial $ fromJust (uncons usedTxOutRefs)
        -- head unlocked
        test "Head should be unlocked" do
          runReaderT (unlockTxOutRefs $ singleton head) outs
          runReaderT (isTxOutRefUsed head) outs `shouldReturn` false
        -- tail remains locked
        test "Trail should be locked" do
          allTxOutsLocked tail outs `shouldReturn` true
        test "Unused should be unlocked" do
          anyTxOutsLocked unusedTxOutRefs outs `shouldReturn` false
