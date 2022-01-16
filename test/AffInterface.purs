module Test.AffInterface where

import Prelude
import Control.Monad.Reader.Trans (runReaderT)
import TestM (TestPlanM)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Mote (bracket, group, test)
import Ogmios (QueryM, mkOgmiosWebSocketAff, discardOgmiosWebSocket, utxosAt, txQuery)

testnet_addr :: String
testnet_addr = 
  "addr_test1qr7g8nrv76fc7k4ueqwecljxx9jfwvsgawhl55hck3n8uwaz26mpcwu58zdkhpdnc6nuq3fa8vylc8ak9qvns7r2dsysp7ll4d"

txid :: String
txid = "f24a9c175f4042f4e92c4c392ee00dd599b5fbf6aef21857e83f72598a5c45ad"

serverURL :: String
serverURL = "ws:127.0.0.1:1337"


-- note: currently this suite relies on Ogmios being open and running against the
-- testnet, and does not directly test outputs, as this suite is intended to 
-- help verify that the Aff interface for websockets itself works, 
-- not that the data represents expected values, as that would depend on chain 
-- state, and ogmios itself.
suite :: TestPlanM Unit
suite = do
  group "Aff Interface" $ do
    test "utxosAt" $ do
      runQueryM $ utxosAt testnet_addr
    test "txQuery" $ do
      runQueryM $ txQuery txid

runQueryM 
  :: forall a
   . QueryM a
  -> Aff Unit
runQueryM qma = do
  ws <- mkOgmiosWebSocketAff serverURL
  runReaderT 
    (do
      _ <- qma
      liftEffect $ discardOgmiosWebSocket ws
      pure unit
    )
    { ws }


