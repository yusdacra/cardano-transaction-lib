module Ogmios where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ReaderT, ask)
import Data.Argonaut as Json
import Data.Either(Either(..), either, isRight)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Types.JsonWsp (Address, JsonWspRequest, JsonWspResponse, QueryArgs, TxId, TxQR, UtxoQR, mkUtxosAtQuery, mkTxQuery, parseJsonWspResponse)

-- This module defines an Aff interface for Ogmios Websocket Queries
-- Since WebSockets do not define a mechanism for linking request/response
-- Or for verifying that the connection is live, those concerns are addressed 
-- here


--------------------------------------------------------------------------------
-- Websocket Basics
--------------------------------------------------------------------------------
foreign import _mkWebSocket :: Url -> Effect WebSocket 

foreign import _onWsConnect :: WebSocket -> (Effect Unit) -> Effect Unit

foreign import _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsError :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _wsSend :: WebSocket -> String -> Effect Unit

foreign import _wsClose :: WebSocket -> Effect Unit

foreign import _wsTerminate :: WebSocket -> Effect Unit

foreign import _stringify :: forall a. a -> Effect String

foreign import _wsWatch :: WebSocket -> Effect Unit -> Effect Unit

data WebSocket

type Url = String

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- when we add multiple query backends or wallets, 
-- we just need to extend this type
type QueryConfig = { ws :: OgmiosWebSocket } 

type QueryM a = ReaderT QueryConfig Aff a

-- the first query type in the QueryM/Aff interface
utxosAt :: Address -> QueryM UtxoQR
utxosAt addr = do
  let 
    getListenerSet = (_.utxo <<< listeners <<< _.ws) <$> ask
    mkQuery = (\addr' -> mkUtxosAtQuery { utxo: [ addr' ] })
  setupQueryDetails getListenerSet mkQuery addr

txQuery :: TxId -> QueryM TxQR
txQuery txid = do
  let 
    getListenerSet = (_.tx <<< listeners <<< _.ws) <$> ask
    mkQuery = (\txid' -> mkTxQuery { transaction: [ txid' ] })
  setupQueryDetails getListenerSet mkQuery txid

setupQueryDetails
  :: forall a b c
   . QueryM (ListenerSet b)
  -> (a -> Effect (JsonWspRequest (QueryArgs c)))
  -> a
  -> QueryM b
setupQueryDetails getListenerSet mkQuery arg = do
  body <- liftEffect $ mkQuery arg
  let id = body.mirror.id
  sBody <- liftEffect $ _stringify body
  lsForQuery <- getListenerSet
  config <- ask
  let 
    affFunc :: (Either Error b -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ws = underlyingWebSocket config.ws
      lsForQuery.addMessageListener id
        (\result -> do
          lsForQuery.removeMessageListener id
          (allowError cont) $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ lsForQuery.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc

allowError :: forall a. (Either Error a -> Effect Unit) -> a -> Effect Unit
allowError func = func <<< Right

--------------------------------------------------------------------------------
-- OgmiosWebSocket Setup and PrimOps
--------------------------------------------------------------------------------

-- don't export this constructor
-- type-safe websocket which has automated req/res dispatch and websocket
-- failure handling
data OgmiosWebSocket = OgmiosWebSocket WebSocket Listeners

-- smart-constructor for OgmiosWebSocket in Aff Context
-- (prevents sending messages before the websocket opens, etc)
mkOgmiosWebSocket' 
  :: Url 
  -> (Either Error OgmiosWebSocket -> Effect Unit) 
  -> Effect Canceler
mkOgmiosWebSocket' url cb = do
  -- yes, you need to do this for every query type...
  utxoQueryDispatchIdMap <- createMutableDispatch 
  txQueryDispatchIdMap <- createMutableDispatch 
  let md = (messageDispatch utxoQueryDispatchIdMap txQueryDispatchIdMap)
  ws <- _mkWebSocket url
  _onWsConnect ws $ do
    _wsWatch ws (removeAllListeners utxoQueryDispatchIdMap txQueryDispatchIdMap)
    _onWsMessage ws (defaultMessageListener md)
    _onWsError ws defaultErrorListener
    cb $ Right $ OgmiosWebSocket ws 
      { utxo: mkListenerSet utxoQueryDispatchIdMap 
      , tx: mkListenerSet txQueryDispatchIdMap
      }
  pure $ Canceler $ \err -> liftEffect $ cb $ Left $ err

-- smart destructor for ogmios websocket.
discardOgmiosWebSocket :: OgmiosWebSocket -> Effect Unit
discardOgmiosWebSocket (OgmiosWebSocket ws ls) = do
  removeAllListeners ls.utxo.dispatchIdMap ls.tx.dispatchIdMap
  _wsTerminate ws

mkOgmiosWebSocketAff :: Url -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff url = makeAff (mkOgmiosWebSocket' url)

-- getter
underlyingWebSocket :: OgmiosWebSocket -> WebSocket
underlyingWebSocket (OgmiosWebSocket ws _) = ws

-- getter
listeners :: OgmiosWebSocket -> Listeners
listeners (OgmiosWebSocket _ ls) = ls

-- interface required for adding/removing listeners
type Listeners = 
  { utxo :: ListenerSet UtxoQR
  , tx :: ListenerSet TxQR
  }

-- convenience type for adding additional query types later
type ListenerSet a =
  { addMessageListener :: String -> (a -> Effect Unit) -> Effect Unit
  , removeMessageListener :: String -> Effect Unit
  , dispatchIdMap :: DispatchIdMap a
  }

-- we manipluate closures to make the DispatchIdMap updateable using these
-- methods, this can be picked up by a query or cancellation function
mkListenerSet :: forall a. DispatchIdMap a -> ListenerSet a
mkListenerSet dim =
  { addMessageListener: 
    \id -> \func -> do
      idMap <- Ref.read dim
      Ref.write (Map.insert id func idMap) dim
  , removeMessageListener:
    \id -> do
      idMap <- Ref.read dim
      Ref.write (Map.delete id idMap) dim
  , dispatchIdMap: dim
  }

removeAllListeners 
  :: DispatchIdMap UtxoQR 
  -> DispatchIdMap TxQR
  -> Effect Unit
removeAllListeners dimUtxo dimTx = do
  log "error hit, removing all listeners"
  -- yes, you need to do this for each of them.
  Ref.write Map.empty dimUtxo
  Ref.write Map.empty dimTx

-------------------------------------------------------------------------------
-- Dispatch Setup
--------------------------------------------------------------------------------

-- A function which accepts some unparsed Json, and checks it against one or 
-- more possible types to perform an appropriate effect (such as supplying the 
-- parsed result to an async fiber/Aff listener)
type WebsocketDispatch = String -> Effect (Either Json.JsonDecodeError (Effect Unit))

-- A mutable queue of requests
type DispatchIdMap a = Ref.Ref (Map.Map String (a -> Effect Unit))
  
-- an immutable queue of response type handlers
-- yes, you need to do this for each query type
messageDispatch 
  :: DispatchIdMap UtxoQR 
  -> DispatchIdMap TxQR
  -> Array WebsocketDispatch
messageDispatch dimUtxo dimTx = 
  [ utxoQueryDispatch dimUtxo
  , txQueryDispatch dimTx
  ]

-- each query type will have a corresponding ref that lives in ReaderT config or similar
-- for utxoQueryDispatch, the `a` parameter will be `UtxoQR` or similar
-- the add and remove listener functions will know to grab the correct mutable dispatch, if one exists.
createMutableDispatch :: forall a. Effect (DispatchIdMap a)
createMutableDispatch = Ref.new Map.empty

-- we provide monomorphic query dispatch just for improved type 
-- safety/type inference
utxoQueryDispatch 
  :: Ref.Ref (Map.Map String (UtxoQR -> Effect Unit))
  -> String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
utxoQueryDispatch = polymorphicQueryDispatch

txQueryDispatch
  :: Ref.Ref (Map.Map String (TxQR -> Effect Unit))
  -> String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
txQueryDispatch = polymorphicQueryDispatch

-- we parse out the utxo query result, then check if we're expecting a result
-- with the provided id, if we are then we dispatch to the effect that is
-- waiting on this result
polymorphicQueryDispatch 
  :: forall a
   . Json.DecodeJson a
  => Ref.Ref (Map.Map String (a -> Effect Unit))
  -> String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
polymorphicQueryDispatch ref str = do
  let parsed' = parseJsonWspResponse =<< Json.parseJson str 
  case parsed' of
      (Left err) -> pure $ Left err
      (Right res) -> afterParse res 
  where
    afterParse 
      :: JsonWspResponse a 
      -> Effect (Either Json.JsonDecodeError (Effect Unit))
    afterParse parsed = do
      let (id :: String) = parsed.reflection.id
      idMap <- Ref.read ref
      let (mAction :: Maybe (a -> Effect Unit)) 
              = (Map.lookup id idMap)
      case mAction of
        Nothing -> pure $ (Left (Json.TypeMismatch ("Parse succeeded but Request Id: " <> id <> " has been cancelled")) :: Either Json.JsonDecodeError (Effect Unit))
        Just action -> pure $ Right $ action parsed.result

-- an empty error we can compare to, useful for ensuring we've not received any other kind of error
defaultErr :: Json.JsonDecodeError
defaultErr = Json.TypeMismatch "default error"

-- For now, we just throw this error, if we find error types that can be linked
-- to request Id's, then we should run a similar dispatch and throw within the
-- appropriate Aff handler
defaultErrorListener :: String -> Effect Unit
defaultErrorListener str = 
  throwError $ error $ "a WebSocket Error has occured: " <> str

defaultMessageListener :: Array WebsocketDispatch -> String -> Effect Unit
defaultMessageListener dispatchArray msg = do
  -- here, we need to fold the input over the array of functions until we get 
  -- a success, then execute the effect.
  -- using a fold instead of a traverse allows us to skip a bunch of execution
  eAction :: Either Json.JsonDecodeError (Effect Unit) <- foldl (messageFoldF msg) (pure $ Left defaultErr) dispatchArray
  either 
    -- we expect a lot of parse errors, some messages could? fall through completely
    (\err -> if err == defaultErr then pure unit else log ("unexpected parse error on input:" <> msg))
    (\act -> act)
    (eAction :: Either Json.JsonDecodeError (Effect Unit))

messageFoldF 
  :: String 
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
  -> (String -> (Effect (Either Json.JsonDecodeError (Effect Unit))))
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
messageFoldF msg acc' func = do
  acc <- acc'
  if isRight acc then acc' else func msg
