const WebSocket = require("ws");

// _mkWebsocket :: String -> Effect WebSocket
exports._mkWebSocket = url => () => {
  const ws = new WebSocket(url, { perMessageDeflate: false });
  console.log("new websocket");
  return ws;
}

// _onWsConnect :: WebSocket -> (Unit -> Effect Unit) -> Effect Unit
exports._onWsConnect = ws => fn => () => ws.on('open', fn);

// _onWsError :: WebSocket -> (String -> Effect Unit) -> Effect Unit
exports._onWsError = ws => fn => () => { 
  ws.on('error', function func(msg) {
    const str = msg.toString();
    console.log("error: ", msg.toString())
    fn(str)();
  })
}

// _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit
exports._onWsMessage = ws => fn => () => { 
  ws.on('message', function func(msg) {
    const str = msg.toString();
    console.log("message: ", msg.toString())
    fn(str)();
  })
}

// _wsSend :: WebSocket -> String -> Effect Unit
exports._wsSend = ws => str => () => {
  console.log("sending: ", str);
  ws.send(str);
}

// safely but asynchronously close the connection
// _wsClose :: WebSocket -> Effect Unit
exports._wsClose = ws => () => ws.close()

// immediately close connection
// _wsTerminate :: Websocket -> Effect Unit
exports._wsTerminate = ws => () => ws.terminate()

// _stringify :: a -> Effect String
exports._stringify = a => () => JSON.stringify(a)

// Every 30 seconds if we haven't heard from the server, sever the connection.
// There is a chance this will leak due to not removing listeners, in this case it should take the purescript function `discardOgmiosWebSocket`, fully applied as a lazy parameter and use it instead of `ws.terminate()`
// heartbeat :: WebSocket -> Int -> Effect Unit-> ImplicitUnsafeEffect Int
const heartbeat = ws => id => onError => {
  console.log("websocket heartbeat fired")
  ws.ping()
  if (id !== null) {
    clearTimeout(id);
  }
  const cancelId = setTimeout(() => {
    ws.terminate()
    onError()
  }, 30000);
  return cancelId;
}

// _wsWatch :: WebSocket -> Effect Unit -> Effect Unit
exports._wsWatch = ws => onError => () => {
  let counter = null;
  let heartbeatAndCount = () => { counter = heartbeat(ws, counter, onError) }
  ws.on('open', heartbeatAndCount)
  ws.on('ping', heartbeatAndCount)
  ws.on('pong', heartbeatAndCount)
  ws.on('close', () => clearTimeout(counter))
}
