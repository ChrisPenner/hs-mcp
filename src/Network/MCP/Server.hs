{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.MCP.Server
  ( Server,
    ServerHandler,
    ResourceReadHandler,
    ToolCallHandler,
    PromptHandler,
    createServer,
    registerResources,
    registerResourceReadHandler,
    registerTools,
    registerToolCallHandler,
    registerPrompts,
    registerPromptHandler,
    runServerWithTransport,
    withTransportServer,
    handleRequest,
  )
where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, toException, try)
import Control.Monad
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.MCP.Server.Types
import Network.MCP.Transport.Types
import Network.MCP.Types

createServer :: ServerInfo -> ServerCapabilities -> Text -> IO Server
createServer info caps instructions = do
  resourcesVar <- newTVarIO []
  toolsVar <- newTVarIO []
  promptsVar <- newTVarIO []
  resourceHandlerVar <- newTVarIO Nothing
  toolHandlerVar <- newTVarIO Nothing
  promptHandlerVar <- newTVarIO Nothing

  -- Initialize message handlers map
  handlersVar <- newTVarIO Map.empty

  let server =
        Server
          { serverInfo = info,
            serverCapabilities = caps,
            serverResources = resourcesVar,
            serverTools = toolsVar,
            serverPrompts = promptsVar,
            serverInstructions = instructions,
            serverResourceReadHandler = resourceHandlerVar,
            serverToolCallHandler = toolHandlerVar,
            serverPromptHandler = promptHandlerVar,
            serverMessageHandlers = handlersVar
          }

  -- Register standard request handlers
  registerInitializeHandler server
  registerListResourcesHandler server
  registerReadResourceHandler server
  registerListToolsHandler server
  registerCallToolHandler server
  registerListPromptsHandler server
  registerGetPromptHandler server

  return server

-- | Register available resources
registerResources :: Server -> [Resource] -> IO ()
registerResources server resources = atomically $ writeTVar (serverResources server) resources

-- | Register resource read handler
registerResourceReadHandler :: Server -> ResourceReadHandler -> IO ()
registerResourceReadHandler server handler = atomically $ writeTVar (serverResourceReadHandler server) (Just handler)

-- | Register available tools
registerTools :: Server -> [Tool] -> IO ()
registerTools server tools = atomically $ writeTVar (serverTools server) tools

-- | Register tool call handler
registerToolCallHandler :: Server -> ToolCallHandler -> IO ()
registerToolCallHandler server handler = atomically $ writeTVar (serverToolCallHandler server) (Just handler)

-- | Register available prompts
registerPrompts :: Server -> [Prompt] -> IO ()
registerPrompts server prompts = atomically $ writeTVar (serverPrompts server) prompts

-- | Register prompt handler
registerPromptHandler :: Server -> PromptHandler -> IO ()
registerPromptHandler server handler = atomically $ writeTVar (serverPromptHandler server) (Just handler)

runServerWithTransport :: (Transport t) => Server -> t -> IO ()
runServerWithTransport server t = do
  void $
    forkIO
      ( handleMessages t (handleMessage server)
      )

withTransportServer :: (Transport t) => Server -> t -> IO a -> IO a
withTransportServer server t action = do
  let runServer = handleMessages t (handleMessage server)
  Async.withAsync runServer $ \_ -> do
    -- Run the server with the provided transport
    runServerWithTransport server t
    -- Execute the action with the server running
    action

-- Helper to process a message
handleMessage :: Server -> Message -> IO (Maybe Message)
handleMessage server msg = case msg of
  RequestMessage request ->
    handleRequest server request >>= \case
      Right response -> pure . Just $ (ResponseMessage response)
      Left err -> pure . Just $ mkErrorResponse request err
  NotificationMessage notification -> do
    handleNotification server notification
    pure Nothing
  _ -> do
    throwIO $ TransportError $ "Received unexpected message type in handleMessage: " <> show msg

-- Helper to send error responses
mkErrorResponse :: Request -> SomeException -> Message
mkErrorResponse request err =
  let errorResponse =
        Response
          { responseJsonrpc = JSONRPC "2.0",
            responseId = requestId request,
            responseResult = Nothing,
            responseError =
              Just $
                ErrorResponse
                  { errorCode = -32603,
                    errorMessage = T.pack $ show err,
                    errorData = Nothing
                  }
          }
   in (ResponseMessage errorResponse)

-- | Handle a request
handleRequest :: Server -> Request -> IO (Either SomeException Response)
handleRequest server request = do
  let method = requestMethod request
      params = fromMaybe Null (requestParams request)

  handlers <- readTVarIO (serverMessageHandlers server)
  case Map.lookup method handlers of
    Just handler -> do
      e'result <- try $ handler params server
      case e'result of
        Right value ->
          return $
            Right $
              Response
                { responseJsonrpc = JSONRPC "2.0",
                  responseId = requestId request,
                  responseResult = case value of
                    Left _err -> error (show _err) -- WAT?
                    Right v -> Just v,
                  responseError = Nothing
                }
        Left err -> return $ Left err
    Nothing ->
      return $
        Right $
          Response
            { responseJsonrpc = JSONRPC "2.0",
              responseId = requestId request,
              responseResult = Nothing,
              responseError =
                Just $
                  ErrorResponse
                    { errorCode = -32601,
                      errorMessage = "Method not found",
                      errorData = Nothing
                    }
            }

-- | Handle a notification
handleNotification :: Server -> Notification -> IO ()
handleNotification _server notification = do
  let method = notificationMethod notification
      _params = fromMaybe Null (notificationParams notification)

  -- TODO: Implement notification handling
  putStrLn $ "Received notification: " ++ T.unpack method

-- | Register a request handler
registerRequestHandler :: Server -> Text -> (Value -> Server -> IO (Either SomeException Value)) -> IO ()
registerRequestHandler server method handler = atomically $ do
  handlers <- readTVar (serverMessageHandlers server)
  writeTVar (serverMessageHandlers server) (Map.insert method handler handlers)

-- | Register initialize handler
registerInitializeHandler :: Server -> IO ()
registerInitializeHandler server = registerRequestHandler server "initialize" $ \params _ -> do
  case fromJSON params of
    Success (_clientOptions :: ClientInitializeOptions) -> do
      -- TODO: Verify protocol version compatibility

      let serverOptions =
            ServerInitializeOptions
              { serverInitProtocolVersion = head supportedVersions,
                serverInitInfo = serverInfo server,
                serverInitCapabilities = serverCapabilities server,
                serverInitInstructions = serverInstructions server
              }

      return $ Right $ toJSON serverOptions
    Error err -> return $ Left $ toException (userError $ "Invalid initialize parameters: " ++ err)

-- | Register list resources handler
registerListResourcesHandler :: Server -> IO ()
registerListResourcesHandler server = registerRequestHandler server "resources/list" $ \_ svr -> do
  resources <- readTVarIO (serverResources svr)
  let result = ListResourcesResult resources
  return $ Right $ toJSON result

-- | Register read resource handler
registerReadResourceHandler :: Server -> IO ()
registerReadResourceHandler server = registerRequestHandler server "resources/read" $ \params svr -> do
  handlerM <- readTVarIO (serverResourceReadHandler svr)
  case handlerM of
    Just handler -> case fromJSON params of
      Success req -> do
        result <- handler req
        return $ Right $ toJSON result
      Error err -> return $ Left $ toException (userError $ "Invalid resource read parameters: " ++ err)
    Nothing -> return $ Left $ toException (userError "No resource read handler registered")

-- | Register list tools handler
registerListToolsHandler :: Server -> IO ()
registerListToolsHandler server = registerRequestHandler server "tools/list" $ \_ svr -> do
  tools <- readTVarIO (serverTools svr)
  let result = ListToolsResult tools
  return $ Right $ toJSON result

-- | Register call tool handler
registerCallToolHandler :: Server -> IO ()
registerCallToolHandler server = registerRequestHandler server "tools/call" $ \params svr -> do
  handlerM <- readTVarIO (serverToolCallHandler svr)
  case handlerM of
    Just handler -> case fromJSON params of
      Success req -> do
        result <- handler req
        return $ Right $ toJSON result
      Error err -> return $ Left $ toException (userError $ "Invalid tool call parameters: " ++ err)
    Nothing -> return $ Left $ toException (userError "No tool call handler registered")

-- | Register list prompts handler
registerListPromptsHandler :: Server -> IO ()
registerListPromptsHandler server = registerRequestHandler server "prompts/list" $ \_ svr -> do
  prompts <- readTVarIO (serverPrompts svr)
  let result = ListPromptsResult prompts
  return $ Right $ toJSON result

-- | Register get prompt handler
registerGetPromptHandler :: Server -> IO ()
registerGetPromptHandler server = registerRequestHandler server "prompts/get" $ \params svr -> do
  handlerM <- readTVarIO (serverPromptHandler svr)
  case handlerM of
    Just handler -> case fromJSON params of
      Success req -> do
        result <- handler req
        return $ Right $ toJSON result
      Error err -> return $ Left $ toException (userError $ "Invalid get prompt parameters: " ++ err)
    Nothing -> return $ Left $ toException (userError "No prompt handler registered")
