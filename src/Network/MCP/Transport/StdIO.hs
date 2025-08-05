{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.MCP.Transport.StdIO
  ( STDIOTransport (..),
    newSTDIOTransport,
  )
where

import Control.Exception (SomeException, handle, throwIO, toException)
import Control.Monad (forever)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Network.MCP.Transport.Types
import System.IO

-- | STDIO implementation of the Transport interface
data STDIOTransport = STDIOTransport
  { stdinHandle :: Handle,
    stdoutHandle :: Handle,
    stderrHandle :: Handle
  }

-- | Create a new STDIO transport with the given message handler
newSTDIOTransport ::
  IO STDIOTransport
newSTDIOTransport = do
  -- Configure handles for better performance
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8

  return $
    STDIOTransport
      { stdinHandle = stdin,
        stdoutHandle = stdout,
        stderrHandle = stderr
      }

-- | A simple, synchronous transport implementation using standard input/output.
instance Transport STDIOTransport where
  handleMessagesForever (STDIOTransport {stdinHandle, stderrHandle, stdoutHandle}) handler = do
    hSetBuffering stdinHandle LineBuffering
    hSetBuffering stdoutHandle LineBuffering
    hSetEncoding stdinHandle utf8
    hSetEncoding stdoutHandle utf8
    forever $ handle handleErr $ do
      msg <- readMessage
      handler msg >>= \case
        Nothing -> pure ()
        Just response -> sendMessage response
    where
      readMessage = do
        line <- BS8.hGetLine stdinHandle
        case eitherDecode (BS8.fromStrict line) of
          Left err ->
            -- On parse error, log and try again with a default error message
            throwIO $ toException $ userError ("JSON decode error: " ++ err)
          Right msg -> pure msg

      -- Send a message through the transport
      sendMessage msg = do
        BL.hPut stdoutHandle (encode msg)
        BL.hPut stdoutHandle "\n"
        hFlush stdoutHandle

      handleErr :: SomeException -> IO ()
      handleErr err = do
        hPutStrLn stderrHandle $ "Error reading message: " ++ show err
