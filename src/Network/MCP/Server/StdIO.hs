{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.MCP.Server.StdIO
  ( runServerWithSTDIO,
    withSTDIOServer,
  )
where

import Data.Void (Void)
import Network.MCP.Server
import Network.MCP.Transport.StdIO (newSTDIOTransport)

-- | Run an MCP server using STDIO transport, handling messages forever.
runServerWithSTDIO :: Server -> IO Void
runServerWithSTDIO server = do
  -- Create the STDIO transport
  stdioTransport <- newSTDIOTransport

  -- Start the transport
  runServerWithTransport server stdioTransport

withSTDIOServer :: Server -> IO a -> IO a
withSTDIOServer server action = do
  stdioTransport <- newSTDIOTransport
  withTransportServer server stdioTransport action
