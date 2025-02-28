{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


-- | Run a TCP server, with hardening against connection flooding
module Cardano.Logging.Prometheus.NetworkRun
       ( NetworkRunParams (..)
       , TimeoutServer
       , defaultRunParams
       , runTCPServer
       ) where

import           Cardano.Logging.Utils (threadLabelMe)

import           Control.Concurrent (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue
import qualified Control.Exception as E
import           Control.Monad (forever, void, when)
import           Data.Hashable (hash)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Network.Socket
import qualified System.TimeManager as T


-- | Parameters specifying how the TCP server should be run
data NetworkRunParams = NetworkRunParams
  { runSocketTimeout    :: !Int             -- ^ Release socket after inactivity (seconds)
  , runSocketGraceful   :: !Int             -- ^ Graceful closing of socket (milliseconds), 0 to disable
  , runRecvMaxSize      :: !Int             -- ^ Close socket if more than (runRecvMaxSize - 1) bytes received; choose a small power of 2
  , runRateLimit        :: !Double          -- ^ Limit requests per second (may be < 0.0), 0.0 to disable
  }

defaultRunParams :: NetworkRunParams
defaultRunParams = NetworkRunParams
  { runSocketTimeout    = 16
  , runSocketGraceful   = 1000
  , runRecvMaxSize      = 2048
  , runRateLimit        = 3.0
  }


-- A server having the run params in scope, as well as an IO action to reset the timeout
type TimeoutServer a
    =  NetworkRunParams
    -> IO ()
    -> Socket
    -> IO a

-- | Runs a TCP server conforming to the run parameters.
--   Will bind to localhost / loopback device only if no host name is specified.
runTCPServer
  :: NetworkRunParams
  -> Maybe HostName
  -> PortNumber
  -> TimeoutServer a
  -> IO a
runTCPServer runParams (fromMaybe "127.0.0.1" -> host) portNo server = do
  threadLabelMe "PrometheusSimple server"
  addr <- resolve host portNo
  E.bracket (openTCPServerSocket addr) close $ \sock ->
    runTCPServerWithSocket runParams sock server

runTCPServerWithSocket
  :: NetworkRunParams
  -> Socket
  -> TimeoutServer a
  -> IO a
runTCPServerWithSocket runParams@NetworkRunParams{..} sock server = do
  rateLimiter <- mkRateLimiter runRateLimit
  T.withManager (runSocketTimeout * 1000000) $ \mgr -> forever $ do
    waitForLimiter rateLimiter
    E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) ->

      -- TODO implement connection limit (global + per peer)

      void $ forkFinally (server' mgr conn) (const $ gclose conn)
  where
    gclose = if runSocketGraceful > 0 then flip gracefulClose runSocketGraceful else close
    server' mgr conn = do
      threadLabelMe "PrometheusSimple timeout server"
      T.withHandle mgr (return ()) $ \timeoutHandle ->
        server runParams (T.tickle timeoutHandle) conn

resolve :: HostName -> PortNumber -> IO AddrInfo
resolve host portNo =
  head <$> getAddrInfo (Just hints) (Just host) (Just $ show portNo)
  where
    hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }

openTCPServerSocket :: AddrInfo -> IO Socket
openTCPServerSocket addr = do
  sock <- openServerSocket
  listen sock 1024
  return sock
  where
    openServerSocket = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
#if !defined(openbsd_HOST_OS)
      when (addrFamily addr == AF_INET6) $ setSocketOption sock IPv6Only 1
#endif
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      return sock

newtype RateLimiter = RateLimiter {waitForLimiter :: IO ()}

mkRateLimiter :: Double -> IO RateLimiter
mkRateLimiter reqPerSecond
  | reqPerSecond == 0.0 = pure $ RateLimiter (pure ())
  | otherwise = do
    lock <- newTBQueueIO queueSize
    void . forkIO $ do
      threadLabelMe "PrometheusSimple rate limiter"
      forever $ do
        atomically $ writeTBQueue lock ()
        threadDelay delay
    pure $ RateLimiter (void $ atomically $ readTBQueue lock)
    where
        delay     = round $ 1000000 / reqPerSecond
        queueSize = ceiling reqPerSecond

_peerId :: SockAddr -> Int
_peerId = \case
  SockAddrInet _ h      -> hash h
  SockAddrInet6 _ _ h _ -> hash h
  SockAddrUnix s        -> hash s
