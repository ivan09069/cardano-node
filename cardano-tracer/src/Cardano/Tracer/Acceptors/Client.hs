{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Acceptors.Client
  ( runAcceptorsClient
  ) where

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
#if RTVIEW
import           Cardano.Tracer.Acceptors.Utils (notifyAboutNodeDisconnected,
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
#else
import           Cardano.Tracer.Acceptors.Utils (
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
#endif
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (deregisterNodeId, traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)
import qualified Network.Mux as Mux
import           Ouroboros.Network.Context (MinimalInitiatorContext (..), ResponderContext (..))
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer)
import           Ouroboros.Network.Socket (ConnectionId (..),
                   ConnectToArgs (..), HandshakeCallbacks (..),
                   connectToNode, debuggingNetworkConnectTracers)

import           Codec.CBOR.Term (Term)
import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void, absurd)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsInit)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsInit)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsInit)

runAcceptorsClient
  :: TracerEnv
  -> TracerEnvRTView
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsClient tracerEnv@TracerEnv {..} tracerEnvRTView p (ekgConfig, tfConfig, dpfConfig) = withIOManager \iocp -> do
  traceWith teTracer $ TracerSockConnecting p
  doConnectToForwarder
    (localSnocket iocp)
    (localAddressFromPath p)
    tracerEnv
    noTimeLimitsHandshake $
    -- Please note that we always run all the supported protocols,
    -- there is no mechanism to disable some of them.
    appInitiator
      [ (runEKGAcceptorInit          tracerEnv ekgConfig errorHandler, 1)
      , (runTraceObjectsAcceptorInit tracerEnv tracerEnvRTView tfConfig errorHandler, 2)
      , (runDataPointsAcceptorInit   tracerEnv dpfConfig errorHandler, 3)
      ]
 where
  appInitiator protocolsWithNums =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol
         }
      | (protocol, num) <- protocolsWithNums
      ]
  errorHandler connId = do
    deregisterNodeId tracerEnv (connIdToNodeId connId)
    removeDisconnectedNode tracerEnv connId
#if RTVIEW
    notifyAboutNodeDisconnected tracerEnvRTView connId
#endif

doConnectToForwarder
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> TracerEnv
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'Mux.InitiatorMode
                          (MinimalInitiatorContext LocalAddress)
                          (ResponderContext LocalAddress)
                          LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarder snocket address TracerEnv {..} timeLimits app
  = connectToNode
      snocket
      makeLocalBearer
      ConnectToArgs {..}
      mempty -- LocalSocket does not require to be configured
      versions
      Nothing
      address >>= \case
                      Right (Left ()) -> pure ()
                      Right (Right void) -> absurd void
                      Left err -> throwIO err
  where
    TC.TracerConfig {..} = teConfig
    ctaHandshakeCodec = codecHandshake forwardingVersionCodec
    ctaHandshakeTimeLimits = timeLimits
    ctaVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm
    ctaConnectTracers
      | Just True <- tracerConfigDebug = debuggingNetworkConnectTracers
      | otherwise = nullNetworkConnectTracers
    ctaHandshakeCallbacks = HandshakeCallbacks acceptableVersion queryVersion
    fwdVersData = ForwardingVersionData $ NetworkMagic networkMagic
    versions = simpleSingletonVersions ForwardingV_1 fwdVersData app

runEKGAcceptorInit
  :: TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     respoinderCtx
                     LBS.ByteString IO () Void
runEKGAcceptorInit tracerEnv ekgConfig errorHandler =
  acceptEKGMetricsInit
    ekgConfig
    (prepareMetricsStores tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)

runTraceObjectsAcceptorInit
  :: TracerEnv
  -> TracerEnvRTView
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     responderCtx
                     LBS.ByteString IO () Void
runTraceObjectsAcceptorInit tracerEnv tracerEnvRTView tfConfig errorHandler =
  acceptTraceObjectsInit
    tfConfig
    (traceObjectsHandler tracerEnv tracerEnvRTView . connIdToNodeId . micConnectionId)
    (errorHandler . micConnectionId)

runDataPointsAcceptorInit
  :: TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     responderCtx
                     LBS.ByteString IO () Void
runDataPointsAcceptorInit tracerEnv dpfConfig errorHandler =
  acceptDataPointsInit
    dpfConfig
    (prepareDataPointRequestor tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)
