{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Wi-Fi Network Diagnostics
--
-- The Wi-Fi Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRBaseClusterWiFiNetworkDiagnostics@.
module ObjC.Matter.MTRBaseClusterWiFiNetworkDiagnostics
  ( MTRBaseClusterWiFiNetworkDiagnostics
  , IsMTRBaseClusterWiFiNetworkDiagnostics(..)
  , resetCountsWithParams_completion
  , resetCountsWithCompletion
  , readAttributeBSSIDWithCompletion
  , subscribeAttributeBSSIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeBSSIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeSecurityTypeWithCompletion
  , subscribeAttributeSecurityTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSecurityTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeWiFiVersionWithCompletion
  , subscribeAttributeWiFiVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeWiFiVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeChannelNumberWithCompletion
  , subscribeAttributeChannelNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributeChannelNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeRSSIWithCompletion
  , subscribeAttributeRSSIWithParams_subscriptionEstablished_reportHandler
  , readAttributeRSSIWithClusterStateCache_endpoint_queue_completion
  , readAttributeBeaconLostCountWithCompletion
  , subscribeAttributeBeaconLostCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeBeaconLostCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeBeaconRxCountWithCompletion
  , subscribeAttributeBeaconRxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeBeaconRxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributePacketMulticastRxCountWithCompletion
  , subscribeAttributePacketMulticastRxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributePacketMulticastRxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributePacketMulticastTxCountWithCompletion
  , subscribeAttributePacketMulticastTxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributePacketMulticastTxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributePacketUnicastRxCountWithCompletion
  , subscribeAttributePacketUnicastRxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributePacketUnicastRxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributePacketUnicastTxCountWithCompletion
  , subscribeAttributePacketUnicastTxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributePacketUnicastTxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentMaxRateWithCompletion
  , subscribeAttributeCurrentMaxRateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentMaxRateWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverrunCountWithCompletion
  , subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeGeneratedCommandListWithCompletion
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAcceptedCommandListWithCompletion
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAttributeListWithCompletion
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeFeatureMapWithCompletion
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeClusterRevisionWithCompletion
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion
  , init_
  , new
  , initWithDevice_endpoint_queue
  , resetCountsWithParams_completionHandler
  , resetCountsWithCompletionHandler
  , readAttributeBssidWithCompletionHandler
  , subscribeAttributeBssidWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBssidWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSecurityTypeWithCompletionHandler
  , subscribeAttributeSecurityTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSecurityTypeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeWiFiVersionWithCompletionHandler
  , subscribeAttributeWiFiVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWiFiVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeChannelNumberWithCompletionHandler
  , subscribeAttributeChannelNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeChannelNumberWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRssiWithCompletionHandler
  , subscribeAttributeRssiWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRssiWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBeaconLostCountWithCompletionHandler
  , subscribeAttributeBeaconLostCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBeaconLostCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBeaconRxCountWithCompletionHandler
  , subscribeAttributeBeaconRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBeaconRxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePacketMulticastRxCountWithCompletionHandler
  , subscribeAttributePacketMulticastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePacketMulticastRxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePacketMulticastTxCountWithCompletionHandler
  , subscribeAttributePacketMulticastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePacketMulticastTxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePacketUnicastRxCountWithCompletionHandler
  , subscribeAttributePacketUnicastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePacketUnicastRxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePacketUnicastTxCountWithCompletionHandler
  , subscribeAttributePacketUnicastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePacketUnicastTxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentMaxRateWithCompletionHandler
  , subscribeAttributeCurrentMaxRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentMaxRateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOverrunCountWithCompletionHandler
  , subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , resetCountsWithParams_completionSelector
  , resetCountsWithCompletionSelector
  , readAttributeBSSIDWithCompletionSelector
  , subscribeAttributeBSSIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBSSIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSecurityTypeWithCompletionSelector
  , subscribeAttributeSecurityTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSecurityTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWiFiVersionWithCompletionSelector
  , subscribeAttributeWiFiVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeWiFiVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeChannelNumberWithCompletionSelector
  , subscribeAttributeChannelNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeChannelNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRSSIWithCompletionSelector
  , subscribeAttributeRSSIWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRSSIWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBeaconLostCountWithCompletionSelector
  , subscribeAttributeBeaconLostCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBeaconLostCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBeaconRxCountWithCompletionSelector
  , subscribeAttributeBeaconRxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBeaconRxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePacketMulticastRxCountWithCompletionSelector
  , subscribeAttributePacketMulticastRxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketMulticastRxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePacketMulticastTxCountWithCompletionSelector
  , subscribeAttributePacketMulticastTxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketMulticastTxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePacketUnicastRxCountWithCompletionSelector
  , subscribeAttributePacketUnicastRxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketUnicastRxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePacketUnicastTxCountWithCompletionSelector
  , subscribeAttributePacketUnicastTxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketUnicastTxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentMaxRateWithCompletionSelector
  , subscribeAttributeCurrentMaxRateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentMaxRateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverrunCountWithCompletionSelector
  , subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , resetCountsWithParams_completionHandlerSelector
  , resetCountsWithCompletionHandlerSelector
  , readAttributeBssidWithCompletionHandlerSelector
  , subscribeAttributeBssidWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBssidWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSecurityTypeWithCompletionHandlerSelector
  , subscribeAttributeSecurityTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSecurityTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeWiFiVersionWithCompletionHandlerSelector
  , subscribeAttributeWiFiVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeWiFiVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeChannelNumberWithCompletionHandlerSelector
  , subscribeAttributeChannelNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeChannelNumberWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRssiWithCompletionHandlerSelector
  , subscribeAttributeRssiWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeRssiWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBeaconLostCountWithCompletionHandlerSelector
  , subscribeAttributeBeaconLostCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBeaconLostCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBeaconRxCountWithCompletionHandlerSelector
  , subscribeAttributeBeaconRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBeaconRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePacketMulticastRxCountWithCompletionHandlerSelector
  , subscribeAttributePacketMulticastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketMulticastRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePacketMulticastTxCountWithCompletionHandlerSelector
  , subscribeAttributePacketMulticastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketMulticastTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePacketUnicastRxCountWithCompletionHandlerSelector
  , subscribeAttributePacketUnicastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketUnicastRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePacketUnicastTxCountWithCompletionHandlerSelector
  , subscribeAttributePacketUnicastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketUnicastTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentMaxRateWithCompletionHandlerSelector
  , subscribeAttributeCurrentMaxRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentMaxRateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOverrunCountWithCompletionHandlerSelector
  , subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , initWithDevice_endpointID_queueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ResetCounts
--
-- This command is used to reset the count attributes.
--
-- ObjC selector: @- resetCountsWithParams:completion:@
resetCountsWithParams_completion :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRWiFiNetworkDiagnosticsClusterResetCountsParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> IO ()
resetCountsWithParams_completion mtrBaseClusterWiFiNetworkDiagnostics  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetCountsWithCompletion:@
resetCountsWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
resetCountsWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBSSIDWithCompletion:@
readAttributeBSSIDWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeBSSIDWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBSSIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBSSIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBSSIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBSSIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeBSSIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBSSIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeBSSIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBSSIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBSSIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSecurityTypeWithCompletion:@
readAttributeSecurityTypeWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeSecurityTypeWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeSecurityTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSecurityTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSecurityTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSecurityTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeSecurityTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSecurityTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSecurityTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSecurityTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSecurityTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeWiFiVersionWithCompletion:@
readAttributeWiFiVersionWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeWiFiVersionWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeWiFiVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeWiFiVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWiFiVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWiFiVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeWiFiVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWiFiVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeWiFiVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWiFiVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWiFiVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeChannelNumberWithCompletion:@
readAttributeChannelNumberWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeChannelNumberWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeChannelNumberWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeChannelNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChannelNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeChannelNumberWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeChannelNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeChannelNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChannelNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeChannelNumberWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRSSIWithCompletion:@
readAttributeRSSIWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeRSSIWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeRSSIWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRSSIWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRSSIWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRSSIWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeRSSIWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRSSIWithClusterStateCache:endpoint:queue:completion:@
readAttributeRSSIWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRSSIWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRSSIWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBeaconLostCountWithCompletion:@
readAttributeBeaconLostCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeBeaconLostCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBeaconLostCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBeaconLostCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconLostCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBeaconLostCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeBeaconLostCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBeaconLostCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeBeaconLostCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBeaconLostCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBeaconLostCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBeaconRxCountWithCompletion:@
readAttributeBeaconRxCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeBeaconRxCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBeaconRxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBeaconRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconRxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBeaconRxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeBeaconRxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBeaconRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeBeaconRxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBeaconRxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBeaconRxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePacketMulticastRxCountWithCompletion:@
readAttributePacketMulticastRxCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketMulticastRxCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketMulticastRxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePacketMulticastRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastRxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketMulticastRxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketMulticastRxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketMulticastRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketMulticastRxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketMulticastRxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketMulticastRxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePacketMulticastTxCountWithCompletion:@
readAttributePacketMulticastTxCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketMulticastTxCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketMulticastTxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePacketMulticastTxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastTxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketMulticastTxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketMulticastTxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketMulticastTxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketMulticastTxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketMulticastTxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketMulticastTxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePacketUnicastRxCountWithCompletion:@
readAttributePacketUnicastRxCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketUnicastRxCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketUnicastRxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePacketUnicastRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastRxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketUnicastRxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketUnicastRxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketUnicastRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketUnicastRxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketUnicastRxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketUnicastRxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePacketUnicastTxCountWithCompletion:@
readAttributePacketUnicastTxCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketUnicastTxCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketUnicastTxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePacketUnicastTxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastTxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketUnicastTxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketUnicastTxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketUnicastTxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketUnicastTxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketUnicastTxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketUnicastTxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentMaxRateWithCompletion:@
readAttributeCurrentMaxRateWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeCurrentMaxRateWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeCurrentMaxRateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentMaxRateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentMaxRateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentMaxRateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeCurrentMaxRateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentMaxRateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentMaxRateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentMaxRateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentMaxRateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOverrunCountWithCompletion:@
readAttributeOverrunCountWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeOverrunCountWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWiFiNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> IO (Id MTRBaseClusterWiFiNetworkDiagnostics)
init_ mtrBaseClusterWiFiNetworkDiagnostics  =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterWiFiNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterWiFiNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRBaseClusterWiFiNetworkDiagnostics)
initWithDevice_endpoint_queue mtrBaseClusterWiFiNetworkDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetCountsWithParams:completionHandler:@
resetCountsWithParams_completionHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRWiFiNetworkDiagnosticsClusterResetCountsParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> params -> Ptr () -> IO ()
resetCountsWithParams_completionHandler mtrBaseClusterWiFiNetworkDiagnostics  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetCountsWithCompletionHandler:@
resetCountsWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
resetCountsWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "resetCountsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBssidWithCompletionHandler:@
readAttributeBssidWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeBssidWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBssidWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBssidWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBssidWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBssidWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeBssidWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBssidWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBssidWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBssidWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBssidWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSecurityTypeWithCompletionHandler:@
readAttributeSecurityTypeWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeSecurityTypeWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeSecurityTypeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSecurityTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSecurityTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSecurityTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeSecurityTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSecurityTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSecurityTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSecurityTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSecurityTypeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeWiFiVersionWithCompletionHandler:@
readAttributeWiFiVersionWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeWiFiVersionWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeWiFiVersionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeWiFiVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWiFiVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWiFiVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeWiFiVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWiFiVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWiFiVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWiFiVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWiFiVersionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeChannelNumberWithCompletionHandler:@
readAttributeChannelNumberWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeChannelNumberWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeChannelNumberWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeChannelNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChannelNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeChannelNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeChannelNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeChannelNumberWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChannelNumberWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeChannelNumberWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeRssiWithCompletionHandler:@
readAttributeRssiWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeRssiWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeRssiWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeRssiWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRssiWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRssiWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeRssiWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRssiWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRssiWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRssiWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRssiWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBeaconLostCountWithCompletionHandler:@
readAttributeBeaconLostCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeBeaconLostCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBeaconLostCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBeaconLostCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconLostCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBeaconLostCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeBeaconLostCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBeaconLostCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBeaconLostCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBeaconLostCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBeaconLostCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBeaconRxCountWithCompletionHandler:@
readAttributeBeaconRxCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeBeaconRxCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeBeaconRxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBeaconRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBeaconRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeBeaconRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBeaconRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBeaconRxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBeaconRxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBeaconRxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePacketMulticastRxCountWithCompletionHandler:@
readAttributePacketMulticastRxCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketMulticastRxCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketMulticastRxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePacketMulticastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketMulticastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketMulticastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketMulticastRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketMulticastRxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketMulticastRxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketMulticastRxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePacketMulticastTxCountWithCompletionHandler:@
readAttributePacketMulticastTxCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketMulticastTxCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketMulticastTxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePacketMulticastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketMulticastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketMulticastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketMulticastTxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketMulticastTxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketMulticastTxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketMulticastTxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePacketUnicastRxCountWithCompletionHandler:@
readAttributePacketUnicastRxCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketUnicastRxCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketUnicastRxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePacketUnicastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketUnicastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketUnicastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketUnicastRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketUnicastRxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketUnicastRxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketUnicastRxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePacketUnicastTxCountWithCompletionHandler:@
readAttributePacketUnicastTxCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketUnicastTxCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributePacketUnicastTxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePacketUnicastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketUnicastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributePacketUnicastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketUnicastTxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketUnicastTxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketUnicastTxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketUnicastTxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentMaxRateWithCompletionHandler:@
readAttributeCurrentMaxRateWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeCurrentMaxRateWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeCurrentMaxRateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentMaxRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentMaxRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentMaxRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeCurrentMaxRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentMaxRateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentMaxRateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentMaxRateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentMaxRateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOverrunCountWithCompletionHandler:@
readAttributeOverrunCountWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeOverrunCountWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics => mtrBaseClusterWiFiNetworkDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterWiFiNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterWiFiNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterWiFiNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterWiFiNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWiFiNetworkDiagnostics mtrBaseClusterWiFiNetworkDiagnostics, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWiFiNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWiFiNetworkDiagnostics)
initWithDevice_endpointID_queue mtrBaseClusterWiFiNetworkDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterWiFiNetworkDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:completion:@
resetCountsWithParams_completionSelector :: Selector
resetCountsWithParams_completionSelector = mkSelector "resetCountsWithParams:completion:"

-- | @Selector@ for @resetCountsWithCompletion:@
resetCountsWithCompletionSelector :: Selector
resetCountsWithCompletionSelector = mkSelector "resetCountsWithCompletion:"

-- | @Selector@ for @readAttributeBSSIDWithCompletion:@
readAttributeBSSIDWithCompletionSelector :: Selector
readAttributeBSSIDWithCompletionSelector = mkSelector "readAttributeBSSIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeBSSIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBSSIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBSSIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBSSIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBSSIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeBSSIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBSSIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBSSIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSecurityTypeWithCompletion:@
readAttributeSecurityTypeWithCompletionSelector :: Selector
readAttributeSecurityTypeWithCompletionSelector = mkSelector "readAttributeSecurityTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeSecurityTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSecurityTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSecurityTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSecurityTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSecurityTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSecurityTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSecurityTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSecurityTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWiFiVersionWithCompletion:@
readAttributeWiFiVersionWithCompletionSelector :: Selector
readAttributeWiFiVersionWithCompletionSelector = mkSelector "readAttributeWiFiVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeWiFiVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWiFiVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWiFiVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWiFiVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWiFiVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeWiFiVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeWiFiVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWiFiVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeChannelNumberWithCompletion:@
readAttributeChannelNumberWithCompletionSelector :: Selector
readAttributeChannelNumberWithCompletionSelector = mkSelector "readAttributeChannelNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributeChannelNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeChannelNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChannelNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChannelNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeChannelNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeChannelNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChannelNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRSSIWithCompletion:@
readAttributeRSSIWithCompletionSelector :: Selector
readAttributeRSSIWithCompletionSelector = mkSelector "readAttributeRSSIWithCompletion:"

-- | @Selector@ for @subscribeAttributeRSSIWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRSSIWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRSSIWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRSSIWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRSSIWithClusterStateCache:endpoint:queue:completion:@
readAttributeRSSIWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRSSIWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRSSIWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBeaconLostCountWithCompletion:@
readAttributeBeaconLostCountWithCompletionSelector :: Selector
readAttributeBeaconLostCountWithCompletionSelector = mkSelector "readAttributeBeaconLostCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeBeaconLostCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconLostCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBeaconLostCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBeaconLostCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBeaconLostCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeBeaconLostCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBeaconLostCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBeaconLostCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBeaconRxCountWithCompletion:@
readAttributeBeaconRxCountWithCompletionSelector :: Selector
readAttributeBeaconRxCountWithCompletionSelector = mkSelector "readAttributeBeaconRxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeBeaconRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconRxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBeaconRxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBeaconRxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBeaconRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeBeaconRxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBeaconRxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBeaconRxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePacketMulticastRxCountWithCompletion:@
readAttributePacketMulticastRxCountWithCompletionSelector :: Selector
readAttributePacketMulticastRxCountWithCompletionSelector = mkSelector "readAttributePacketMulticastRxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributePacketMulticastRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastRxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketMulticastRxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketMulticastRxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketMulticastRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketMulticastRxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePacketMulticastRxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePacketMulticastRxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePacketMulticastTxCountWithCompletion:@
readAttributePacketMulticastTxCountWithCompletionSelector :: Selector
readAttributePacketMulticastTxCountWithCompletionSelector = mkSelector "readAttributePacketMulticastTxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributePacketMulticastTxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastTxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketMulticastTxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketMulticastTxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketMulticastTxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketMulticastTxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePacketMulticastTxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePacketMulticastTxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePacketUnicastRxCountWithCompletion:@
readAttributePacketUnicastRxCountWithCompletionSelector :: Selector
readAttributePacketUnicastRxCountWithCompletionSelector = mkSelector "readAttributePacketUnicastRxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributePacketUnicastRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastRxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketUnicastRxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketUnicastRxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketUnicastRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketUnicastRxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePacketUnicastRxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePacketUnicastRxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePacketUnicastTxCountWithCompletion:@
readAttributePacketUnicastTxCountWithCompletionSelector :: Selector
readAttributePacketUnicastTxCountWithCompletionSelector = mkSelector "readAttributePacketUnicastTxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributePacketUnicastTxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastTxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketUnicastTxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketUnicastTxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketUnicastTxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketUnicastTxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePacketUnicastTxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePacketUnicastTxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentMaxRateWithCompletion:@
readAttributeCurrentMaxRateWithCompletionSelector :: Selector
readAttributeCurrentMaxRateWithCompletionSelector = mkSelector "readAttributeCurrentMaxRateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentMaxRateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentMaxRateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentMaxRateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentMaxRateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentMaxRateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentMaxRateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentMaxRateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentMaxRateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverrunCountWithCompletion:@
readAttributeOverrunCountWithCompletionSelector :: Selector
readAttributeOverrunCountWithCompletionSelector = mkSelector "readAttributeOverrunCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletionSelector :: Selector
readAttributeGeneratedCommandListWithCompletionSelector = mkSelector "readAttributeGeneratedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletionSelector :: Selector
readAttributeAcceptedCommandListWithCompletionSelector = mkSelector "readAttributeAcceptedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletionSelector :: Selector
readAttributeAttributeListWithCompletionSelector = mkSelector "readAttributeAttributeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletionSelector :: Selector
readAttributeFeatureMapWithCompletionSelector = mkSelector "readAttributeFeatureMapWithCompletion:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletionSelector :: Selector
readAttributeClusterRevisionWithCompletionSelector = mkSelector "readAttributeClusterRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @resetCountsWithParams:completionHandler:@
resetCountsWithParams_completionHandlerSelector :: Selector
resetCountsWithParams_completionHandlerSelector = mkSelector "resetCountsWithParams:completionHandler:"

-- | @Selector@ for @resetCountsWithCompletionHandler:@
resetCountsWithCompletionHandlerSelector :: Selector
resetCountsWithCompletionHandlerSelector = mkSelector "resetCountsWithCompletionHandler:"

-- | @Selector@ for @readAttributeBssidWithCompletionHandler:@
readAttributeBssidWithCompletionHandlerSelector :: Selector
readAttributeBssidWithCompletionHandlerSelector = mkSelector "readAttributeBssidWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBssidWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBssidWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBssidWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBssidWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBssidWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBssidWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBssidWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBssidWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSecurityTypeWithCompletionHandler:@
readAttributeSecurityTypeWithCompletionHandlerSelector :: Selector
readAttributeSecurityTypeWithCompletionHandlerSelector = mkSelector "readAttributeSecurityTypeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSecurityTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSecurityTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSecurityTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSecurityTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSecurityTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSecurityTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSecurityTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSecurityTypeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeWiFiVersionWithCompletionHandler:@
readAttributeWiFiVersionWithCompletionHandlerSelector :: Selector
readAttributeWiFiVersionWithCompletionHandlerSelector = mkSelector "readAttributeWiFiVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeWiFiVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWiFiVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWiFiVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWiFiVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWiFiVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWiFiVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeWiFiVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWiFiVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeChannelNumberWithCompletionHandler:@
readAttributeChannelNumberWithCompletionHandlerSelector :: Selector
readAttributeChannelNumberWithCompletionHandlerSelector = mkSelector "readAttributeChannelNumberWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeChannelNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeChannelNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChannelNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChannelNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeChannelNumberWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeChannelNumberWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeChannelNumberWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRssiWithCompletionHandler:@
readAttributeRssiWithCompletionHandlerSelector :: Selector
readAttributeRssiWithCompletionHandlerSelector = mkSelector "readAttributeRssiWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeRssiWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRssiWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRssiWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRssiWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRssiWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRssiWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeRssiWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRssiWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBeaconLostCountWithCompletionHandler:@
readAttributeBeaconLostCountWithCompletionHandlerSelector :: Selector
readAttributeBeaconLostCountWithCompletionHandlerSelector = mkSelector "readAttributeBeaconLostCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBeaconLostCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconLostCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBeaconLostCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBeaconLostCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBeaconLostCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBeaconLostCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBeaconLostCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBeaconLostCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBeaconRxCountWithCompletionHandler:@
readAttributeBeaconRxCountWithCompletionHandlerSelector :: Selector
readAttributeBeaconRxCountWithCompletionHandlerSelector = mkSelector "readAttributeBeaconRxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBeaconRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBeaconRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBeaconRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBeaconRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBeaconRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBeaconRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBeaconRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBeaconRxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePacketMulticastRxCountWithCompletionHandler:@
readAttributePacketMulticastRxCountWithCompletionHandlerSelector :: Selector
readAttributePacketMulticastRxCountWithCompletionHandlerSelector = mkSelector "readAttributePacketMulticastRxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePacketMulticastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketMulticastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketMulticastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketMulticastRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketMulticastRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePacketMulticastRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePacketMulticastRxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePacketMulticastTxCountWithCompletionHandler:@
readAttributePacketMulticastTxCountWithCompletionHandlerSelector :: Selector
readAttributePacketMulticastTxCountWithCompletionHandlerSelector = mkSelector "readAttributePacketMulticastTxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePacketMulticastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketMulticastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketMulticastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketMulticastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketMulticastTxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketMulticastTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePacketMulticastTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePacketMulticastTxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePacketUnicastRxCountWithCompletionHandler:@
readAttributePacketUnicastRxCountWithCompletionHandlerSelector :: Selector
readAttributePacketUnicastRxCountWithCompletionHandlerSelector = mkSelector "readAttributePacketUnicastRxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePacketUnicastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketUnicastRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketUnicastRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketUnicastRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketUnicastRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePacketUnicastRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePacketUnicastRxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePacketUnicastTxCountWithCompletionHandler:@
readAttributePacketUnicastTxCountWithCompletionHandlerSelector :: Selector
readAttributePacketUnicastTxCountWithCompletionHandlerSelector = mkSelector "readAttributePacketUnicastTxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePacketUnicastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketUnicastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketUnicastTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketUnicastTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketUnicastTxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketUnicastTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePacketUnicastTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePacketUnicastTxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentMaxRateWithCompletionHandler:@
readAttributeCurrentMaxRateWithCompletionHandlerSelector :: Selector
readAttributeCurrentMaxRateWithCompletionHandlerSelector = mkSelector "readAttributeCurrentMaxRateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentMaxRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentMaxRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentMaxRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentMaxRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentMaxRateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentMaxRateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentMaxRateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentMaxRateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOverrunCountWithCompletionHandler:@
readAttributeOverrunCountWithCompletionHandlerSelector :: Selector
readAttributeOverrunCountWithCompletionHandlerSelector = mkSelector "readAttributeOverrunCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

