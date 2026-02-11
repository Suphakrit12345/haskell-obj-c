{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ethernet Network Diagnostics
--
-- The Ethernet Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRBaseClusterEthernetNetworkDiagnostics@.
module ObjC.Matter.MTRBaseClusterEthernetNetworkDiagnostics
  ( MTRBaseClusterEthernetNetworkDiagnostics
  , IsMTRBaseClusterEthernetNetworkDiagnostics(..)
  , resetCountsWithParams_completion
  , resetCountsWithCompletion
  , readAttributePHYRateWithCompletion
  , subscribeAttributePHYRateWithParams_subscriptionEstablished_reportHandler
  , readAttributePHYRateWithClusterStateCache_endpoint_queue_completion
  , readAttributeFullDuplexWithCompletion
  , subscribeAttributeFullDuplexWithParams_subscriptionEstablished_reportHandler
  , readAttributeFullDuplexWithClusterStateCache_endpoint_queue_completion
  , readAttributePacketRxCountWithCompletion
  , subscribeAttributePacketRxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributePacketRxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributePacketTxCountWithCompletion
  , subscribeAttributePacketTxCountWithParams_subscriptionEstablished_reportHandler
  , readAttributePacketTxCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeTxErrCountWithCompletion
  , subscribeAttributeTxErrCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeTxErrCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeCollisionCountWithCompletion
  , subscribeAttributeCollisionCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeCollisionCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverrunCountWithCompletion
  , subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeCarrierDetectWithCompletion
  , subscribeAttributeCarrierDetectWithParams_subscriptionEstablished_reportHandler
  , readAttributeCarrierDetectWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeSinceResetWithCompletion
  , subscribeAttributeTimeSinceResetWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeSinceResetWithClusterStateCache_endpoint_queue_completion
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
  , readAttributePHYRateWithCompletionHandler
  , subscribeAttributePHYRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePHYRateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFullDuplexWithCompletionHandler
  , subscribeAttributeFullDuplexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFullDuplexWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePacketRxCountWithCompletionHandler
  , subscribeAttributePacketRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePacketRxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePacketTxCountWithCompletionHandler
  , subscribeAttributePacketTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePacketTxCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTxErrCountWithCompletionHandler
  , subscribeAttributeTxErrCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTxErrCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCollisionCountWithCompletionHandler
  , subscribeAttributeCollisionCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCollisionCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOverrunCountWithCompletionHandler
  , subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCarrierDetectWithCompletionHandler
  , subscribeAttributeCarrierDetectWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCarrierDetectWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTimeSinceResetWithCompletionHandler
  , subscribeAttributeTimeSinceResetWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTimeSinceResetWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributePHYRateWithCompletionSelector
  , subscribeAttributePHYRateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePHYRateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFullDuplexWithCompletionSelector
  , subscribeAttributeFullDuplexWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFullDuplexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePacketRxCountWithCompletionSelector
  , subscribeAttributePacketRxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketRxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePacketTxCountWithCompletionSelector
  , subscribeAttributePacketTxCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketTxCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTxErrCountWithCompletionSelector
  , subscribeAttributeTxErrCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTxErrCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCollisionCountWithCompletionSelector
  , subscribeAttributeCollisionCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCollisionCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverrunCountWithCompletionSelector
  , subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCarrierDetectWithCompletionSelector
  , subscribeAttributeCarrierDetectWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCarrierDetectWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeSinceResetWithCompletionSelector
  , subscribeAttributeTimeSinceResetWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeSinceResetWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributePHYRateWithCompletionHandlerSelector
  , subscribeAttributePHYRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePHYRateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFullDuplexWithCompletionHandlerSelector
  , subscribeAttributeFullDuplexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFullDuplexWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePacketRxCountWithCompletionHandlerSelector
  , subscribeAttributePacketRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePacketTxCountWithCompletionHandlerSelector
  , subscribeAttributePacketTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePacketTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTxErrCountWithCompletionHandlerSelector
  , subscribeAttributeTxErrCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTxErrCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCollisionCountWithCompletionHandlerSelector
  , subscribeAttributeCollisionCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCollisionCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOverrunCountWithCompletionHandlerSelector
  , subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCarrierDetectWithCompletionHandlerSelector
  , subscribeAttributeCarrierDetectWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCarrierDetectWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTimeSinceResetWithCompletionHandlerSelector
  , subscribeAttributeTimeSinceResetWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeSinceResetWithAttributeCache_endpoint_queue_completionHandlerSelector
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
resetCountsWithParams_completion :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTREthernetNetworkDiagnosticsClusterResetCountsParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> IO ()
resetCountsWithParams_completion mtrBaseClusterEthernetNetworkDiagnostics  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetCountsWithCompletion:@
resetCountsWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
resetCountsWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePHYRateWithCompletion:@
readAttributePHYRateWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributePHYRateWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributePHYRateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePHYRateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePHYRateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePHYRateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributePHYRateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePHYRateWithClusterStateCache:endpoint:queue:completion:@
readAttributePHYRateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePHYRateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePHYRateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFullDuplexWithCompletion:@
readAttributeFullDuplexWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeFullDuplexWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeFullDuplexWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFullDuplexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFullDuplexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFullDuplexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeFullDuplexWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFullDuplexWithClusterStateCache:endpoint:queue:completion:@
readAttributeFullDuplexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFullDuplexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFullDuplexWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePacketRxCountWithCompletion:@
readAttributePacketRxCountWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketRxCountWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributePacketRxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePacketRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketRxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketRxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributePacketRxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketRxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketRxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketRxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePacketTxCountWithCompletion:@
readAttributePacketTxCountWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketTxCountWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributePacketTxCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePacketTxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketTxCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketTxCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributePacketTxCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketTxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketTxCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketTxCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketTxCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTxErrCountWithCompletion:@
readAttributeTxErrCountWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeTxErrCountWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeTxErrCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTxErrCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTxErrCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTxErrCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeTxErrCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTxErrCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeTxErrCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTxErrCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTxErrCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCollisionCountWithCompletion:@
readAttributeCollisionCountWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeCollisionCountWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeCollisionCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCollisionCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCollisionCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCollisionCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeCollisionCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCollisionCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeCollisionCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCollisionCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCollisionCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOverrunCountWithCompletion:@
readAttributeOverrunCountWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeOverrunCountWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCarrierDetectWithCompletion:@
readAttributeCarrierDetectWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeCarrierDetectWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeCarrierDetectWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCarrierDetectWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCarrierDetectWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCarrierDetectWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeCarrierDetectWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCarrierDetectWithClusterStateCache:endpoint:queue:completion:@
readAttributeCarrierDetectWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCarrierDetectWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCarrierDetectWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimeSinceResetWithCompletion:@
readAttributeTimeSinceResetWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeTimeSinceResetWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeTimeSinceResetWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimeSinceResetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSinceResetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeSinceResetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeTimeSinceResetWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeSinceResetWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeSinceResetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeSinceResetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeSinceResetWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterEthernetNetworkDiagnostics  completion =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> IO (Id MTRBaseClusterEthernetNetworkDiagnostics)
init_ mtrBaseClusterEthernetNetworkDiagnostics  =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterEthernetNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterEthernetNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRBaseClusterEthernetNetworkDiagnostics)
initWithDevice_endpoint_queue mtrBaseClusterEthernetNetworkDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetCountsWithParams:completionHandler:@
resetCountsWithParams_completionHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTREthernetNetworkDiagnosticsClusterResetCountsParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> params -> Ptr () -> IO ()
resetCountsWithParams_completionHandler mtrBaseClusterEthernetNetworkDiagnostics  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetCountsWithCompletionHandler:@
resetCountsWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
resetCountsWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "resetCountsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePHYRateWithCompletionHandler:@
readAttributePHYRateWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributePHYRateWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributePHYRateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePHYRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePHYRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePHYRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributePHYRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePHYRateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePHYRateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePHYRateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePHYRateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFullDuplexWithCompletionHandler:@
readAttributeFullDuplexWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeFullDuplexWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeFullDuplexWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFullDuplexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFullDuplexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFullDuplexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeFullDuplexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFullDuplexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFullDuplexWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFullDuplexWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFullDuplexWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePacketRxCountWithCompletionHandler:@
readAttributePacketRxCountWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketRxCountWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributePacketRxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePacketRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributePacketRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketRxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketRxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketRxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePacketTxCountWithCompletionHandler:@
readAttributePacketTxCountWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributePacketTxCountWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributePacketTxCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePacketTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePacketTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributePacketTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePacketTxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketTxCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePacketTxCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePacketTxCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeTxErrCountWithCompletionHandler:@
readAttributeTxErrCountWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeTxErrCountWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeTxErrCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTxErrCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTxErrCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTxErrCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeTxErrCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTxErrCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTxErrCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTxErrCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTxErrCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCollisionCountWithCompletionHandler:@
readAttributeCollisionCountWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeCollisionCountWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeCollisionCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCollisionCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCollisionCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCollisionCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeCollisionCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCollisionCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCollisionCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCollisionCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCollisionCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOverrunCountWithCompletionHandler:@
readAttributeOverrunCountWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeOverrunCountWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCarrierDetectWithCompletionHandler:@
readAttributeCarrierDetectWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeCarrierDetectWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeCarrierDetectWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCarrierDetectWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCarrierDetectWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCarrierDetectWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeCarrierDetectWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCarrierDetectWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCarrierDetectWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCarrierDetectWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCarrierDetectWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeTimeSinceResetWithCompletionHandler:@
readAttributeTimeSinceResetWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeTimeSinceResetWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeTimeSinceResetWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTimeSinceResetWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSinceResetWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeSinceResetWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeTimeSinceResetWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeSinceResetWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTimeSinceResetWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeSinceResetWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeSinceResetWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics => mtrBaseClusterEthernetNetworkDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterEthernetNetworkDiagnostics  completionHandler =
    sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterEthernetNetworkDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterEthernetNetworkDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterEthernetNetworkDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterEthernetNetworkDiagnostics mtrBaseClusterEthernetNetworkDiagnostics, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterEthernetNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRBaseClusterEthernetNetworkDiagnostics)
initWithDevice_endpointID_queue mtrBaseClusterEthernetNetworkDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterEthernetNetworkDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:completion:@
resetCountsWithParams_completionSelector :: Selector
resetCountsWithParams_completionSelector = mkSelector "resetCountsWithParams:completion:"

-- | @Selector@ for @resetCountsWithCompletion:@
resetCountsWithCompletionSelector :: Selector
resetCountsWithCompletionSelector = mkSelector "resetCountsWithCompletion:"

-- | @Selector@ for @readAttributePHYRateWithCompletion:@
readAttributePHYRateWithCompletionSelector :: Selector
readAttributePHYRateWithCompletionSelector = mkSelector "readAttributePHYRateWithCompletion:"

-- | @Selector@ for @subscribeAttributePHYRateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePHYRateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePHYRateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePHYRateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePHYRateWithClusterStateCache:endpoint:queue:completion:@
readAttributePHYRateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePHYRateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePHYRateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFullDuplexWithCompletion:@
readAttributeFullDuplexWithCompletionSelector :: Selector
readAttributeFullDuplexWithCompletionSelector = mkSelector "readAttributeFullDuplexWithCompletion:"

-- | @Selector@ for @subscribeAttributeFullDuplexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFullDuplexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFullDuplexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFullDuplexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFullDuplexWithClusterStateCache:endpoint:queue:completion:@
readAttributeFullDuplexWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFullDuplexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFullDuplexWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePacketRxCountWithCompletion:@
readAttributePacketRxCountWithCompletionSelector :: Selector
readAttributePacketRxCountWithCompletionSelector = mkSelector "readAttributePacketRxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributePacketRxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketRxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketRxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketRxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketRxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketRxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePacketRxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePacketRxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePacketTxCountWithCompletion:@
readAttributePacketTxCountWithCompletionSelector :: Selector
readAttributePacketTxCountWithCompletionSelector = mkSelector "readAttributePacketTxCountWithCompletion:"

-- | @Selector@ for @subscribeAttributePacketTxCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePacketTxCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketTxCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketTxCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketTxCountWithClusterStateCache:endpoint:queue:completion:@
readAttributePacketTxCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePacketTxCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePacketTxCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTxErrCountWithCompletion:@
readAttributeTxErrCountWithCompletionSelector :: Selector
readAttributeTxErrCountWithCompletionSelector = mkSelector "readAttributeTxErrCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeTxErrCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTxErrCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTxErrCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTxErrCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTxErrCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeTxErrCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTxErrCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTxErrCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCollisionCountWithCompletion:@
readAttributeCollisionCountWithCompletionSelector :: Selector
readAttributeCollisionCountWithCompletionSelector = mkSelector "readAttributeCollisionCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeCollisionCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCollisionCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCollisionCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCollisionCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCollisionCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeCollisionCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCollisionCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCollisionCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverrunCountWithCompletion:@
readAttributeOverrunCountWithCompletionSelector :: Selector
readAttributeOverrunCountWithCompletionSelector = mkSelector "readAttributeOverrunCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverrunCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverrunCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOverrunCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverrunCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCarrierDetectWithCompletion:@
readAttributeCarrierDetectWithCompletionSelector :: Selector
readAttributeCarrierDetectWithCompletionSelector = mkSelector "readAttributeCarrierDetectWithCompletion:"

-- | @Selector@ for @subscribeAttributeCarrierDetectWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCarrierDetectWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCarrierDetectWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCarrierDetectWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCarrierDetectWithClusterStateCache:endpoint:queue:completion:@
readAttributeCarrierDetectWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCarrierDetectWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCarrierDetectWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeSinceResetWithCompletion:@
readAttributeTimeSinceResetWithCompletionSelector :: Selector
readAttributeTimeSinceResetWithCompletionSelector = mkSelector "readAttributeTimeSinceResetWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeSinceResetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSinceResetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeSinceResetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeSinceResetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeSinceResetWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeSinceResetWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimeSinceResetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeSinceResetWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributePHYRateWithCompletionHandler:@
readAttributePHYRateWithCompletionHandlerSelector :: Selector
readAttributePHYRateWithCompletionHandlerSelector = mkSelector "readAttributePHYRateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePHYRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePHYRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePHYRateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePHYRateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePHYRateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePHYRateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePHYRateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePHYRateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFullDuplexWithCompletionHandler:@
readAttributeFullDuplexWithCompletionHandlerSelector :: Selector
readAttributeFullDuplexWithCompletionHandlerSelector = mkSelector "readAttributeFullDuplexWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFullDuplexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFullDuplexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFullDuplexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFullDuplexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFullDuplexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFullDuplexWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFullDuplexWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFullDuplexWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePacketRxCountWithCompletionHandler:@
readAttributePacketRxCountWithCompletionHandlerSelector :: Selector
readAttributePacketRxCountWithCompletionHandlerSelector = mkSelector "readAttributePacketRxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePacketRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketRxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketRxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketRxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePacketRxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePacketRxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePacketTxCountWithCompletionHandler:@
readAttributePacketTxCountWithCompletionHandlerSelector :: Selector
readAttributePacketTxCountWithCompletionHandlerSelector = mkSelector "readAttributePacketTxCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePacketTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePacketTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePacketTxCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePacketTxCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePacketTxCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePacketTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePacketTxCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePacketTxCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTxErrCountWithCompletionHandler:@
readAttributeTxErrCountWithCompletionHandlerSelector :: Selector
readAttributeTxErrCountWithCompletionHandlerSelector = mkSelector "readAttributeTxErrCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTxErrCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTxErrCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTxErrCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTxErrCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTxErrCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTxErrCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTxErrCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTxErrCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCollisionCountWithCompletionHandler:@
readAttributeCollisionCountWithCompletionHandlerSelector :: Selector
readAttributeCollisionCountWithCompletionHandlerSelector = mkSelector "readAttributeCollisionCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCollisionCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCollisionCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCollisionCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCollisionCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCollisionCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCollisionCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCollisionCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCollisionCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOverrunCountWithCompletionHandler:@
readAttributeOverrunCountWithCompletionHandlerSelector :: Selector
readAttributeOverrunCountWithCompletionHandlerSelector = mkSelector "readAttributeOverrunCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverrunCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverrunCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOverrunCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOverrunCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCarrierDetectWithCompletionHandler:@
readAttributeCarrierDetectWithCompletionHandlerSelector :: Selector
readAttributeCarrierDetectWithCompletionHandlerSelector = mkSelector "readAttributeCarrierDetectWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCarrierDetectWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCarrierDetectWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCarrierDetectWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCarrierDetectWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCarrierDetectWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCarrierDetectWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCarrierDetectWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCarrierDetectWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTimeSinceResetWithCompletionHandler:@
readAttributeTimeSinceResetWithCompletionHandlerSelector :: Selector
readAttributeTimeSinceResetWithCompletionHandlerSelector = mkSelector "readAttributeTimeSinceResetWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTimeSinceResetWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSinceResetWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeSinceResetWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeSinceResetWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeSinceResetWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTimeSinceResetWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTimeSinceResetWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTimeSinceResetWithAttributeCache:endpoint:queue:completionHandler:"

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

