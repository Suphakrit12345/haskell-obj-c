{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster General Diagnostics
--
-- The General Diagnostics Cluster, along with other diagnostics clusters, provide a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRBaseClusterGeneralDiagnostics@.
module ObjC.Matter.MTRBaseClusterGeneralDiagnostics
  ( MTRBaseClusterGeneralDiagnostics
  , IsMTRBaseClusterGeneralDiagnostics(..)
  , testEventTriggerWithParams_completion
  , timeSnapshotWithParams_completion
  , timeSnapshotWithCompletion
  , payloadTestRequestWithParams_completion
  , readAttributeNetworkInterfacesWithCompletion
  , subscribeAttributeNetworkInterfacesWithParams_subscriptionEstablished_reportHandler
  , readAttributeNetworkInterfacesWithClusterStateCache_endpoint_queue_completion
  , readAttributeRebootCountWithCompletion
  , subscribeAttributeRebootCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeRebootCountWithClusterStateCache_endpoint_queue_completion
  , readAttributeUpTimeWithCompletion
  , subscribeAttributeUpTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeUpTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTotalOperationalHoursWithCompletion
  , subscribeAttributeTotalOperationalHoursWithParams_subscriptionEstablished_reportHandler
  , readAttributeTotalOperationalHoursWithClusterStateCache_endpoint_queue_completion
  , readAttributeBootReasonWithCompletion
  , subscribeAttributeBootReasonWithParams_subscriptionEstablished_reportHandler
  , readAttributeBootReasonWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveHardwareFaultsWithCompletion
  , subscribeAttributeActiveHardwareFaultsWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveHardwareFaultsWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveRadioFaultsWithCompletion
  , subscribeAttributeActiveRadioFaultsWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveRadioFaultsWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveNetworkFaultsWithCompletion
  , subscribeAttributeActiveNetworkFaultsWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveNetworkFaultsWithClusterStateCache_endpoint_queue_completion
  , readAttributeTestEventTriggersEnabledWithCompletion
  , subscribeAttributeTestEventTriggersEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeTestEventTriggersEnabledWithClusterStateCache_endpoint_queue_completion
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
  , testEventTriggerWithParams_completionHandler
  , readAttributeNetworkInterfacesWithCompletionHandler
  , subscribeAttributeNetworkInterfacesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNetworkInterfacesWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeRebootCountWithCompletionHandler
  , subscribeAttributeRebootCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeRebootCountWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeUpTimeWithCompletionHandler
  , subscribeAttributeUpTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeUpTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTotalOperationalHoursWithCompletionHandler
  , subscribeAttributeTotalOperationalHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTotalOperationalHoursWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBootReasonsWithCompletionHandler
  , subscribeAttributeBootReasonsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBootReasonsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeActiveHardwareFaultsWithCompletionHandler
  , subscribeAttributeActiveHardwareFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveHardwareFaultsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeActiveRadioFaultsWithCompletionHandler
  , subscribeAttributeActiveRadioFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveRadioFaultsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeActiveNetworkFaultsWithCompletionHandler
  , subscribeAttributeActiveNetworkFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveNetworkFaultsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTestEventTriggersEnabledWithCompletionHandler
  , subscribeAttributeTestEventTriggersEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTestEventTriggersEnabledWithAttributeCache_endpoint_queue_completionHandler
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
  , testEventTriggerWithParams_completionSelector
  , timeSnapshotWithParams_completionSelector
  , timeSnapshotWithCompletionSelector
  , payloadTestRequestWithParams_completionSelector
  , readAttributeNetworkInterfacesWithCompletionSelector
  , subscribeAttributeNetworkInterfacesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNetworkInterfacesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRebootCountWithCompletionSelector
  , subscribeAttributeRebootCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRebootCountWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUpTimeWithCompletionSelector
  , subscribeAttributeUpTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUpTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTotalOperationalHoursWithCompletionSelector
  , subscribeAttributeTotalOperationalHoursWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTotalOperationalHoursWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBootReasonWithCompletionSelector
  , subscribeAttributeBootReasonWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBootReasonWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveHardwareFaultsWithCompletionSelector
  , subscribeAttributeActiveHardwareFaultsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveHardwareFaultsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveRadioFaultsWithCompletionSelector
  , subscribeAttributeActiveRadioFaultsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveRadioFaultsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveNetworkFaultsWithCompletionSelector
  , subscribeAttributeActiveNetworkFaultsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveNetworkFaultsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTestEventTriggersEnabledWithCompletionSelector
  , subscribeAttributeTestEventTriggersEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTestEventTriggersEnabledWithClusterStateCache_endpoint_queue_completionSelector
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
  , testEventTriggerWithParams_completionHandlerSelector
  , readAttributeNetworkInterfacesWithCompletionHandlerSelector
  , subscribeAttributeNetworkInterfacesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeNetworkInterfacesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeRebootCountWithCompletionHandlerSelector
  , subscribeAttributeRebootCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeRebootCountWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeUpTimeWithCompletionHandlerSelector
  , subscribeAttributeUpTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeUpTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTotalOperationalHoursWithCompletionHandlerSelector
  , subscribeAttributeTotalOperationalHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTotalOperationalHoursWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBootReasonsWithCompletionHandlerSelector
  , subscribeAttributeBootReasonsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBootReasonsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActiveHardwareFaultsWithCompletionHandlerSelector
  , subscribeAttributeActiveHardwareFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveHardwareFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActiveRadioFaultsWithCompletionHandlerSelector
  , subscribeAttributeActiveRadioFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveRadioFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActiveNetworkFaultsWithCompletionHandlerSelector
  , subscribeAttributeActiveNetworkFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveNetworkFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTestEventTriggersEnabledWithCompletionHandlerSelector
  , subscribeAttributeTestEventTriggersEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTestEventTriggersEnabledWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command TestEventTrigger
--
-- Provide a means for certification tests to trigger some test-plan-specific events
--
-- ObjC selector: @- testEventTriggerWithParams:completion:@
testEventTriggerWithParams_completion :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterTestEventTriggerParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> IO ()
testEventTriggerWithParams_completion mtrBaseClusterGeneralDiagnostics  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "testEventTriggerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command TimeSnapshot
--
-- Take a snapshot of system time and epoch time.
--
-- ObjC selector: @- timeSnapshotWithParams:completion:@
timeSnapshotWithParams_completion :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterTimeSnapshotParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> IO ()
timeSnapshotWithParams_completion mtrBaseClusterGeneralDiagnostics  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "timeSnapshotWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- timeSnapshotWithCompletion:@
timeSnapshotWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
timeSnapshotWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "timeSnapshotWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command PayloadTestRequest
--
-- Request a variable length payload response.
--
-- ObjC selector: @- payloadTestRequestWithParams:completion:@
payloadTestRequestWithParams_completion :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> IO ()
payloadTestRequestWithParams_completion mtrBaseClusterGeneralDiagnostics  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "payloadTestRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNetworkInterfacesWithCompletion:@
readAttributeNetworkInterfacesWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeNetworkInterfacesWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeNetworkInterfacesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNetworkInterfacesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworkInterfacesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNetworkInterfacesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeNetworkInterfacesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNetworkInterfacesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNetworkInterfacesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNetworkInterfacesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNetworkInterfacesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRebootCountWithCompletion:@
readAttributeRebootCountWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeRebootCountWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeRebootCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRebootCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRebootCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRebootCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeRebootCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRebootCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeRebootCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRebootCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRebootCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUpTimeWithCompletion:@
readAttributeUpTimeWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeUpTimeWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeUpTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUpTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeUpTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUpTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUpTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTotalOperationalHoursWithCompletion:@
readAttributeTotalOperationalHoursWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeTotalOperationalHoursWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeTotalOperationalHoursWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTotalOperationalHoursWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTotalOperationalHoursWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTotalOperationalHoursWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeTotalOperationalHoursWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTotalOperationalHoursWithClusterStateCache:endpoint:queue:completion:@
readAttributeTotalOperationalHoursWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTotalOperationalHoursWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTotalOperationalHoursWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBootReasonWithCompletion:@
readAttributeBootReasonWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeBootReasonWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeBootReasonWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBootReasonWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBootReasonWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBootReasonWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeBootReasonWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBootReasonWithClusterStateCache:endpoint:queue:completion:@
readAttributeBootReasonWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBootReasonWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBootReasonWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveHardwareFaultsWithCompletion:@
readAttributeActiveHardwareFaultsWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeActiveHardwareFaultsWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeActiveHardwareFaultsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveHardwareFaultsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveHardwareFaultsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveHardwareFaultsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeActiveHardwareFaultsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveHardwareFaultsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveHardwareFaultsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveHardwareFaultsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveHardwareFaultsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveRadioFaultsWithCompletion:@
readAttributeActiveRadioFaultsWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeActiveRadioFaultsWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeActiveRadioFaultsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveRadioFaultsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveRadioFaultsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveRadioFaultsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeActiveRadioFaultsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveRadioFaultsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveRadioFaultsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveRadioFaultsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveRadioFaultsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveNetworkFaultsWithCompletion:@
readAttributeActiveNetworkFaultsWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeActiveNetworkFaultsWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeActiveNetworkFaultsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveNetworkFaultsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveNetworkFaultsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveNetworkFaultsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeActiveNetworkFaultsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveNetworkFaultsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveNetworkFaultsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveNetworkFaultsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveNetworkFaultsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTestEventTriggersEnabledWithCompletion:@
readAttributeTestEventTriggersEnabledWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeTestEventTriggersEnabledWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeTestEventTriggersEnabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTestEventTriggersEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTestEventTriggersEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTestEventTriggersEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeTestEventTriggersEnabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTestEventTriggersEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeTestEventTriggersEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTestEventTriggersEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTestEventTriggersEnabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGeneralDiagnostics  completion =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> IO (Id MTRBaseClusterGeneralDiagnostics)
init_ mtrBaseClusterGeneralDiagnostics  =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterGeneralDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterGeneralDiagnostics -> device -> CUShort -> queue -> IO (Id MTRBaseClusterGeneralDiagnostics)
initWithDevice_endpoint_queue mtrBaseClusterGeneralDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- testEventTriggerWithParams:completionHandler:@
testEventTriggerWithParams_completionHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterTestEventTriggerParams params) => mtrBaseClusterGeneralDiagnostics -> params -> Ptr () -> IO ()
testEventTriggerWithParams_completionHandler mtrBaseClusterGeneralDiagnostics  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "testEventTriggerWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeNetworkInterfacesWithCompletionHandler:@
readAttributeNetworkInterfacesWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeNetworkInterfacesWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeNetworkInterfacesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeNetworkInterfacesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworkInterfacesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNetworkInterfacesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeNetworkInterfacesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNetworkInterfacesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNetworkInterfacesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNetworkInterfacesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNetworkInterfacesWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeRebootCountWithCompletionHandler:@
readAttributeRebootCountWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeRebootCountWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeRebootCountWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeRebootCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRebootCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRebootCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeRebootCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRebootCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRebootCountWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRebootCountWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRebootCountWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeUpTimeWithCompletionHandler:@
readAttributeUpTimeWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeUpTimeWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeUpTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeUpTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUpTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeUpTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUpTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUpTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUpTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeTotalOperationalHoursWithCompletionHandler:@
readAttributeTotalOperationalHoursWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeTotalOperationalHoursWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeTotalOperationalHoursWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTotalOperationalHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTotalOperationalHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTotalOperationalHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeTotalOperationalHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTotalOperationalHoursWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTotalOperationalHoursWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTotalOperationalHoursWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTotalOperationalHoursWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBootReasonsWithCompletionHandler:@
readAttributeBootReasonsWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeBootReasonsWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeBootReasonsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBootReasonsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBootReasonsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBootReasonsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeBootReasonsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBootReasonsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBootReasonsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBootReasonsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBootReasonsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeActiveHardwareFaultsWithCompletionHandler:@
readAttributeActiveHardwareFaultsWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeActiveHardwareFaultsWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeActiveHardwareFaultsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeActiveHardwareFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveHardwareFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveHardwareFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeActiveHardwareFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveHardwareFaultsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveHardwareFaultsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveHardwareFaultsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveHardwareFaultsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeActiveRadioFaultsWithCompletionHandler:@
readAttributeActiveRadioFaultsWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeActiveRadioFaultsWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeActiveRadioFaultsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeActiveRadioFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveRadioFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveRadioFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeActiveRadioFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveRadioFaultsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveRadioFaultsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveRadioFaultsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveRadioFaultsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeActiveNetworkFaultsWithCompletionHandler:@
readAttributeActiveNetworkFaultsWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeActiveNetworkFaultsWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeActiveNetworkFaultsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeActiveNetworkFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveNetworkFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveNetworkFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeActiveNetworkFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveNetworkFaultsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveNetworkFaultsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveNetworkFaultsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveNetworkFaultsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeTestEventTriggersEnabledWithCompletionHandler:@
readAttributeTestEventTriggersEnabledWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeTestEventTriggersEnabledWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeTestEventTriggersEnabledWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTestEventTriggersEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTestEventTriggersEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTestEventTriggersEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeTestEventTriggersEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTestEventTriggersEnabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTestEventTriggersEnabledWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTestEventTriggersEnabledWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTestEventTriggersEnabledWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics => mtrBaseClusterGeneralDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterGeneralDiagnostics  completionHandler =
    sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGeneralDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGeneralDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGeneralDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGeneralDiagnostics mtrBaseClusterGeneralDiagnostics, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGeneralDiagnostics -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGeneralDiagnostics)
initWithDevice_endpointID_queue mtrBaseClusterGeneralDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterGeneralDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @testEventTriggerWithParams:completion:@
testEventTriggerWithParams_completionSelector :: Selector
testEventTriggerWithParams_completionSelector = mkSelector "testEventTriggerWithParams:completion:"

-- | @Selector@ for @timeSnapshotWithParams:completion:@
timeSnapshotWithParams_completionSelector :: Selector
timeSnapshotWithParams_completionSelector = mkSelector "timeSnapshotWithParams:completion:"

-- | @Selector@ for @timeSnapshotWithCompletion:@
timeSnapshotWithCompletionSelector :: Selector
timeSnapshotWithCompletionSelector = mkSelector "timeSnapshotWithCompletion:"

-- | @Selector@ for @payloadTestRequestWithParams:completion:@
payloadTestRequestWithParams_completionSelector :: Selector
payloadTestRequestWithParams_completionSelector = mkSelector "payloadTestRequestWithParams:completion:"

-- | @Selector@ for @readAttributeNetworkInterfacesWithCompletion:@
readAttributeNetworkInterfacesWithCompletionSelector :: Selector
readAttributeNetworkInterfacesWithCompletionSelector = mkSelector "readAttributeNetworkInterfacesWithCompletion:"

-- | @Selector@ for @subscribeAttributeNetworkInterfacesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworkInterfacesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNetworkInterfacesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNetworkInterfacesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNetworkInterfacesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNetworkInterfacesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNetworkInterfacesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNetworkInterfacesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRebootCountWithCompletion:@
readAttributeRebootCountWithCompletionSelector :: Selector
readAttributeRebootCountWithCompletionSelector = mkSelector "readAttributeRebootCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeRebootCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRebootCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRebootCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRebootCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRebootCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeRebootCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRebootCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRebootCountWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUpTimeWithCompletion:@
readAttributeUpTimeWithCompletionSelector :: Selector
readAttributeUpTimeWithCompletionSelector = mkSelector "readAttributeUpTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeUpTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUpTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUpTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUpTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUpTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUpTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTotalOperationalHoursWithCompletion:@
readAttributeTotalOperationalHoursWithCompletionSelector :: Selector
readAttributeTotalOperationalHoursWithCompletionSelector = mkSelector "readAttributeTotalOperationalHoursWithCompletion:"

-- | @Selector@ for @subscribeAttributeTotalOperationalHoursWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTotalOperationalHoursWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTotalOperationalHoursWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTotalOperationalHoursWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTotalOperationalHoursWithClusterStateCache:endpoint:queue:completion:@
readAttributeTotalOperationalHoursWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTotalOperationalHoursWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTotalOperationalHoursWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBootReasonWithCompletion:@
readAttributeBootReasonWithCompletionSelector :: Selector
readAttributeBootReasonWithCompletionSelector = mkSelector "readAttributeBootReasonWithCompletion:"

-- | @Selector@ for @subscribeAttributeBootReasonWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBootReasonWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBootReasonWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBootReasonWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBootReasonWithClusterStateCache:endpoint:queue:completion:@
readAttributeBootReasonWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBootReasonWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBootReasonWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveHardwareFaultsWithCompletion:@
readAttributeActiveHardwareFaultsWithCompletionSelector :: Selector
readAttributeActiveHardwareFaultsWithCompletionSelector = mkSelector "readAttributeActiveHardwareFaultsWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveHardwareFaultsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveHardwareFaultsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveHardwareFaultsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveHardwareFaultsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveHardwareFaultsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveHardwareFaultsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveHardwareFaultsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveHardwareFaultsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveRadioFaultsWithCompletion:@
readAttributeActiveRadioFaultsWithCompletionSelector :: Selector
readAttributeActiveRadioFaultsWithCompletionSelector = mkSelector "readAttributeActiveRadioFaultsWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveRadioFaultsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveRadioFaultsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveRadioFaultsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveRadioFaultsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveRadioFaultsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveRadioFaultsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveRadioFaultsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveRadioFaultsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsWithCompletion:@
readAttributeActiveNetworkFaultsWithCompletionSelector :: Selector
readAttributeActiveNetworkFaultsWithCompletionSelector = mkSelector "readAttributeActiveNetworkFaultsWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveNetworkFaultsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveNetworkFaultsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveNetworkFaultsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveNetworkFaultsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveNetworkFaultsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveNetworkFaultsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveNetworkFaultsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTestEventTriggersEnabledWithCompletion:@
readAttributeTestEventTriggersEnabledWithCompletionSelector :: Selector
readAttributeTestEventTriggersEnabledWithCompletionSelector = mkSelector "readAttributeTestEventTriggersEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeTestEventTriggersEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTestEventTriggersEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTestEventTriggersEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTestEventTriggersEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTestEventTriggersEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeTestEventTriggersEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTestEventTriggersEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTestEventTriggersEnabledWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @testEventTriggerWithParams:completionHandler:@
testEventTriggerWithParams_completionHandlerSelector :: Selector
testEventTriggerWithParams_completionHandlerSelector = mkSelector "testEventTriggerWithParams:completionHandler:"

-- | @Selector@ for @readAttributeNetworkInterfacesWithCompletionHandler:@
readAttributeNetworkInterfacesWithCompletionHandlerSelector :: Selector
readAttributeNetworkInterfacesWithCompletionHandlerSelector = mkSelector "readAttributeNetworkInterfacesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeNetworkInterfacesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNetworkInterfacesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNetworkInterfacesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNetworkInterfacesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNetworkInterfacesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNetworkInterfacesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeNetworkInterfacesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNetworkInterfacesWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeRebootCountWithCompletionHandler:@
readAttributeRebootCountWithCompletionHandlerSelector :: Selector
readAttributeRebootCountWithCompletionHandlerSelector = mkSelector "readAttributeRebootCountWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeRebootCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeRebootCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRebootCountWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRebootCountWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRebootCountWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeRebootCountWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeRebootCountWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeRebootCountWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeUpTimeWithCompletionHandler:@
readAttributeUpTimeWithCompletionHandlerSelector :: Selector
readAttributeUpTimeWithCompletionHandlerSelector = mkSelector "readAttributeUpTimeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeUpTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUpTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUpTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUpTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUpTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUpTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeUpTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeUpTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTotalOperationalHoursWithCompletionHandler:@
readAttributeTotalOperationalHoursWithCompletionHandlerSelector :: Selector
readAttributeTotalOperationalHoursWithCompletionHandlerSelector = mkSelector "readAttributeTotalOperationalHoursWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTotalOperationalHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTotalOperationalHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTotalOperationalHoursWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTotalOperationalHoursWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTotalOperationalHoursWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTotalOperationalHoursWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTotalOperationalHoursWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTotalOperationalHoursWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBootReasonsWithCompletionHandler:@
readAttributeBootReasonsWithCompletionHandlerSelector :: Selector
readAttributeBootReasonsWithCompletionHandlerSelector = mkSelector "readAttributeBootReasonsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBootReasonsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBootReasonsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBootReasonsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBootReasonsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBootReasonsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBootReasonsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBootReasonsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBootReasonsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeActiveHardwareFaultsWithCompletionHandler:@
readAttributeActiveHardwareFaultsWithCompletionHandlerSelector :: Selector
readAttributeActiveHardwareFaultsWithCompletionHandlerSelector = mkSelector "readAttributeActiveHardwareFaultsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeActiveHardwareFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveHardwareFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveHardwareFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveHardwareFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveHardwareFaultsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveHardwareFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeActiveHardwareFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveHardwareFaultsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeActiveRadioFaultsWithCompletionHandler:@
readAttributeActiveRadioFaultsWithCompletionHandlerSelector :: Selector
readAttributeActiveRadioFaultsWithCompletionHandlerSelector = mkSelector "readAttributeActiveRadioFaultsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeActiveRadioFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveRadioFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveRadioFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveRadioFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveRadioFaultsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveRadioFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeActiveRadioFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveRadioFaultsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsWithCompletionHandler:@
readAttributeActiveNetworkFaultsWithCompletionHandlerSelector :: Selector
readAttributeActiveNetworkFaultsWithCompletionHandlerSelector = mkSelector "readAttributeActiveNetworkFaultsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeActiveNetworkFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveNetworkFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveNetworkFaultsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveNetworkFaultsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveNetworkFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeActiveNetworkFaultsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveNetworkFaultsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTestEventTriggersEnabledWithCompletionHandler:@
readAttributeTestEventTriggersEnabledWithCompletionHandlerSelector :: Selector
readAttributeTestEventTriggersEnabledWithCompletionHandlerSelector = mkSelector "readAttributeTestEventTriggersEnabledWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTestEventTriggersEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTestEventTriggersEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTestEventTriggersEnabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTestEventTriggersEnabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTestEventTriggersEnabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTestEventTriggersEnabledWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTestEventTriggersEnabledWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTestEventTriggersEnabledWithAttributeCache:endpoint:queue:completionHandler:"

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

