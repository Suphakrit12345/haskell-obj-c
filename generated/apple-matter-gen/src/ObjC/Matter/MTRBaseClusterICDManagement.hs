{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster ICD Management
--
-- Allows servers to ensure that listed clients are notified when a server is available for communication.
--
-- Generated bindings for @MTRBaseClusterICDManagement@.
module ObjC.Matter.MTRBaseClusterICDManagement
  ( MTRBaseClusterICDManagement
  , IsMTRBaseClusterICDManagement(..)
  , registerClientWithParams_completion
  , unregisterClientWithParams_completion
  , stayActiveRequestWithParams_completion
  , readAttributeIdleModeDurationWithCompletion
  , subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveModeDurationWithCompletion
  , subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveModeThresholdWithCompletion
  , subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completion
  , readAttributeRegisteredClientsWithParams_completion
  , subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandler
  , readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completion
  , readAttributeICDCounterWithCompletion
  , subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandler
  , readAttributeICDCounterWithClusterStateCache_endpoint_queue_completion
  , readAttributeClientsSupportedPerFabricWithCompletion
  , subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completion
  , readAttributeUserActiveModeTriggerHintWithCompletion
  , subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandler
  , readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completion
  , readAttributeUserActiveModeTriggerInstructionWithCompletion
  , subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandler
  , readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperatingModeWithCompletion
  , subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumCheckInBackOffWithCompletion
  , subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpointID_queue
  , registerClientWithParams_completionSelector
  , unregisterClientWithParams_completionSelector
  , stayActiveRequestWithParams_completionSelector
  , readAttributeIdleModeDurationWithCompletionSelector
  , subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveModeDurationWithCompletionSelector
  , subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveModeThresholdWithCompletionSelector
  , subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRegisteredClientsWithParams_completionSelector
  , subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeICDCounterWithCompletionSelector
  , subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClientsSupportedPerFabricWithCompletionSelector
  , subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUserActiveModeTriggerHintWithCompletionSelector
  , subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUserActiveModeTriggerInstructionWithCompletionSelector
  , subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperatingModeWithCompletionSelector
  , subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumCheckInBackOffWithCompletionSelector
  , subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command RegisterClient
--
-- This command allows a client to register itself with the ICD to be notified when the device is available for communication.
--
-- ObjC selector: @- registerClientWithParams:completion:@
registerClientWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRICDManagementClusterRegisterClientParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
registerClientWithParams_completion mtrBaseClusterICDManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "registerClientWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UnregisterClient
--
-- This command allows a client to unregister itself with the ICD.
--
-- ObjC selector: @- unregisterClientWithParams:completion:@
unregisterClientWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRICDManagementClusterUnregisterClientParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
unregisterClientWithParams_completion mtrBaseClusterICDManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "unregisterClientWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StayActiveRequest
--
-- This command allows a client to request that the server stays in active mode for at least a given time duration (in milliseconds) from when this command is received.
--
-- ObjC selector: @- stayActiveRequestWithParams:completion:@
stayActiveRequestWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRICDManagementClusterStayActiveRequestParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
stayActiveRequestWithParams_completion mtrBaseClusterICDManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "stayActiveRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeIdleModeDurationWithCompletion:@
readAttributeIdleModeDurationWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeIdleModeDurationWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeIdleModeDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveModeDurationWithCompletion:@
readAttributeActiveModeDurationWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeActiveModeDurationWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeActiveModeDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveModeThresholdWithCompletion:@
readAttributeActiveModeThresholdWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeActiveModeThresholdWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeActiveModeThresholdWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRegisteredClientsWithParams:completion:@
readAttributeRegisteredClientsWithParams_completion :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRReadParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> IO ()
readAttributeRegisteredClientsWithParams_completion mtrBaseClusterICDManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeRegisteredClientsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:@
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeICDCounterWithCompletion:@
readAttributeICDCounterWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeICDCounterWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeICDCounterWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:@
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClientsSupportedPerFabricWithCompletion:@
readAttributeClientsSupportedPerFabricWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeClientsSupportedPerFabricWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeClientsSupportedPerFabricWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUserActiveModeTriggerHintWithCompletion:@
readAttributeUserActiveModeTriggerHintWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerHintWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeUserActiveModeTriggerHintWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUserActiveModeTriggerInstructionWithCompletion:@
readAttributeUserActiveModeTriggerInstructionWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerInstructionWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeUserActiveModeTriggerInstructionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOperatingModeWithCompletion:@
readAttributeOperatingModeWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeOperatingModeWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeOperatingModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaximumCheckInBackOffWithCompletion:@
readAttributeMaximumCheckInBackOffWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeMaximumCheckInBackOffWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeMaximumCheckInBackOffWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterICDManagement  completion =
    sendMsg mtrBaseClusterICDManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRSubscribeParams params) => mtrBaseClusterICDManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterICDManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterICDManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement => mtrBaseClusterICDManagement -> IO (Id MTRBaseClusterICDManagement)
init_ mtrBaseClusterICDManagement  =
    sendMsg mtrBaseClusterICDManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterICDManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterICDManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterICDManagement mtrBaseClusterICDManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterICDManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterICDManagement)
initWithDevice_endpointID_queue mtrBaseClusterICDManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterICDManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerClientWithParams:completion:@
registerClientWithParams_completionSelector :: Selector
registerClientWithParams_completionSelector = mkSelector "registerClientWithParams:completion:"

-- | @Selector@ for @unregisterClientWithParams:completion:@
unregisterClientWithParams_completionSelector :: Selector
unregisterClientWithParams_completionSelector = mkSelector "unregisterClientWithParams:completion:"

-- | @Selector@ for @stayActiveRequestWithParams:completion:@
stayActiveRequestWithParams_completionSelector :: Selector
stayActiveRequestWithParams_completionSelector = mkSelector "stayActiveRequestWithParams:completion:"

-- | @Selector@ for @readAttributeIdleModeDurationWithCompletion:@
readAttributeIdleModeDurationWithCompletionSelector :: Selector
readAttributeIdleModeDurationWithCompletionSelector = mkSelector "readAttributeIdleModeDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeIdleModeDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIdleModeDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeIdleModeDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIdleModeDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveModeDurationWithCompletion:@
readAttributeActiveModeDurationWithCompletionSelector :: Selector
readAttributeActiveModeDurationWithCompletionSelector = mkSelector "readAttributeActiveModeDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveModeDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveModeDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveModeDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveModeDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveModeThresholdWithCompletion:@
readAttributeActiveModeThresholdWithCompletionSelector :: Selector
readAttributeActiveModeThresholdWithCompletionSelector = mkSelector "readAttributeActiveModeThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveModeThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveModeThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveModeThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveModeThresholdWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRegisteredClientsWithParams:completion:@
readAttributeRegisteredClientsWithParams_completionSelector :: Selector
readAttributeRegisteredClientsWithParams_completionSelector = mkSelector "readAttributeRegisteredClientsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRegisteredClientsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRegisteredClientsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:@
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRegisteredClientsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRegisteredClientsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeICDCounterWithCompletion:@
readAttributeICDCounterWithCompletionSelector :: Selector
readAttributeICDCounterWithCompletionSelector = mkSelector "readAttributeICDCounterWithCompletion:"

-- | @Selector@ for @subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeICDCounterWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeICDCounterWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:@
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeICDCounterWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeICDCounterWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClientsSupportedPerFabricWithCompletion:@
readAttributeClientsSupportedPerFabricWithCompletionSelector :: Selector
readAttributeClientsSupportedPerFabricWithCompletionSelector = mkSelector "readAttributeClientsSupportedPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClientsSupportedPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClientsSupportedPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeClientsSupportedPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClientsSupportedPerFabricWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerHintWithCompletion:@
readAttributeUserActiveModeTriggerHintWithCompletionSelector :: Selector
readAttributeUserActiveModeTriggerHintWithCompletionSelector = mkSelector "readAttributeUserActiveModeTriggerHintWithCompletion:"

-- | @Selector@ for @subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUserActiveModeTriggerHintWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUserActiveModeTriggerHintWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUserActiveModeTriggerHintWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUserActiveModeTriggerHintWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerInstructionWithCompletion:@
readAttributeUserActiveModeTriggerInstructionWithCompletionSelector :: Selector
readAttributeUserActiveModeTriggerInstructionWithCompletionSelector = mkSelector "readAttributeUserActiveModeTriggerInstructionWithCompletion:"

-- | @Selector@ for @subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUserActiveModeTriggerInstructionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUserActiveModeTriggerInstructionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUserActiveModeTriggerInstructionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUserActiveModeTriggerInstructionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperatingModeWithCompletion:@
readAttributeOperatingModeWithCompletionSelector :: Selector
readAttributeOperatingModeWithCompletionSelector = mkSelector "readAttributeOperatingModeWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOperatingModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperatingModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOperatingModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperatingModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumCheckInBackOffWithCompletion:@
readAttributeMaximumCheckInBackOffWithCompletionSelector :: Selector
readAttributeMaximumCheckInBackOffWithCompletionSelector = mkSelector "readAttributeMaximumCheckInBackOffWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaximumCheckInBackOffWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumCheckInBackOffWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaximumCheckInBackOffWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumCheckInBackOffWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

