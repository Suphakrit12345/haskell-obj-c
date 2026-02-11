{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Activated Carbon Filter Monitoring
--
-- Attributes and commands for monitoring activated carbon filters in a device
--
-- Generated bindings for @MTRBaseClusterActivatedCarbonFilterMonitoring@.
module ObjC.Matter.MTRBaseClusterActivatedCarbonFilterMonitoring
  ( MTRBaseClusterActivatedCarbonFilterMonitoring
  , IsMTRBaseClusterActivatedCarbonFilterMonitoring(..)
  , resetConditionWithParams_completion
  , resetConditionWithCompletion
  , readAttributeConditionWithCompletion
  , subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler
  , readAttributeConditionWithClusterStateCache_endpoint_queue_completion
  , readAttributeDegradationDirectionWithCompletion
  , subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion
  , readAttributeChangeIndicationWithCompletion
  , subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler
  , readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion
  , readAttributeInPlaceIndicatorWithCompletion
  , subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler
  , readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion
  , readAttributeLastChangedTimeWithCompletion
  , writeAttributeLastChangedTimeWithValue_completion
  , writeAttributeLastChangedTimeWithValue_params_completion
  , subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeReplacementProductListWithCompletion
  , subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler
  , readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion
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
  , resetConditionWithParams_completionSelector
  , resetConditionWithCompletionSelector
  , readAttributeConditionWithCompletionSelector
  , subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDegradationDirectionWithCompletionSelector
  , subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeChangeIndicationWithCompletionSelector
  , subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInPlaceIndicatorWithCompletionSelector
  , subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLastChangedTimeWithCompletionSelector
  , writeAttributeLastChangedTimeWithValue_completionSelector
  , writeAttributeLastChangedTimeWithValue_params_completionSelector
  , subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReplacementProductListWithCompletionSelector
  , subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command ResetCondition
--
-- Reset the condition of the replaceable to the non degraded state
--
-- ObjC selector: @- resetConditionWithParams:completion:@
resetConditionWithParams_completion :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRActivatedCarbonFilterMonitoringClusterResetConditionParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> IO ()
resetConditionWithParams_completion mtrBaseClusterActivatedCarbonFilterMonitoring  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "resetConditionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetConditionWithCompletion:@
resetConditionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
resetConditionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "resetConditionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeConditionWithCompletion:@
readAttributeConditionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeConditionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeConditionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeConditionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConditionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConditionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeConditionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDegradationDirectionWithCompletion:@
readAttributeDegradationDirectionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeDegradationDirectionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeDegradationDirectionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeChangeIndicationWithCompletion:@
readAttributeChangeIndicationWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeChangeIndicationWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeChangeIndicationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInPlaceIndicatorWithCompletion:@
readAttributeInPlaceIndicatorWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeInPlaceIndicatorWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeInPlaceIndicatorWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:@
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLastChangedTimeWithCompletion:@
readAttributeLastChangedTimeWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeLastChangedTimeWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeLastChangedTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLastChangedTimeWithValue:completion:@
writeAttributeLastChangedTimeWithValue_completion :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsNSNumber value) => mtrBaseClusterActivatedCarbonFilterMonitoring -> value -> Ptr () -> IO ()
writeAttributeLastChangedTimeWithValue_completion mtrBaseClusterActivatedCarbonFilterMonitoring  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "writeAttributeLastChangedTimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeLastChangedTimeWithValue:params:completion:@
writeAttributeLastChangedTimeWithValue_params_completion :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> value -> params -> Ptr () -> IO ()
writeAttributeLastChangedTimeWithValue_params_completion mtrBaseClusterActivatedCarbonFilterMonitoring  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "writeAttributeLastChangedTimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeReplacementProductListWithCompletion:@
readAttributeReplacementProductListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeReplacementProductListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeReplacementProductListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:@
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterActivatedCarbonFilterMonitoring  completion =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRSubscribeParams params) => mtrBaseClusterActivatedCarbonFilterMonitoring -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActivatedCarbonFilterMonitoring  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring => mtrBaseClusterActivatedCarbonFilterMonitoring -> IO (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
init_ mtrBaseClusterActivatedCarbonFilterMonitoring  =
    sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterActivatedCarbonFilterMonitoring"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterActivatedCarbonFilterMonitoring mtrBaseClusterActivatedCarbonFilterMonitoring, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterActivatedCarbonFilterMonitoring -> device -> endpointID -> queue -> IO (Id MTRBaseClusterActivatedCarbonFilterMonitoring)
initWithDevice_endpointID_queue mtrBaseClusterActivatedCarbonFilterMonitoring  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterActivatedCarbonFilterMonitoring (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetConditionWithParams:completion:@
resetConditionWithParams_completionSelector :: Selector
resetConditionWithParams_completionSelector = mkSelector "resetConditionWithParams:completion:"

-- | @Selector@ for @resetConditionWithCompletion:@
resetConditionWithCompletionSelector :: Selector
resetConditionWithCompletionSelector = mkSelector "resetConditionWithCompletion:"

-- | @Selector@ for @readAttributeConditionWithCompletion:@
readAttributeConditionWithCompletionSelector :: Selector
readAttributeConditionWithCompletionSelector = mkSelector "readAttributeConditionWithCompletion:"

-- | @Selector@ for @subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeConditionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeConditionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeConditionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeConditionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeConditionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDegradationDirectionWithCompletion:@
readAttributeDegradationDirectionWithCompletionSelector :: Selector
readAttributeDegradationDirectionWithCompletionSelector = mkSelector "readAttributeDegradationDirectionWithCompletion:"

-- | @Selector@ for @subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDegradationDirectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDegradationDirectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDegradationDirectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDegradationDirectionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeChangeIndicationWithCompletion:@
readAttributeChangeIndicationWithCompletionSelector :: Selector
readAttributeChangeIndicationWithCompletionSelector = mkSelector "readAttributeChangeIndicationWithCompletion:"

-- | @Selector@ for @subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeChangeIndicationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChangeIndicationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeChangeIndicationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChangeIndicationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInPlaceIndicatorWithCompletion:@
readAttributeInPlaceIndicatorWithCompletionSelector :: Selector
readAttributeInPlaceIndicatorWithCompletionSelector = mkSelector "readAttributeInPlaceIndicatorWithCompletion:"

-- | @Selector@ for @subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInPlaceIndicatorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInPlaceIndicatorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:@
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInPlaceIndicatorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInPlaceIndicatorWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLastChangedTimeWithCompletion:@
readAttributeLastChangedTimeWithCompletionSelector :: Selector
readAttributeLastChangedTimeWithCompletionSelector = mkSelector "readAttributeLastChangedTimeWithCompletion:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:completion:@
writeAttributeLastChangedTimeWithValue_completionSelector :: Selector
writeAttributeLastChangedTimeWithValue_completionSelector = mkSelector "writeAttributeLastChangedTimeWithValue:completion:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:params:completion:@
writeAttributeLastChangedTimeWithValue_params_completionSelector :: Selector
writeAttributeLastChangedTimeWithValue_params_completionSelector = mkSelector "writeAttributeLastChangedTimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLastChangedTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLastChangedTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLastChangedTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLastChangedTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReplacementProductListWithCompletion:@
readAttributeReplacementProductListWithCompletionSelector :: Selector
readAttributeReplacementProductListWithCompletionSelector = mkSelector "readAttributeReplacementProductListWithCompletion:"

-- | @Selector@ for @subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReplacementProductListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReplacementProductListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:@
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeReplacementProductListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReplacementProductListWithClusterStateCache:endpoint:queue:completion:"

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

