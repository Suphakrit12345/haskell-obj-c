{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Barrier Control
--
-- This cluster provides control of a barrier (garage door).
--
-- Generated bindings for @MTRBaseClusterBarrierControl@.
module ObjC.Matter.MTRBaseClusterBarrierControl
  ( MTRBaseClusterBarrierControl
  , IsMTRBaseClusterBarrierControl(..)
  , barrierControlGoToPercentWithParams_completion
  , barrierControlStopWithParams_completion
  , barrierControlStopWithCompletion
  , readAttributeBarrierMovingStateWithCompletion
  , subscribeAttributeBarrierMovingStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierMovingStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierSafetyStatusWithCompletion
  , subscribeAttributeBarrierSafetyStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierSafetyStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierCapabilitiesWithCompletion
  , subscribeAttributeBarrierCapabilitiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierCapabilitiesWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierOpenEventsWithCompletion
  , writeAttributeBarrierOpenEventsWithValue_completion
  , writeAttributeBarrierOpenEventsWithValue_params_completion
  , subscribeAttributeBarrierOpenEventsWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierOpenEventsWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierCloseEventsWithCompletion
  , writeAttributeBarrierCloseEventsWithValue_completion
  , writeAttributeBarrierCloseEventsWithValue_params_completion
  , subscribeAttributeBarrierCloseEventsWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierCloseEventsWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierCommandOpenEventsWithCompletion
  , writeAttributeBarrierCommandOpenEventsWithValue_completion
  , writeAttributeBarrierCommandOpenEventsWithValue_params_completion
  , subscribeAttributeBarrierCommandOpenEventsWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierCommandOpenEventsWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierCommandCloseEventsWithCompletion
  , writeAttributeBarrierCommandCloseEventsWithValue_completion
  , writeAttributeBarrierCommandCloseEventsWithValue_params_completion
  , subscribeAttributeBarrierCommandCloseEventsWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierCommandCloseEventsWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierOpenPeriodWithCompletion
  , writeAttributeBarrierOpenPeriodWithValue_completion
  , writeAttributeBarrierOpenPeriodWithValue_params_completion
  , subscribeAttributeBarrierOpenPeriodWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierOpenPeriodWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierClosePeriodWithCompletion
  , writeAttributeBarrierClosePeriodWithValue_completion
  , writeAttributeBarrierClosePeriodWithValue_params_completion
  , subscribeAttributeBarrierClosePeriodWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierClosePeriodWithClusterStateCache_endpoint_queue_completion
  , readAttributeBarrierPositionWithCompletion
  , subscribeAttributeBarrierPositionWithParams_subscriptionEstablished_reportHandler
  , readAttributeBarrierPositionWithClusterStateCache_endpoint_queue_completion
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
  , barrierControlGoToPercentWithParams_completionHandler
  , barrierControlStopWithParams_completionHandler
  , barrierControlStopWithCompletionHandler
  , readAttributeBarrierMovingStateWithCompletionHandler
  , subscribeAttributeBarrierMovingStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierMovingStateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierSafetyStatusWithCompletionHandler
  , subscribeAttributeBarrierSafetyStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierSafetyStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierCapabilitiesWithCompletionHandler
  , subscribeAttributeBarrierCapabilitiesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierCapabilitiesWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierOpenEventsWithCompletionHandler
  , writeAttributeBarrierOpenEventsWithValue_completionHandler
  , writeAttributeBarrierOpenEventsWithValue_params_completionHandler
  , subscribeAttributeBarrierOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierOpenEventsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierCloseEventsWithCompletionHandler
  , writeAttributeBarrierCloseEventsWithValue_completionHandler
  , writeAttributeBarrierCloseEventsWithValue_params_completionHandler
  , subscribeAttributeBarrierCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierCloseEventsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierCommandOpenEventsWithCompletionHandler
  , writeAttributeBarrierCommandOpenEventsWithValue_completionHandler
  , writeAttributeBarrierCommandOpenEventsWithValue_params_completionHandler
  , subscribeAttributeBarrierCommandOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierCommandOpenEventsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierCommandCloseEventsWithCompletionHandler
  , writeAttributeBarrierCommandCloseEventsWithValue_completionHandler
  , writeAttributeBarrierCommandCloseEventsWithValue_params_completionHandler
  , subscribeAttributeBarrierCommandCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierCommandCloseEventsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierOpenPeriodWithCompletionHandler
  , writeAttributeBarrierOpenPeriodWithValue_completionHandler
  , writeAttributeBarrierOpenPeriodWithValue_params_completionHandler
  , subscribeAttributeBarrierOpenPeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierOpenPeriodWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierClosePeriodWithCompletionHandler
  , writeAttributeBarrierClosePeriodWithValue_completionHandler
  , writeAttributeBarrierClosePeriodWithValue_params_completionHandler
  , subscribeAttributeBarrierClosePeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierClosePeriodWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeBarrierPositionWithCompletionHandler
  , subscribeAttributeBarrierPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeBarrierPositionWithAttributeCache_endpoint_queue_completionHandler
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
  , barrierControlGoToPercentWithParams_completionSelector
  , barrierControlStopWithParams_completionSelector
  , barrierControlStopWithCompletionSelector
  , readAttributeBarrierMovingStateWithCompletionSelector
  , subscribeAttributeBarrierMovingStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierMovingStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierSafetyStatusWithCompletionSelector
  , subscribeAttributeBarrierSafetyStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierSafetyStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierCapabilitiesWithCompletionSelector
  , subscribeAttributeBarrierCapabilitiesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCapabilitiesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierOpenEventsWithCompletionSelector
  , writeAttributeBarrierOpenEventsWithValue_completionSelector
  , writeAttributeBarrierOpenEventsWithValue_params_completionSelector
  , subscribeAttributeBarrierOpenEventsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierOpenEventsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierCloseEventsWithCompletionSelector
  , writeAttributeBarrierCloseEventsWithValue_completionSelector
  , writeAttributeBarrierCloseEventsWithValue_params_completionSelector
  , subscribeAttributeBarrierCloseEventsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCloseEventsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierCommandOpenEventsWithCompletionSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_completionSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_params_completionSelector
  , subscribeAttributeBarrierCommandOpenEventsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCommandOpenEventsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierCommandCloseEventsWithCompletionSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_completionSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_params_completionSelector
  , subscribeAttributeBarrierCommandCloseEventsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCommandCloseEventsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierOpenPeriodWithCompletionSelector
  , writeAttributeBarrierOpenPeriodWithValue_completionSelector
  , writeAttributeBarrierOpenPeriodWithValue_params_completionSelector
  , subscribeAttributeBarrierOpenPeriodWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierOpenPeriodWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierClosePeriodWithCompletionSelector
  , writeAttributeBarrierClosePeriodWithValue_completionSelector
  , writeAttributeBarrierClosePeriodWithValue_params_completionSelector
  , subscribeAttributeBarrierClosePeriodWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierClosePeriodWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBarrierPositionWithCompletionSelector
  , subscribeAttributeBarrierPositionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierPositionWithClusterStateCache_endpoint_queue_completionSelector
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
  , barrierControlGoToPercentWithParams_completionHandlerSelector
  , barrierControlStopWithParams_completionHandlerSelector
  , barrierControlStopWithCompletionHandlerSelector
  , readAttributeBarrierMovingStateWithCompletionHandlerSelector
  , subscribeAttributeBarrierMovingStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierMovingStateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierSafetyStatusWithCompletionHandlerSelector
  , subscribeAttributeBarrierSafetyStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierSafetyStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierCapabilitiesWithCompletionHandlerSelector
  , subscribeAttributeBarrierCapabilitiesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCapabilitiesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierOpenEventsWithCompletionHandlerSelector
  , writeAttributeBarrierOpenEventsWithValue_completionHandlerSelector
  , writeAttributeBarrierOpenEventsWithValue_params_completionHandlerSelector
  , subscribeAttributeBarrierOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierOpenEventsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierCloseEventsWithCompletionHandlerSelector
  , writeAttributeBarrierCloseEventsWithValue_completionHandlerSelector
  , writeAttributeBarrierCloseEventsWithValue_params_completionHandlerSelector
  , subscribeAttributeBarrierCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCloseEventsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierCommandOpenEventsWithCompletionHandlerSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_completionHandlerSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_params_completionHandlerSelector
  , subscribeAttributeBarrierCommandOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCommandOpenEventsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierCommandCloseEventsWithCompletionHandlerSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_completionHandlerSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_params_completionHandlerSelector
  , subscribeAttributeBarrierCommandCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierCommandCloseEventsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierOpenPeriodWithCompletionHandlerSelector
  , writeAttributeBarrierOpenPeriodWithValue_completionHandlerSelector
  , writeAttributeBarrierOpenPeriodWithValue_params_completionHandlerSelector
  , subscribeAttributeBarrierOpenPeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierOpenPeriodWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierClosePeriodWithCompletionHandlerSelector
  , writeAttributeBarrierClosePeriodWithValue_completionHandlerSelector
  , writeAttributeBarrierClosePeriodWithValue_params_completionHandlerSelector
  , subscribeAttributeBarrierClosePeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierClosePeriodWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeBarrierPositionWithCompletionHandlerSelector
  , subscribeAttributeBarrierPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeBarrierPositionWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command BarrierControlGoToPercent
--
-- Command to instruct a barrier to go to a percent open state.
--
-- ObjC selector: @- barrierControlGoToPercentWithParams:completion:@
barrierControlGoToPercentWithParams_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlGoToPercentParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> IO ()
barrierControlGoToPercentWithParams_completion mtrBaseClusterBarrierControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "barrierControlGoToPercentWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command BarrierControlStop
--
-- Command that instructs the barrier to stop moving.
--
-- ObjC selector: @- barrierControlStopWithParams:completion:@
barrierControlStopWithParams_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlStopParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> IO ()
barrierControlStopWithParams_completion mtrBaseClusterBarrierControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "barrierControlStopWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- barrierControlStopWithCompletion:@
barrierControlStopWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
barrierControlStopWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "barrierControlStopWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierMovingStateWithCompletion:@
readAttributeBarrierMovingStateWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierMovingStateWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierMovingStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierMovingStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierMovingStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierMovingStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierMovingStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierMovingStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierMovingStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierMovingStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierMovingStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierSafetyStatusWithCompletion:@
readAttributeBarrierSafetyStatusWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierSafetyStatusWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierSafetyStatusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierSafetyStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierSafetyStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierSafetyStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierSafetyStatusWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierSafetyStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierSafetyStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierSafetyStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierSafetyStatusWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierCapabilitiesWithCompletion:@
readAttributeBarrierCapabilitiesWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCapabilitiesWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCapabilitiesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierCapabilitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCapabilitiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCapabilitiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCapabilitiesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCapabilitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCapabilitiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCapabilitiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCapabilitiesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierOpenEventsWithCompletion:@
readAttributeBarrierOpenEventsWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierOpenEventsWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierOpenEventsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierOpenEventsWithValue:completion:@
writeAttributeBarrierOpenEventsWithValue_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierOpenEventsWithValue_completion mtrBaseClusterBarrierControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenEventsWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierOpenEventsWithValue:params:completion:@
writeAttributeBarrierOpenEventsWithValue_params_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierOpenEventsWithValue_params_completion mtrBaseClusterBarrierControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenEventsWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierOpenEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenEventsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierOpenEventsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierOpenEventsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierOpenEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierOpenEventsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierOpenEventsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierOpenEventsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierCloseEventsWithCompletion:@
readAttributeBarrierCloseEventsWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCloseEventsWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCloseEventsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierCloseEventsWithValue:completion:@
writeAttributeBarrierCloseEventsWithValue_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierCloseEventsWithValue_completion mtrBaseClusterBarrierControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCloseEventsWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierCloseEventsWithValue:params:completion:@
writeAttributeBarrierCloseEventsWithValue_params_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierCloseEventsWithValue_params_completion mtrBaseClusterBarrierControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCloseEventsWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierCloseEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCloseEventsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCloseEventsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCloseEventsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCloseEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCloseEventsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCloseEventsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCloseEventsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierCommandOpenEventsWithCompletion:@
readAttributeBarrierCommandOpenEventsWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCommandOpenEventsWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCommandOpenEventsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:completion:@
writeAttributeBarrierCommandOpenEventsWithValue_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_completion mtrBaseClusterBarrierControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:params:completion:@
writeAttributeBarrierCommandOpenEventsWithValue_params_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_params_completion mtrBaseClusterBarrierControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierCommandOpenEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandOpenEventsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCommandOpenEventsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCommandOpenEventsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCommandOpenEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCommandOpenEventsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCommandOpenEventsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCommandOpenEventsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierCommandCloseEventsWithCompletion:@
readAttributeBarrierCommandCloseEventsWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCommandCloseEventsWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCommandCloseEventsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:completion:@
writeAttributeBarrierCommandCloseEventsWithValue_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_completion mtrBaseClusterBarrierControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:params:completion:@
writeAttributeBarrierCommandCloseEventsWithValue_params_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_params_completion mtrBaseClusterBarrierControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierCommandCloseEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandCloseEventsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCommandCloseEventsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCommandCloseEventsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCommandCloseEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCommandCloseEventsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCommandCloseEventsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCommandCloseEventsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierOpenPeriodWithCompletion:@
readAttributeBarrierOpenPeriodWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierOpenPeriodWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierOpenPeriodWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierOpenPeriodWithValue:completion:@
writeAttributeBarrierOpenPeriodWithValue_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierOpenPeriodWithValue_completion mtrBaseClusterBarrierControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenPeriodWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierOpenPeriodWithValue:params:completion:@
writeAttributeBarrierOpenPeriodWithValue_params_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierOpenPeriodWithValue_params_completion mtrBaseClusterBarrierControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenPeriodWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierOpenPeriodWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenPeriodWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierOpenPeriodWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierOpenPeriodWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierOpenPeriodWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierOpenPeriodWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierOpenPeriodWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierOpenPeriodWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierClosePeriodWithCompletion:@
readAttributeBarrierClosePeriodWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierClosePeriodWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierClosePeriodWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierClosePeriodWithValue:completion:@
writeAttributeBarrierClosePeriodWithValue_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierClosePeriodWithValue_completion mtrBaseClusterBarrierControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierClosePeriodWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeBarrierClosePeriodWithValue:params:completion:@
writeAttributeBarrierClosePeriodWithValue_params_completion :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierClosePeriodWithValue_params_completion mtrBaseClusterBarrierControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierClosePeriodWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierClosePeriodWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierClosePeriodWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierClosePeriodWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierClosePeriodWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierClosePeriodWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierClosePeriodWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierClosePeriodWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierClosePeriodWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBarrierPositionWithCompletion:@
readAttributeBarrierPositionWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierPositionWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierPositionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBarrierPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierPositionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierPositionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierPositionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierPositionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierPositionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierPositionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBarrierControl  completion =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> IO (Id MTRBaseClusterBarrierControl)
init_ mtrBaseClusterBarrierControl  =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterBarrierControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterBarrierControl -> device -> CUShort -> queue -> IO (Id MTRBaseClusterBarrierControl)
initWithDevice_endpoint_queue mtrBaseClusterBarrierControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- barrierControlGoToPercentWithParams:completionHandler:@
barrierControlGoToPercentWithParams_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlGoToPercentParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> IO ()
barrierControlGoToPercentWithParams_completionHandler mtrBaseClusterBarrierControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "barrierControlGoToPercentWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- barrierControlStopWithParams:completionHandler:@
barrierControlStopWithParams_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlStopParams params) => mtrBaseClusterBarrierControl -> params -> Ptr () -> IO ()
barrierControlStopWithParams_completionHandler mtrBaseClusterBarrierControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "barrierControlStopWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- barrierControlStopWithCompletionHandler:@
barrierControlStopWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
barrierControlStopWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "barrierControlStopWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierMovingStateWithCompletionHandler:@
readAttributeBarrierMovingStateWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierMovingStateWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierMovingStateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierMovingStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierMovingStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierMovingStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierMovingStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierMovingStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierMovingStateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierMovingStateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierMovingStateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierSafetyStatusWithCompletionHandler:@
readAttributeBarrierSafetyStatusWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierSafetyStatusWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierSafetyStatusWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierSafetyStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierSafetyStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierSafetyStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierSafetyStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierSafetyStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierSafetyStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierSafetyStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierSafetyStatusWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierCapabilitiesWithCompletionHandler:@
readAttributeBarrierCapabilitiesWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCapabilitiesWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCapabilitiesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierCapabilitiesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCapabilitiesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCapabilitiesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCapabilitiesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCapabilitiesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCapabilitiesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCapabilitiesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCapabilitiesWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierOpenEventsWithCompletionHandler:@
readAttributeBarrierOpenEventsWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierOpenEventsWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierOpenEventsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierOpenEventsWithValue:completionHandler:@
writeAttributeBarrierOpenEventsWithValue_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierOpenEventsWithValue_completionHandler mtrBaseClusterBarrierControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenEventsWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierOpenEventsWithValue:params:completionHandler:@
writeAttributeBarrierOpenEventsWithValue_params_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierOpenEventsWithValue_params_completionHandler mtrBaseClusterBarrierControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenEventsWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierOpenEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierOpenEventsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierOpenEventsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierOpenEventsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierCloseEventsWithCompletionHandler:@
readAttributeBarrierCloseEventsWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCloseEventsWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCloseEventsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierCloseEventsWithValue:completionHandler:@
writeAttributeBarrierCloseEventsWithValue_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierCloseEventsWithValue_completionHandler mtrBaseClusterBarrierControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCloseEventsWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierCloseEventsWithValue:params:completionHandler:@
writeAttributeBarrierCloseEventsWithValue_params_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierCloseEventsWithValue_params_completionHandler mtrBaseClusterBarrierControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCloseEventsWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCloseEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCloseEventsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCloseEventsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCloseEventsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierCommandOpenEventsWithCompletionHandler:@
readAttributeBarrierCommandOpenEventsWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCommandOpenEventsWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCommandOpenEventsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:completionHandler:@
writeAttributeBarrierCommandOpenEventsWithValue_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_completionHandler mtrBaseClusterBarrierControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:params:completionHandler:@
writeAttributeBarrierCommandOpenEventsWithValue_params_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_params_completionHandler mtrBaseClusterBarrierControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierCommandOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCommandOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCommandOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCommandOpenEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCommandOpenEventsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCommandOpenEventsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCommandOpenEventsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierCommandCloseEventsWithCompletionHandler:@
readAttributeBarrierCommandCloseEventsWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierCommandCloseEventsWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierCommandCloseEventsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:completionHandler:@
writeAttributeBarrierCommandCloseEventsWithValue_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_completionHandler mtrBaseClusterBarrierControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:params:completionHandler:@
writeAttributeBarrierCommandCloseEventsWithValue_params_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_params_completionHandler mtrBaseClusterBarrierControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierCommandCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierCommandCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierCommandCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierCommandCloseEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCommandCloseEventsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierCommandCloseEventsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierCommandCloseEventsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierOpenPeriodWithCompletionHandler:@
readAttributeBarrierOpenPeriodWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierOpenPeriodWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierOpenPeriodWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierOpenPeriodWithValue:completionHandler:@
writeAttributeBarrierOpenPeriodWithValue_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierOpenPeriodWithValue_completionHandler mtrBaseClusterBarrierControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenPeriodWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierOpenPeriodWithValue:params:completionHandler:@
writeAttributeBarrierOpenPeriodWithValue_params_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierOpenPeriodWithValue_params_completionHandler mtrBaseClusterBarrierControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierOpenPeriodWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierOpenPeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenPeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierOpenPeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierOpenPeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierOpenPeriodWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierOpenPeriodWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierOpenPeriodWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierOpenPeriodWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierClosePeriodWithCompletionHandler:@
readAttributeBarrierClosePeriodWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierClosePeriodWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierClosePeriodWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierClosePeriodWithValue:completionHandler:@
writeAttributeBarrierClosePeriodWithValue_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value) => mtrBaseClusterBarrierControl -> value -> Ptr () -> IO ()
writeAttributeBarrierClosePeriodWithValue_completionHandler mtrBaseClusterBarrierControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierClosePeriodWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeBarrierClosePeriodWithValue:params:completionHandler:@
writeAttributeBarrierClosePeriodWithValue_params_completionHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBarrierControl -> value -> params -> Ptr () -> IO ()
writeAttributeBarrierClosePeriodWithValue_params_completionHandler mtrBaseClusterBarrierControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBarrierControl (mkSelector "writeAttributeBarrierClosePeriodWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierClosePeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierClosePeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierClosePeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierClosePeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierClosePeriodWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierClosePeriodWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierClosePeriodWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierClosePeriodWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBarrierPositionWithCompletionHandler:@
readAttributeBarrierPositionWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeBarrierPositionWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeBarrierPositionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeBarrierPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBarrierPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeBarrierPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBarrierPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierPositionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBarrierPositionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBarrierPositionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl => mtrBaseClusterBarrierControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterBarrierControl  completionHandler =
    sendMsg mtrBaseClusterBarrierControl (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBarrierControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBarrierControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBarrierControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBarrierControl mtrBaseClusterBarrierControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBarrierControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBarrierControl)
initWithDevice_endpointID_queue mtrBaseClusterBarrierControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterBarrierControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @barrierControlGoToPercentWithParams:completion:@
barrierControlGoToPercentWithParams_completionSelector :: Selector
barrierControlGoToPercentWithParams_completionSelector = mkSelector "barrierControlGoToPercentWithParams:completion:"

-- | @Selector@ for @barrierControlStopWithParams:completion:@
barrierControlStopWithParams_completionSelector :: Selector
barrierControlStopWithParams_completionSelector = mkSelector "barrierControlStopWithParams:completion:"

-- | @Selector@ for @barrierControlStopWithCompletion:@
barrierControlStopWithCompletionSelector :: Selector
barrierControlStopWithCompletionSelector = mkSelector "barrierControlStopWithCompletion:"

-- | @Selector@ for @readAttributeBarrierMovingStateWithCompletion:@
readAttributeBarrierMovingStateWithCompletionSelector :: Selector
readAttributeBarrierMovingStateWithCompletionSelector = mkSelector "readAttributeBarrierMovingStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeBarrierMovingStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierMovingStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierMovingStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierMovingStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierMovingStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierMovingStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierMovingStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierMovingStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierSafetyStatusWithCompletion:@
readAttributeBarrierSafetyStatusWithCompletionSelector :: Selector
readAttributeBarrierSafetyStatusWithCompletionSelector = mkSelector "readAttributeBarrierSafetyStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeBarrierSafetyStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierSafetyStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierSafetyStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierSafetyStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierSafetyStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierSafetyStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierSafetyStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierSafetyStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierCapabilitiesWithCompletion:@
readAttributeBarrierCapabilitiesWithCompletionSelector :: Selector
readAttributeBarrierCapabilitiesWithCompletionSelector = mkSelector "readAttributeBarrierCapabilitiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeBarrierCapabilitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCapabilitiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCapabilitiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCapabilitiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCapabilitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCapabilitiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierCapabilitiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierCapabilitiesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierOpenEventsWithCompletion:@
readAttributeBarrierOpenEventsWithCompletionSelector :: Selector
readAttributeBarrierOpenEventsWithCompletionSelector = mkSelector "readAttributeBarrierOpenEventsWithCompletion:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:completion:@
writeAttributeBarrierOpenEventsWithValue_completionSelector :: Selector
writeAttributeBarrierOpenEventsWithValue_completionSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:completion:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:params:completion:@
writeAttributeBarrierOpenEventsWithValue_params_completionSelector :: Selector
writeAttributeBarrierOpenEventsWithValue_params_completionSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBarrierOpenEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenEventsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierOpenEventsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierOpenEventsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierOpenEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierOpenEventsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierOpenEventsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierOpenEventsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierCloseEventsWithCompletion:@
readAttributeBarrierCloseEventsWithCompletionSelector :: Selector
readAttributeBarrierCloseEventsWithCompletionSelector = mkSelector "readAttributeBarrierCloseEventsWithCompletion:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:completion:@
writeAttributeBarrierCloseEventsWithValue_completionSelector :: Selector
writeAttributeBarrierCloseEventsWithValue_completionSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:completion:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:params:completion:@
writeAttributeBarrierCloseEventsWithValue_params_completionSelector :: Selector
writeAttributeBarrierCloseEventsWithValue_params_completionSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBarrierCloseEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCloseEventsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCloseEventsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCloseEventsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCloseEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCloseEventsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierCloseEventsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierCloseEventsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierCommandOpenEventsWithCompletion:@
readAttributeBarrierCommandOpenEventsWithCompletionSelector :: Selector
readAttributeBarrierCommandOpenEventsWithCompletionSelector = mkSelector "readAttributeBarrierCommandOpenEventsWithCompletion:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:completion:@
writeAttributeBarrierCommandOpenEventsWithValue_completionSelector :: Selector
writeAttributeBarrierCommandOpenEventsWithValue_completionSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:completion:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:params:completion:@
writeAttributeBarrierCommandOpenEventsWithValue_params_completionSelector :: Selector
writeAttributeBarrierCommandOpenEventsWithValue_params_completionSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBarrierCommandOpenEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandOpenEventsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCommandOpenEventsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCommandOpenEventsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCommandOpenEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCommandOpenEventsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierCommandOpenEventsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierCommandOpenEventsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierCommandCloseEventsWithCompletion:@
readAttributeBarrierCommandCloseEventsWithCompletionSelector :: Selector
readAttributeBarrierCommandCloseEventsWithCompletionSelector = mkSelector "readAttributeBarrierCommandCloseEventsWithCompletion:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:completion:@
writeAttributeBarrierCommandCloseEventsWithValue_completionSelector :: Selector
writeAttributeBarrierCommandCloseEventsWithValue_completionSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:completion:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:params:completion:@
writeAttributeBarrierCommandCloseEventsWithValue_params_completionSelector :: Selector
writeAttributeBarrierCommandCloseEventsWithValue_params_completionSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBarrierCommandCloseEventsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandCloseEventsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCommandCloseEventsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCommandCloseEventsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCommandCloseEventsWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierCommandCloseEventsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierCommandCloseEventsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierCommandCloseEventsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierOpenPeriodWithCompletion:@
readAttributeBarrierOpenPeriodWithCompletionSelector :: Selector
readAttributeBarrierOpenPeriodWithCompletionSelector = mkSelector "readAttributeBarrierOpenPeriodWithCompletion:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:completion:@
writeAttributeBarrierOpenPeriodWithValue_completionSelector :: Selector
writeAttributeBarrierOpenPeriodWithValue_completionSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:completion:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:params:completion:@
writeAttributeBarrierOpenPeriodWithValue_params_completionSelector :: Selector
writeAttributeBarrierOpenPeriodWithValue_params_completionSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBarrierOpenPeriodWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenPeriodWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierOpenPeriodWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierOpenPeriodWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierOpenPeriodWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierOpenPeriodWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierOpenPeriodWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierOpenPeriodWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierClosePeriodWithCompletion:@
readAttributeBarrierClosePeriodWithCompletionSelector :: Selector
readAttributeBarrierClosePeriodWithCompletionSelector = mkSelector "readAttributeBarrierClosePeriodWithCompletion:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:completion:@
writeAttributeBarrierClosePeriodWithValue_completionSelector :: Selector
writeAttributeBarrierClosePeriodWithValue_completionSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:completion:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:params:completion:@
writeAttributeBarrierClosePeriodWithValue_params_completionSelector :: Selector
writeAttributeBarrierClosePeriodWithValue_params_completionSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeBarrierClosePeriodWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierClosePeriodWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierClosePeriodWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierClosePeriodWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierClosePeriodWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierClosePeriodWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierClosePeriodWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierClosePeriodWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBarrierPositionWithCompletion:@
readAttributeBarrierPositionWithCompletionSelector :: Selector
readAttributeBarrierPositionWithCompletionSelector = mkSelector "readAttributeBarrierPositionWithCompletion:"

-- | @Selector@ for @subscribeAttributeBarrierPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierPositionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierPositionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierPositionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeBarrierPositionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBarrierPositionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBarrierPositionWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @barrierControlGoToPercentWithParams:completionHandler:@
barrierControlGoToPercentWithParams_completionHandlerSelector :: Selector
barrierControlGoToPercentWithParams_completionHandlerSelector = mkSelector "barrierControlGoToPercentWithParams:completionHandler:"

-- | @Selector@ for @barrierControlStopWithParams:completionHandler:@
barrierControlStopWithParams_completionHandlerSelector :: Selector
barrierControlStopWithParams_completionHandlerSelector = mkSelector "barrierControlStopWithParams:completionHandler:"

-- | @Selector@ for @barrierControlStopWithCompletionHandler:@
barrierControlStopWithCompletionHandlerSelector :: Selector
barrierControlStopWithCompletionHandlerSelector = mkSelector "barrierControlStopWithCompletionHandler:"

-- | @Selector@ for @readAttributeBarrierMovingStateWithCompletionHandler:@
readAttributeBarrierMovingStateWithCompletionHandlerSelector :: Selector
readAttributeBarrierMovingStateWithCompletionHandlerSelector = mkSelector "readAttributeBarrierMovingStateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierMovingStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierMovingStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierMovingStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierMovingStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierMovingStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierMovingStateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierMovingStateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierMovingStateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierSafetyStatusWithCompletionHandler:@
readAttributeBarrierSafetyStatusWithCompletionHandlerSelector :: Selector
readAttributeBarrierSafetyStatusWithCompletionHandlerSelector = mkSelector "readAttributeBarrierSafetyStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierSafetyStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierSafetyStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierSafetyStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierSafetyStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierSafetyStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierSafetyStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierSafetyStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierSafetyStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierCapabilitiesWithCompletionHandler:@
readAttributeBarrierCapabilitiesWithCompletionHandlerSelector :: Selector
readAttributeBarrierCapabilitiesWithCompletionHandlerSelector = mkSelector "readAttributeBarrierCapabilitiesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierCapabilitiesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCapabilitiesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCapabilitiesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCapabilitiesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCapabilitiesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCapabilitiesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierCapabilitiesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierCapabilitiesWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierOpenEventsWithCompletionHandler:@
readAttributeBarrierOpenEventsWithCompletionHandlerSelector :: Selector
readAttributeBarrierOpenEventsWithCompletionHandlerSelector = mkSelector "readAttributeBarrierOpenEventsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:completionHandler:@
writeAttributeBarrierOpenEventsWithValue_completionHandlerSelector :: Selector
writeAttributeBarrierOpenEventsWithValue_completionHandlerSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:params:completionHandler:@
writeAttributeBarrierOpenEventsWithValue_params_completionHandlerSelector :: Selector
writeAttributeBarrierOpenEventsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierOpenEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierOpenEventsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierOpenEventsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierOpenEventsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierCloseEventsWithCompletionHandler:@
readAttributeBarrierCloseEventsWithCompletionHandlerSelector :: Selector
readAttributeBarrierCloseEventsWithCompletionHandlerSelector = mkSelector "readAttributeBarrierCloseEventsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:completionHandler:@
writeAttributeBarrierCloseEventsWithValue_completionHandlerSelector :: Selector
writeAttributeBarrierCloseEventsWithValue_completionHandlerSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:params:completionHandler:@
writeAttributeBarrierCloseEventsWithValue_params_completionHandlerSelector :: Selector
writeAttributeBarrierCloseEventsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCloseEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCloseEventsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierCloseEventsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierCloseEventsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierCommandOpenEventsWithCompletionHandler:@
readAttributeBarrierCommandOpenEventsWithCompletionHandlerSelector :: Selector
readAttributeBarrierCommandOpenEventsWithCompletionHandlerSelector = mkSelector "readAttributeBarrierCommandOpenEventsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:completionHandler:@
writeAttributeBarrierCommandOpenEventsWithValue_completionHandlerSelector :: Selector
writeAttributeBarrierCommandOpenEventsWithValue_completionHandlerSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:params:completionHandler:@
writeAttributeBarrierCommandOpenEventsWithValue_params_completionHandlerSelector :: Selector
writeAttributeBarrierCommandOpenEventsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierCommandOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCommandOpenEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCommandOpenEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCommandOpenEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCommandOpenEventsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierCommandOpenEventsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierCommandOpenEventsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierCommandCloseEventsWithCompletionHandler:@
readAttributeBarrierCommandCloseEventsWithCompletionHandlerSelector :: Selector
readAttributeBarrierCommandCloseEventsWithCompletionHandlerSelector = mkSelector "readAttributeBarrierCommandCloseEventsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:completionHandler:@
writeAttributeBarrierCommandCloseEventsWithValue_completionHandlerSelector :: Selector
writeAttributeBarrierCommandCloseEventsWithValue_completionHandlerSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:params:completionHandler:@
writeAttributeBarrierCommandCloseEventsWithValue_params_completionHandlerSelector :: Selector
writeAttributeBarrierCommandCloseEventsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierCommandCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierCommandCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierCommandCloseEventsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierCommandCloseEventsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierCommandCloseEventsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierCommandCloseEventsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierCommandCloseEventsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierCommandCloseEventsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierOpenPeriodWithCompletionHandler:@
readAttributeBarrierOpenPeriodWithCompletionHandlerSelector :: Selector
readAttributeBarrierOpenPeriodWithCompletionHandlerSelector = mkSelector "readAttributeBarrierOpenPeriodWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:completionHandler:@
writeAttributeBarrierOpenPeriodWithValue_completionHandlerSelector :: Selector
writeAttributeBarrierOpenPeriodWithValue_completionHandlerSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:params:completionHandler:@
writeAttributeBarrierOpenPeriodWithValue_params_completionHandlerSelector :: Selector
writeAttributeBarrierOpenPeriodWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierOpenPeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierOpenPeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierOpenPeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierOpenPeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierOpenPeriodWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierOpenPeriodWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierOpenPeriodWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierOpenPeriodWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierClosePeriodWithCompletionHandler:@
readAttributeBarrierClosePeriodWithCompletionHandlerSelector :: Selector
readAttributeBarrierClosePeriodWithCompletionHandlerSelector = mkSelector "readAttributeBarrierClosePeriodWithCompletionHandler:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:completionHandler:@
writeAttributeBarrierClosePeriodWithValue_completionHandlerSelector :: Selector
writeAttributeBarrierClosePeriodWithValue_completionHandlerSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:params:completionHandler:@
writeAttributeBarrierClosePeriodWithValue_params_completionHandlerSelector :: Selector
writeAttributeBarrierClosePeriodWithValue_params_completionHandlerSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierClosePeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierClosePeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierClosePeriodWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierClosePeriodWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierClosePeriodWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierClosePeriodWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierClosePeriodWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierClosePeriodWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeBarrierPositionWithCompletionHandler:@
readAttributeBarrierPositionWithCompletionHandlerSelector :: Selector
readAttributeBarrierPositionWithCompletionHandlerSelector = mkSelector "readAttributeBarrierPositionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeBarrierPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeBarrierPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBarrierPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBarrierPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBarrierPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeBarrierPositionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeBarrierPositionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeBarrierPositionWithAttributeCache:endpoint:queue:completionHandler:"

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

