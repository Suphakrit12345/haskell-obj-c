{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pressure Measurement
--
-- Attributes and commands for configuring the measurement of pressure, and reporting pressure measurements.
--
-- Generated bindings for @MTRBaseClusterPressureMeasurement@.
module ObjC.Matter.MTRBaseClusterPressureMeasurement
  ( MTRBaseClusterPressureMeasurement
  , IsMTRBaseClusterPressureMeasurement(..)
  , readAttributeMeasuredValueWithCompletion
  , subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinMeasuredValueWithCompletion
  , subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxMeasuredValueWithCompletion
  , subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeToleranceWithCompletion
  , subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandler
  , readAttributeToleranceWithClusterStateCache_endpoint_queue_completion
  , readAttributeScaledValueWithCompletion
  , subscribeAttributeScaledValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeScaledValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinScaledValueWithCompletion
  , subscribeAttributeMinScaledValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinScaledValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxScaledValueWithCompletion
  , subscribeAttributeMaxScaledValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxScaledValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeScaledToleranceWithCompletion
  , subscribeAttributeScaledToleranceWithParams_subscriptionEstablished_reportHandler
  , readAttributeScaledToleranceWithClusterStateCache_endpoint_queue_completion
  , readAttributeScaleWithCompletion
  , subscribeAttributeScaleWithParams_subscriptionEstablished_reportHandler
  , readAttributeScaleWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMeasuredValueWithCompletionHandler
  , subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMinMeasuredValueWithCompletionHandler
  , subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxMeasuredValueWithCompletionHandler
  , subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeToleranceWithCompletionHandler
  , subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeScaledValueWithCompletionHandler
  , subscribeAttributeScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeScaledValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMinScaledValueWithCompletionHandler
  , subscribeAttributeMinScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMinScaledValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxScaledValueWithCompletionHandler
  , subscribeAttributeMaxScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxScaledValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeScaledToleranceWithCompletionHandler
  , subscribeAttributeScaledToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeScaledToleranceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeScaleWithCompletionHandler
  , subscribeAttributeScaleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeScaleWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeMeasuredValueWithCompletionSelector
  , subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinMeasuredValueWithCompletionSelector
  , subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxMeasuredValueWithCompletionSelector
  , subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeToleranceWithCompletionSelector
  , subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeToleranceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScaledValueWithCompletionSelector
  , subscribeAttributeScaledValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScaledValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinScaledValueWithCompletionSelector
  , subscribeAttributeMinScaledValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinScaledValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxScaledValueWithCompletionSelector
  , subscribeAttributeMaxScaledValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxScaledValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScaledToleranceWithCompletionSelector
  , subscribeAttributeScaledToleranceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScaledToleranceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScaleWithCompletionSelector
  , subscribeAttributeScaleWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScaleWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeMeasuredValueWithCompletionHandlerSelector
  , subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMinMeasuredValueWithCompletionHandlerSelector
  , subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxMeasuredValueWithCompletionHandlerSelector
  , subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeToleranceWithCompletionHandlerSelector
  , subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeScaledValueWithCompletionHandlerSelector
  , subscribeAttributeScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMinScaledValueWithCompletionHandlerSelector
  , subscribeAttributeMinScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxScaledValueWithCompletionHandlerSelector
  , subscribeAttributeMaxScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeScaledToleranceWithCompletionHandlerSelector
  , subscribeAttributeScaledToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeScaledToleranceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeScaleWithCompletionHandlerSelector
  , subscribeAttributeScaleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeScaleWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributeMeasuredValueWithCompletion:@
readAttributeMeasuredValueWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMeasuredValueWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinMeasuredValueWithCompletion:@
readAttributeMinMeasuredValueWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMinMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxMeasuredValueWithCompletion:@
readAttributeMaxMeasuredValueWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMaxMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeToleranceWithCompletion:@
readAttributeToleranceWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeToleranceWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeToleranceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeToleranceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeToleranceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeToleranceWithClusterStateCache:endpoint:queue:completion:@
readAttributeToleranceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeToleranceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeToleranceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScaledValueWithCompletion:@
readAttributeScaledValueWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeScaledValueWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeScaledValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScaledValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScaledValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeScaledValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScaledValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeScaledValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScaledValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScaledValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinScaledValueWithCompletion:@
readAttributeMinScaledValueWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMinScaledValueWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMinScaledValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinScaledValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinScaledValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinScaledValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMinScaledValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinScaledValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinScaledValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinScaledValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinScaledValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxScaledValueWithCompletion:@
readAttributeMaxScaledValueWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMaxScaledValueWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMaxScaledValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxScaledValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxScaledValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxScaledValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMaxScaledValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxScaledValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxScaledValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxScaledValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxScaledValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScaledToleranceWithCompletion:@
readAttributeScaledToleranceWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeScaledToleranceWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeScaledToleranceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScaledToleranceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledToleranceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScaledToleranceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeScaledToleranceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScaledToleranceWithClusterStateCache:endpoint:queue:completion:@
readAttributeScaledToleranceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScaledToleranceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScaledToleranceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScaleWithCompletion:@
readAttributeScaleWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeScaleWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeScaleWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScaleWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScaleWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScaleWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeScaleWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScaleWithClusterStateCache:endpoint:queue:completion:@
readAttributeScaleWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScaleWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScaleWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterPressureMeasurement  completion =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> IO (Id MTRBaseClusterPressureMeasurement)
init_ mtrBaseClusterPressureMeasurement  =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterPressureMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterPressureMeasurement -> device -> CUShort -> queue -> IO (Id MTRBaseClusterPressureMeasurement)
initWithDevice_endpoint_queue mtrBaseClusterPressureMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterPressureMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeMeasuredValueWithCompletionHandler:@
readAttributeMeasuredValueWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMeasuredValueWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMeasuredValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMinMeasuredValueWithCompletionHandler:@
readAttributeMinMeasuredValueWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMinMeasuredValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMinMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMinMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxMeasuredValueWithCompletionHandler:@
readAttributeMaxMeasuredValueWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMaxMeasuredValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMaxMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeToleranceWithCompletionHandler:@
readAttributeToleranceWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeToleranceWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeToleranceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeToleranceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeToleranceWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeScaledValueWithCompletionHandler:@
readAttributeScaledValueWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeScaledValueWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeScaledValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScaledValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScaledValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScaledValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScaledValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMinScaledValueWithCompletionHandler:@
readAttributeMinScaledValueWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMinScaledValueWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMinScaledValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMinScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMinScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinScaledValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinScaledValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinScaledValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinScaledValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxScaledValueWithCompletionHandler:@
readAttributeMaxScaledValueWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeMaxScaledValueWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeMaxScaledValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeMaxScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxScaledValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxScaledValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxScaledValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxScaledValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeScaledToleranceWithCompletionHandler:@
readAttributeScaledToleranceWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeScaledToleranceWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeScaledToleranceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeScaledToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScaledToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeScaledToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScaledToleranceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScaledToleranceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScaledToleranceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScaledToleranceWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeScaleWithCompletionHandler:@
readAttributeScaleWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeScaleWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeScaleWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeScaleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScaleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScaleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeScaleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScaleWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScaleWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScaleWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScaleWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement => mtrBaseClusterPressureMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterPressureMeasurement  completionHandler =
    sendMsg mtrBaseClusterPressureMeasurement (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterPressureMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterPressureMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterPressureMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterPressureMeasurement mtrBaseClusterPressureMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterPressureMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterPressureMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterPressureMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterPressureMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeasuredValueWithCompletion:@
readAttributeMeasuredValueWithCompletionSelector :: Selector
readAttributeMeasuredValueWithCompletionSelector = mkSelector "readAttributeMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithCompletion:@
readAttributeMinMeasuredValueWithCompletionSelector :: Selector
readAttributeMinMeasuredValueWithCompletionSelector = mkSelector "readAttributeMinMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithCompletion:@
readAttributeMaxMeasuredValueWithCompletionSelector :: Selector
readAttributeMaxMeasuredValueWithCompletionSelector = mkSelector "readAttributeMaxMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeToleranceWithCompletion:@
readAttributeToleranceWithCompletionSelector :: Selector
readAttributeToleranceWithCompletionSelector = mkSelector "readAttributeToleranceWithCompletion:"

-- | @Selector@ for @subscribeAttributeToleranceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeToleranceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeToleranceWithClusterStateCache:endpoint:queue:completion:@
readAttributeToleranceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeToleranceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeToleranceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScaledValueWithCompletion:@
readAttributeScaledValueWithCompletionSelector :: Selector
readAttributeScaledValueWithCompletionSelector = mkSelector "readAttributeScaledValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeScaledValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScaledValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScaledValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScaledValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeScaledValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScaledValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScaledValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinScaledValueWithCompletion:@
readAttributeMinScaledValueWithCompletionSelector :: Selector
readAttributeMinScaledValueWithCompletionSelector = mkSelector "readAttributeMinScaledValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinScaledValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinScaledValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinScaledValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinScaledValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinScaledValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinScaledValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinScaledValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinScaledValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxScaledValueWithCompletion:@
readAttributeMaxScaledValueWithCompletionSelector :: Selector
readAttributeMaxScaledValueWithCompletionSelector = mkSelector "readAttributeMaxScaledValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxScaledValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxScaledValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxScaledValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxScaledValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxScaledValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxScaledValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxScaledValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxScaledValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScaledToleranceWithCompletion:@
readAttributeScaledToleranceWithCompletionSelector :: Selector
readAttributeScaledToleranceWithCompletionSelector = mkSelector "readAttributeScaledToleranceWithCompletion:"

-- | @Selector@ for @subscribeAttributeScaledToleranceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledToleranceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScaledToleranceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScaledToleranceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScaledToleranceWithClusterStateCache:endpoint:queue:completion:@
readAttributeScaledToleranceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScaledToleranceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScaledToleranceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScaleWithCompletion:@
readAttributeScaleWithCompletionSelector :: Selector
readAttributeScaleWithCompletionSelector = mkSelector "readAttributeScaleWithCompletion:"

-- | @Selector@ for @subscribeAttributeScaleWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScaleWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScaleWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScaleWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScaleWithClusterStateCache:endpoint:queue:completion:@
readAttributeScaleWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScaleWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScaleWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeMeasuredValueWithCompletionHandler:@
readAttributeMeasuredValueWithCompletionHandlerSelector :: Selector
readAttributeMeasuredValueWithCompletionHandlerSelector = mkSelector "readAttributeMeasuredValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithCompletionHandler:@
readAttributeMinMeasuredValueWithCompletionHandlerSelector :: Selector
readAttributeMinMeasuredValueWithCompletionHandlerSelector = mkSelector "readAttributeMinMeasuredValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMinMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMinMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithCompletionHandler:@
readAttributeMaxMeasuredValueWithCompletionHandlerSelector :: Selector
readAttributeMaxMeasuredValueWithCompletionHandlerSelector = mkSelector "readAttributeMaxMeasuredValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeToleranceWithCompletionHandler:@
readAttributeToleranceWithCompletionHandlerSelector :: Selector
readAttributeToleranceWithCompletionHandlerSelector = mkSelector "readAttributeToleranceWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeToleranceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeToleranceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeScaledValueWithCompletionHandler:@
readAttributeScaledValueWithCompletionHandlerSelector :: Selector
readAttributeScaledValueWithCompletionHandlerSelector = mkSelector "readAttributeScaledValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScaledValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeScaledValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMinScaledValueWithCompletionHandler:@
readAttributeMinScaledValueWithCompletionHandlerSelector :: Selector
readAttributeMinScaledValueWithCompletionHandlerSelector = mkSelector "readAttributeMinScaledValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMinScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinScaledValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMinScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMinScaledValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxScaledValueWithCompletionHandler:@
readAttributeMaxScaledValueWithCompletionHandlerSelector :: Selector
readAttributeMaxScaledValueWithCompletionHandlerSelector = mkSelector "readAttributeMaxScaledValueWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxScaledValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxScaledValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxScaledValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxScaledValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxScaledValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeScaledToleranceWithCompletionHandler:@
readAttributeScaledToleranceWithCompletionHandlerSelector :: Selector
readAttributeScaledToleranceWithCompletionHandlerSelector = mkSelector "readAttributeScaledToleranceWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeScaledToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScaledToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScaledToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScaledToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScaledToleranceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScaledToleranceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeScaledToleranceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeScaledToleranceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeScaleWithCompletionHandler:@
readAttributeScaleWithCompletionHandlerSelector :: Selector
readAttributeScaleWithCompletionHandlerSelector = mkSelector "readAttributeScaleWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeScaleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScaleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScaleWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScaleWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScaleWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScaleWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeScaleWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeScaleWithAttributeCache:endpoint:queue:completionHandler:"

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

