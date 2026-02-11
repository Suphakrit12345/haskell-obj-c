{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Illuminance Measurement
--
-- Attributes and commands for configuring the measurement of illuminance, and reporting illuminance measurements.
--
-- Generated bindings for @MTRBaseClusterIlluminanceMeasurement@.
module ObjC.Matter.MTRBaseClusterIlluminanceMeasurement
  ( MTRBaseClusterIlluminanceMeasurement
  , IsMTRBaseClusterIlluminanceMeasurement(..)
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
  , readAttributeLightSensorTypeWithCompletion
  , subscribeAttributeLightSensorTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLightSensorTypeWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeLightSensorTypeWithCompletionHandler
  , subscribeAttributeLightSensorTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLightSensorTypeWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeLightSensorTypeWithCompletionSelector
  , subscribeAttributeLightSensorTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLightSensorTypeWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeLightSensorTypeWithCompletionHandlerSelector
  , subscribeAttributeLightSensorTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLightSensorTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
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
readAttributeMeasuredValueWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeMeasuredValueWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinMeasuredValueWithCompletion:@
readAttributeMinMeasuredValueWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeMinMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxMeasuredValueWithCompletion:@
readAttributeMaxMeasuredValueWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeMaxMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeToleranceWithCompletion:@
readAttributeToleranceWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeToleranceWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeToleranceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeToleranceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeToleranceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeToleranceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeToleranceWithClusterStateCache:endpoint:queue:completion:@
readAttributeToleranceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeToleranceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeToleranceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLightSensorTypeWithCompletion:@
readAttributeLightSensorTypeWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeLightSensorTypeWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeLightSensorTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLightSensorTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLightSensorTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLightSensorTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeLightSensorTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLightSensorTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLightSensorTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLightSensorTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLightSensorTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterIlluminanceMeasurement  completion =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> IO (Id MTRBaseClusterIlluminanceMeasurement)
init_ mtrBaseClusterIlluminanceMeasurement  =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterIlluminanceMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterIlluminanceMeasurement -> device -> CUShort -> queue -> IO (Id MTRBaseClusterIlluminanceMeasurement)
initWithDevice_endpoint_queue mtrBaseClusterIlluminanceMeasurement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeMeasuredValueWithCompletionHandler:@
readAttributeMeasuredValueWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeMeasuredValueWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeMeasuredValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasuredValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMinMeasuredValueWithCompletionHandler:@
readAttributeMinMeasuredValueWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeMinMeasuredValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMinMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeMinMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxMeasuredValueWithCompletionHandler:@
readAttributeMaxMeasuredValueWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeMaxMeasuredValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMeasuredValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeMaxMeasuredValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxMeasuredValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeToleranceWithCompletionHandler:@
readAttributeToleranceWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeToleranceWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeToleranceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeToleranceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeToleranceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeToleranceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeToleranceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeToleranceWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLightSensorTypeWithCompletionHandler:@
readAttributeLightSensorTypeWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeLightSensorTypeWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeLightSensorTypeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLightSensorTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLightSensorTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLightSensorTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeLightSensorTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLightSensorTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLightSensorTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLightSensorTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLightSensorTypeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement => mtrBaseClusterIlluminanceMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterIlluminanceMeasurement  completionHandler =
    sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterIlluminanceMeasurement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterIlluminanceMeasurement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterIlluminanceMeasurement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterIlluminanceMeasurement mtrBaseClusterIlluminanceMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterIlluminanceMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterIlluminanceMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterIlluminanceMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterIlluminanceMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

-- | @Selector@ for @readAttributeLightSensorTypeWithCompletion:@
readAttributeLightSensorTypeWithCompletionSelector :: Selector
readAttributeLightSensorTypeWithCompletionSelector = mkSelector "readAttributeLightSensorTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeLightSensorTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLightSensorTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLightSensorTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLightSensorTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLightSensorTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLightSensorTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLightSensorTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLightSensorTypeWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeLightSensorTypeWithCompletionHandler:@
readAttributeLightSensorTypeWithCompletionHandlerSelector :: Selector
readAttributeLightSensorTypeWithCompletionHandlerSelector = mkSelector "readAttributeLightSensorTypeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLightSensorTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLightSensorTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLightSensorTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLightSensorTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLightSensorTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLightSensorTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLightSensorTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLightSensorTypeWithAttributeCache:endpoint:queue:completionHandler:"

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

