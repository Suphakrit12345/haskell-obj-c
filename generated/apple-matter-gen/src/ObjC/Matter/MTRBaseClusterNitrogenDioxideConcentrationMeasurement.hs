{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Nitrogen Dioxide Concentration Measurement
--
-- Attributes for reporting nitrogen dioxide concentration measurements
--
-- Generated bindings for @MTRBaseClusterNitrogenDioxideConcentrationMeasurement@.
module ObjC.Matter.MTRBaseClusterNitrogenDioxideConcentrationMeasurement
  ( MTRBaseClusterNitrogenDioxideConcentrationMeasurement
  , IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement(..)
  , readAttributeMeasuredValueWithCompletion
  , subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinMeasuredValueWithCompletion
  , subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxMeasuredValueWithCompletion
  , subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributePeakMeasuredValueWithCompletion
  , subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributePeakMeasuredValueWindowWithCompletion
  , subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler
  , readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion
  , readAttributeAverageMeasuredValueWithCompletion
  , subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeAverageMeasuredValueWindowWithCompletion
  , subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler
  , readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion
  , readAttributeUncertaintyWithCompletion
  , subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandler
  , readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeasurementUnitWithCompletion
  , subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeasurementMediumWithCompletion
  , subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completion
  , readAttributeLevelValueWithCompletion
  , subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeLevelValueWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMeasuredValueWithCompletionSelector
  , subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinMeasuredValueWithCompletionSelector
  , subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxMeasuredValueWithCompletionSelector
  , subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeakMeasuredValueWithCompletionSelector
  , subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeakMeasuredValueWindowWithCompletionSelector
  , subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAverageMeasuredValueWithCompletionSelector
  , subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAverageMeasuredValueWindowWithCompletionSelector
  , subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUncertaintyWithCompletionSelector
  , subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeasurementUnitWithCompletionSelector
  , subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeasurementMediumWithCompletionSelector
  , subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLevelValueWithCompletionSelector
  , subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeMeasuredValueWithCompletion:@
readAttributeMeasuredValueWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMeasuredValueWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinMeasuredValueWithCompletion:@
readAttributeMinMeasuredValueWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeMinMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeMinMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxMeasuredValueWithCompletion:@
readAttributeMaxMeasuredValueWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeMaxMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeMaxMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePeakMeasuredValueWithCompletion:@
readAttributePeakMeasuredValueWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributePeakMeasuredValueWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePeakMeasuredValueWindowWithCompletion:@
readAttributePeakMeasuredValueWindowWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributePeakMeasuredValueWindowWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributePeakMeasuredValueWindowWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAverageMeasuredValueWithCompletion:@
readAttributeAverageMeasuredValueWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAverageMeasuredValueWindowWithCompletion:@
readAttributeAverageMeasuredValueWindowWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWindowWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeAverageMeasuredValueWindowWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUncertaintyWithCompletion:@
readAttributeUncertaintyWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeUncertaintyWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeUncertaintyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:@
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMeasurementUnitWithCompletion:@
readAttributeMeasurementUnitWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMeasurementUnitWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeMeasurementUnitWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMeasurementMediumWithCompletion:@
readAttributeMeasurementMediumWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeMeasurementMediumWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeMeasurementMediumWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLevelValueWithCompletion:@
readAttributeLevelValueWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeLevelValueWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeLevelValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterNitrogenDioxideConcentrationMeasurement  completion =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterNitrogenDioxideConcentrationMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> IO (Id MTRBaseClusterNitrogenDioxideConcentrationMeasurement)
init_ mtrBaseClusterNitrogenDioxideConcentrationMeasurement  =
    sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterNitrogenDioxideConcentrationMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterNitrogenDioxideConcentrationMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterNitrogenDioxideConcentrationMeasurement mtrBaseClusterNitrogenDioxideConcentrationMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterNitrogenDioxideConcentrationMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterNitrogenDioxideConcentrationMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterNitrogenDioxideConcentrationMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterNitrogenDioxideConcentrationMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

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

-- | @Selector@ for @readAttributePeakMeasuredValueWithCompletion:@
readAttributePeakMeasuredValueWithCompletionSelector :: Selector
readAttributePeakMeasuredValueWithCompletionSelector = mkSelector "readAttributePeakMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePeakMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeakMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePeakMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeakMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeakMeasuredValueWindowWithCompletion:@
readAttributePeakMeasuredValueWindowWithCompletionSelector :: Selector
readAttributePeakMeasuredValueWindowWithCompletionSelector = mkSelector "readAttributePeakMeasuredValueWindowWithCompletion:"

-- | @Selector@ for @subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePeakMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeakMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePeakMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeakMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWithCompletion:@
readAttributeAverageMeasuredValueWithCompletionSelector :: Selector
readAttributeAverageMeasuredValueWithCompletionSelector = mkSelector "readAttributeAverageMeasuredValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAverageMeasuredValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAverageMeasuredValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAverageMeasuredValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAverageMeasuredValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWindowWithCompletion:@
readAttributeAverageMeasuredValueWindowWithCompletionSelector :: Selector
readAttributeAverageMeasuredValueWindowWithCompletionSelector = mkSelector "readAttributeAverageMeasuredValueWindowWithCompletion:"

-- | @Selector@ for @subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAverageMeasuredValueWindowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAverageMeasuredValueWindowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAverageMeasuredValueWindowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAverageMeasuredValueWindowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUncertaintyWithCompletion:@
readAttributeUncertaintyWithCompletionSelector :: Selector
readAttributeUncertaintyWithCompletionSelector = mkSelector "readAttributeUncertaintyWithCompletion:"

-- | @Selector@ for @subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUncertaintyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUncertaintyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:@
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUncertaintyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUncertaintyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeasurementUnitWithCompletion:@
readAttributeMeasurementUnitWithCompletionSelector :: Selector
readAttributeMeasurementUnitWithCompletionSelector = mkSelector "readAttributeMeasurementUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeasurementUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasurementUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeasurementUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeasurementUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeasurementMediumWithCompletion:@
readAttributeMeasurementMediumWithCompletionSelector :: Selector
readAttributeMeasurementMediumWithCompletionSelector = mkSelector "readAttributeMeasurementMediumWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeasurementMediumWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeasurementMediumWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeasurementMediumWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeasurementMediumWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLevelValueWithCompletion:@
readAttributeLevelValueWithCompletionSelector :: Selector
readAttributeLevelValueWithCompletionSelector = mkSelector "readAttributeLevelValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLevelValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLevelValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLevelValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLevelValueWithClusterStateCache:endpoint:queue:completion:"

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

