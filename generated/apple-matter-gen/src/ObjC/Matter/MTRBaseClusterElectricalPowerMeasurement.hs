{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Power Measurement
--
-- This cluster provides a mechanism for querying data about electrical power as measured by the server.
--
-- Generated bindings for @MTRBaseClusterElectricalPowerMeasurement@.
module ObjC.Matter.MTRBaseClusterElectricalPowerMeasurement
  ( MTRBaseClusterElectricalPowerMeasurement
  , IsMTRBaseClusterElectricalPowerMeasurement(..)
  , readAttributePowerModeWithCompletion
  , subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeNumberOfMeasurementTypesWithCompletion
  , subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandler
  , readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completion
  , readAttributeAccuracyWithCompletion
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion
  , readAttributeRangesWithCompletion
  , subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandler
  , readAttributeRangesWithClusterStateCache_endpoint_queue_completion
  , readAttributeVoltageWithCompletion
  , subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandler
  , readAttributeVoltageWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveCurrentWithCompletion
  , subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeReactiveCurrentWithCompletion
  , subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeApparentCurrentWithCompletion
  , subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeActivePowerWithCompletion
  , subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeActivePowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeReactivePowerWithCompletion
  , subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeApparentPowerWithCompletion
  , subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeRMSVoltageWithCompletion
  , subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandler
  , readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completion
  , readAttributeRMSCurrentWithCompletion
  , subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeRMSPowerWithCompletion
  , subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeFrequencyWithCompletion
  , subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeFrequencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeHarmonicCurrentsWithCompletion
  , subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeHarmonicPhasesWithCompletion
  , subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandler
  , readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerFactorWithCompletion
  , subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerFactorWithClusterStateCache_endpoint_queue_completion
  , readAttributeNeutralCurrentWithCompletion
  , subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completion
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
  , readAttributePowerModeWithCompletionSelector
  , subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNumberOfMeasurementTypesWithCompletionSelector
  , subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAccuracyWithCompletionSelector
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRangesWithCompletionSelector
  , subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVoltageWithCompletionSelector
  , subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveCurrentWithCompletionSelector
  , subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReactiveCurrentWithCompletionSelector
  , subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApparentCurrentWithCompletionSelector
  , subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActivePowerWithCompletionSelector
  , subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReactivePowerWithCompletionSelector
  , subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApparentPowerWithCompletionSelector
  , subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRMSVoltageWithCompletionSelector
  , subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRMSCurrentWithCompletionSelector
  , subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRMSPowerWithCompletionSelector
  , subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFrequencyWithCompletionSelector
  , subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHarmonicCurrentsWithCompletionSelector
  , subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHarmonicPhasesWithCompletionSelector
  , subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerFactorWithCompletionSelector
  , subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNeutralCurrentWithCompletionSelector
  , subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributePowerModeWithCompletion:@
readAttributePowerModeWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributePowerModeWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributePowerModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNumberOfMeasurementTypesWithCompletion:@
readAttributeNumberOfMeasurementTypesWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeNumberOfMeasurementTypesWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeNumberOfMeasurementTypesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeAccuracyWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeAccuracyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRangesWithCompletion:@
readAttributeRangesWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRangesWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeRangesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRangesWithClusterStateCache:endpoint:queue:completion:@
readAttributeRangesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRangesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRangesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVoltageWithCompletion:@
readAttributeVoltageWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeVoltageWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeVoltageWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeVoltageWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVoltageWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveCurrentWithCompletion:@
readAttributeActiveCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeActiveCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeActiveCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeReactiveCurrentWithCompletion:@
readAttributeReactiveCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeReactiveCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeReactiveCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApparentCurrentWithCompletion:@
readAttributeApparentCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeApparentCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeApparentCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActivePowerWithCompletion:@
readAttributeActivePowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeActivePowerWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeActivePowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeReactivePowerWithCompletion:@
readAttributeReactivePowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeReactivePowerWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeReactivePowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApparentPowerWithCompletion:@
readAttributeApparentPowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeApparentPowerWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeApparentPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRMSVoltageWithCompletion:@
readAttributeRMSVoltageWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRMSVoltageWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeRMSVoltageWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRMSCurrentWithCompletion:@
readAttributeRMSCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRMSCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeRMSCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRMSPowerWithCompletion:@
readAttributeRMSPowerWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeRMSPowerWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeRMSPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFrequencyWithCompletion:@
readAttributeFrequencyWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeFrequencyWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeFrequencyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHarmonicCurrentsWithCompletion:@
readAttributeHarmonicCurrentsWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeHarmonicCurrentsWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeHarmonicCurrentsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHarmonicPhasesWithCompletion:@
readAttributeHarmonicPhasesWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeHarmonicPhasesWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeHarmonicPhasesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePowerFactorWithCompletion:@
readAttributePowerFactorWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributePowerFactorWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributePowerFactorWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNeutralCurrentWithCompletion:@
readAttributeNeutralCurrentWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeNeutralCurrentWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeNeutralCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterElectricalPowerMeasurement  completion =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalPowerMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalPowerMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement => mtrBaseClusterElectricalPowerMeasurement -> IO (Id MTRBaseClusterElectricalPowerMeasurement)
init_ mtrBaseClusterElectricalPowerMeasurement  =
    sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterElectricalPowerMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalPowerMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterElectricalPowerMeasurement mtrBaseClusterElectricalPowerMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterElectricalPowerMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterElectricalPowerMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterElectricalPowerMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterElectricalPowerMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePowerModeWithCompletion:@
readAttributePowerModeWithCompletionSelector :: Selector
readAttributePowerModeWithCompletionSelector = mkSelector "readAttributePowerModeWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePowerModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePowerModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNumberOfMeasurementTypesWithCompletion:@
readAttributeNumberOfMeasurementTypesWithCompletionSelector :: Selector
readAttributeNumberOfMeasurementTypesWithCompletionSelector = mkSelector "readAttributeNumberOfMeasurementTypesWithCompletion:"

-- | @Selector@ for @subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNumberOfMeasurementTypesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNumberOfMeasurementTypesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNumberOfMeasurementTypesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNumberOfMeasurementTypesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletionSelector :: Selector
readAttributeAccuracyWithCompletionSelector = mkSelector "readAttributeAccuracyWithCompletion:"

-- | @Selector@ for @subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRangesWithCompletion:@
readAttributeRangesWithCompletionSelector :: Selector
readAttributeRangesWithCompletionSelector = mkSelector "readAttributeRangesWithCompletion:"

-- | @Selector@ for @subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRangesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRangesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRangesWithClusterStateCache:endpoint:queue:completion:@
readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRangesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRangesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVoltageWithCompletion:@
readAttributeVoltageWithCompletionSelector :: Selector
readAttributeVoltageWithCompletionSelector = mkSelector "readAttributeVoltageWithCompletion:"

-- | @Selector@ for @subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVoltageWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVoltageWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeVoltageWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVoltageWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveCurrentWithCompletion:@
readAttributeActiveCurrentWithCompletionSelector :: Selector
readAttributeActiveCurrentWithCompletionSelector = mkSelector "readAttributeActiveCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReactiveCurrentWithCompletion:@
readAttributeReactiveCurrentWithCompletionSelector :: Selector
readAttributeReactiveCurrentWithCompletionSelector = mkSelector "readAttributeReactiveCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReactiveCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReactiveCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeReactiveCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReactiveCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApparentCurrentWithCompletion:@
readAttributeApparentCurrentWithCompletionSelector :: Selector
readAttributeApparentCurrentWithCompletionSelector = mkSelector "readAttributeApparentCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApparentCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApparentCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApparentCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApparentCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActivePowerWithCompletion:@
readAttributeActivePowerWithCompletionSelector :: Selector
readAttributeActivePowerWithCompletionSelector = mkSelector "readAttributeActivePowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActivePowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActivePowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActivePowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActivePowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReactivePowerWithCompletion:@
readAttributeReactivePowerWithCompletionSelector :: Selector
readAttributeReactivePowerWithCompletionSelector = mkSelector "readAttributeReactivePowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReactivePowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReactivePowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeReactivePowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReactivePowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApparentPowerWithCompletion:@
readAttributeApparentPowerWithCompletionSelector :: Selector
readAttributeApparentPowerWithCompletionSelector = mkSelector "readAttributeApparentPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApparentPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApparentPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApparentPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApparentPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRMSVoltageWithCompletion:@
readAttributeRMSVoltageWithCompletionSelector :: Selector
readAttributeRMSVoltageWithCompletionSelector = mkSelector "readAttributeRMSVoltageWithCompletion:"

-- | @Selector@ for @subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRMSVoltageWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRMSVoltageWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRMSVoltageWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRMSVoltageWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRMSCurrentWithCompletion:@
readAttributeRMSCurrentWithCompletionSelector :: Selector
readAttributeRMSCurrentWithCompletionSelector = mkSelector "readAttributeRMSCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRMSCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRMSCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRMSCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRMSCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRMSPowerWithCompletion:@
readAttributeRMSPowerWithCompletionSelector :: Selector
readAttributeRMSPowerWithCompletionSelector = mkSelector "readAttributeRMSPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRMSPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRMSPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRMSPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRMSPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFrequencyWithCompletion:@
readAttributeFrequencyWithCompletionSelector :: Selector
readAttributeFrequencyWithCompletionSelector = mkSelector "readAttributeFrequencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFrequencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFrequencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFrequencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFrequencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHarmonicCurrentsWithCompletion:@
readAttributeHarmonicCurrentsWithCompletionSelector :: Selector
readAttributeHarmonicCurrentsWithCompletionSelector = mkSelector "readAttributeHarmonicCurrentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHarmonicCurrentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHarmonicCurrentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHarmonicCurrentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHarmonicCurrentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHarmonicPhasesWithCompletion:@
readAttributeHarmonicPhasesWithCompletionSelector :: Selector
readAttributeHarmonicPhasesWithCompletionSelector = mkSelector "readAttributeHarmonicPhasesWithCompletion:"

-- | @Selector@ for @subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHarmonicPhasesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHarmonicPhasesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHarmonicPhasesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHarmonicPhasesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerFactorWithCompletion:@
readAttributePowerFactorWithCompletionSelector :: Selector
readAttributePowerFactorWithCompletionSelector = mkSelector "readAttributePowerFactorWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePowerFactorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerFactorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePowerFactorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerFactorWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNeutralCurrentWithCompletion:@
readAttributeNeutralCurrentWithCompletionSelector :: Selector
readAttributeNeutralCurrentWithCompletionSelector = mkSelector "readAttributeNeutralCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNeutralCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNeutralCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNeutralCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNeutralCurrentWithClusterStateCache:endpoint:queue:completion:"

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

