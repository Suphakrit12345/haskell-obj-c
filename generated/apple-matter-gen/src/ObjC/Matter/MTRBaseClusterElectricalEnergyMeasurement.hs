{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Energy Measurement
--
-- This cluster provides a mechanism for querying data about the electrical energy imported or provided by the server.
--
-- Generated bindings for @MTRBaseClusterElectricalEnergyMeasurement@.
module ObjC.Matter.MTRBaseClusterElectricalEnergyMeasurement
  ( MTRBaseClusterElectricalEnergyMeasurement
  , IsMTRBaseClusterElectricalEnergyMeasurement(..)
  , readAttributeAccuracyWithCompletion
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion
  , readAttributeCumulativeEnergyImportedWithCompletion
  , subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completion
  , readAttributeCumulativeEnergyExportedWithCompletion
  , subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completion
  , readAttributePeriodicEnergyImportedWithCompletion
  , subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandler
  , readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completion
  , readAttributePeriodicEnergyExportedWithCompletion
  , subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandler
  , readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completion
  , readAttributeCumulativeEnergyResetWithCompletion
  , subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandler
  , readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeAccuracyWithCompletionSelector
  , subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCumulativeEnergyImportedWithCompletionSelector
  , subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCumulativeEnergyExportedWithCompletionSelector
  , subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeriodicEnergyImportedWithCompletionSelector
  , subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePeriodicEnergyExportedWithCompletionSelector
  , subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCumulativeEnergyResetWithCompletionSelector
  , subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeAccuracyWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeAccuracyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCumulativeEnergyImportedWithCompletion:@
readAttributeCumulativeEnergyImportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeCumulativeEnergyImportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeCumulativeEnergyImportedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCumulativeEnergyExportedWithCompletion:@
readAttributeCumulativeEnergyExportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeCumulativeEnergyExportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeCumulativeEnergyExportedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePeriodicEnergyImportedWithCompletion:@
readAttributePeriodicEnergyImportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributePeriodicEnergyImportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributePeriodicEnergyImportedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePeriodicEnergyExportedWithCompletion:@
readAttributePeriodicEnergyExportedWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributePeriodicEnergyExportedWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributePeriodicEnergyExportedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCumulativeEnergyResetWithCompletion:@
readAttributeCumulativeEnergyResetWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeCumulativeEnergyResetWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeCumulativeEnergyResetWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterElectricalEnergyMeasurement  completion =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRSubscribeParams params) => mtrBaseClusterElectricalEnergyMeasurement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterElectricalEnergyMeasurement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement => mtrBaseClusterElectricalEnergyMeasurement -> IO (Id MTRBaseClusterElectricalEnergyMeasurement)
init_ mtrBaseClusterElectricalEnergyMeasurement  =
    sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterElectricalEnergyMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterElectricalEnergyMeasurement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterElectricalEnergyMeasurement mtrBaseClusterElectricalEnergyMeasurement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterElectricalEnergyMeasurement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterElectricalEnergyMeasurement)
initWithDevice_endpointID_queue mtrBaseClusterElectricalEnergyMeasurement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterElectricalEnergyMeasurement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAccuracyWithCompletion:@
readAttributeAccuracyWithCompletionSelector :: Selector
readAttributeAccuracyWithCompletionSelector = mkSelector "readAttributeAccuracyWithCompletion:"

-- | @Selector@ for @subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAccuracyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccuracyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAccuracyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAccuracyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCumulativeEnergyImportedWithCompletion:@
readAttributeCumulativeEnergyImportedWithCompletionSelector :: Selector
readAttributeCumulativeEnergyImportedWithCompletionSelector = mkSelector "readAttributeCumulativeEnergyImportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCumulativeEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCumulativeEnergyImportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCumulativeEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCumulativeEnergyImportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCumulativeEnergyExportedWithCompletion:@
readAttributeCumulativeEnergyExportedWithCompletionSelector :: Selector
readAttributeCumulativeEnergyExportedWithCompletionSelector = mkSelector "readAttributeCumulativeEnergyExportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCumulativeEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCumulativeEnergyExportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCumulativeEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCumulativeEnergyExportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeriodicEnergyImportedWithCompletion:@
readAttributePeriodicEnergyImportedWithCompletionSelector :: Selector
readAttributePeriodicEnergyImportedWithCompletionSelector = mkSelector "readAttributePeriodicEnergyImportedWithCompletion:"

-- | @Selector@ for @subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePeriodicEnergyImportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeriodicEnergyImportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePeriodicEnergyImportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeriodicEnergyImportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePeriodicEnergyExportedWithCompletion:@
readAttributePeriodicEnergyExportedWithCompletionSelector :: Selector
readAttributePeriodicEnergyExportedWithCompletionSelector = mkSelector "readAttributePeriodicEnergyExportedWithCompletion:"

-- | @Selector@ for @subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePeriodicEnergyExportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePeriodicEnergyExportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:@
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePeriodicEnergyExportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePeriodicEnergyExportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCumulativeEnergyResetWithCompletion:@
readAttributeCumulativeEnergyResetWithCompletionSelector :: Selector
readAttributeCumulativeEnergyResetWithCompletionSelector = mkSelector "readAttributeCumulativeEnergyResetWithCompletion:"

-- | @Selector@ for @subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCumulativeEnergyResetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCumulativeEnergyResetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:@
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCumulativeEnergyResetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCumulativeEnergyResetWithClusterStateCache:endpoint:queue:completion:"

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

