{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Water Heater Management
--
-- This cluster is used to allow clients to control the operation of a hot water heating appliance so that it can be used with energy management.
--
-- Generated bindings for @MTRBaseClusterWaterHeaterManagement@.
module ObjC.Matter.MTRBaseClusterWaterHeaterManagement
  ( MTRBaseClusterWaterHeaterManagement
  , IsMTRBaseClusterWaterHeaterManagement(..)
  , boostWithParams_completion
  , cancelBoostWithParams_completion
  , cancelBoostWithCompletion
  , readAttributeHeaterTypesWithCompletion
  , subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandler
  , readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completion
  , readAttributeHeatDemandWithCompletion
  , subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandler
  , readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completion
  , readAttributeTankVolumeWithCompletion
  , subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandler
  , readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completion
  , readAttributeEstimatedHeatRequiredWithCompletion
  , subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandler
  , readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completion
  , readAttributeTankPercentageWithCompletion
  , subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandler
  , readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completion
  , readAttributeBoostStateWithCompletion
  , subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeBoostStateWithClusterStateCache_endpoint_queue_completion
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
  , boostWithParams_completionSelector
  , cancelBoostWithParams_completionSelector
  , cancelBoostWithCompletionSelector
  , readAttributeHeaterTypesWithCompletionSelector
  , subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHeatDemandWithCompletionSelector
  , subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTankVolumeWithCompletionSelector
  , subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEstimatedHeatRequiredWithCompletionSelector
  , subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTankPercentageWithCompletionSelector
  , subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBoostStateWithCompletionSelector
  , subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command Boost
--
-- Allows a client to request that the water heater is put into a Boost state.
--
-- ObjC selector: @- boostWithParams:completion:@
boostWithParams_completion :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterBoostParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> IO ()
boostWithParams_completion mtrBaseClusterWaterHeaterManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "boostWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CancelBoost
--
-- Allows a client to cancel an ongoing Boost operation.
--
-- ObjC selector: @- cancelBoostWithParams:completion:@
cancelBoostWithParams_completion :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterCancelBoostParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> IO ()
cancelBoostWithParams_completion mtrBaseClusterWaterHeaterManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "cancelBoostWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelBoostWithCompletion:@
cancelBoostWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
cancelBoostWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "cancelBoostWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHeaterTypesWithCompletion:@
readAttributeHeaterTypesWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeHeaterTypesWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeHeaterTypesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHeatDemandWithCompletion:@
readAttributeHeatDemandWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeHeatDemandWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeHeatDemandWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTankVolumeWithCompletion:@
readAttributeTankVolumeWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeTankVolumeWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeTankVolumeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEstimatedHeatRequiredWithCompletion:@
readAttributeEstimatedHeatRequiredWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeEstimatedHeatRequiredWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeEstimatedHeatRequiredWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTankPercentageWithCompletion:@
readAttributeTankPercentageWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeTankPercentageWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeTankPercentageWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBoostStateWithCompletion:@
readAttributeBoostStateWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeBoostStateWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeBoostStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWaterHeaterManagement  completion =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRSubscribeParams params) => mtrBaseClusterWaterHeaterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWaterHeaterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement => mtrBaseClusterWaterHeaterManagement -> IO (Id MTRBaseClusterWaterHeaterManagement)
init_ mtrBaseClusterWaterHeaterManagement  =
    sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterWaterHeaterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWaterHeaterManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWaterHeaterManagement mtrBaseClusterWaterHeaterManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWaterHeaterManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWaterHeaterManagement)
initWithDevice_endpointID_queue mtrBaseClusterWaterHeaterManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterWaterHeaterManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostWithParams:completion:@
boostWithParams_completionSelector :: Selector
boostWithParams_completionSelector = mkSelector "boostWithParams:completion:"

-- | @Selector@ for @cancelBoostWithParams:completion:@
cancelBoostWithParams_completionSelector :: Selector
cancelBoostWithParams_completionSelector = mkSelector "cancelBoostWithParams:completion:"

-- | @Selector@ for @cancelBoostWithCompletion:@
cancelBoostWithCompletionSelector :: Selector
cancelBoostWithCompletionSelector = mkSelector "cancelBoostWithCompletion:"

-- | @Selector@ for @readAttributeHeaterTypesWithCompletion:@
readAttributeHeaterTypesWithCompletionSelector :: Selector
readAttributeHeaterTypesWithCompletionSelector = mkSelector "readAttributeHeaterTypesWithCompletion:"

-- | @Selector@ for @subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHeaterTypesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHeaterTypesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHeaterTypesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHeaterTypesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHeatDemandWithCompletion:@
readAttributeHeatDemandWithCompletionSelector :: Selector
readAttributeHeatDemandWithCompletionSelector = mkSelector "readAttributeHeatDemandWithCompletion:"

-- | @Selector@ for @subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHeatDemandWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHeatDemandWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:@
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHeatDemandWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHeatDemandWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTankVolumeWithCompletion:@
readAttributeTankVolumeWithCompletionSelector :: Selector
readAttributeTankVolumeWithCompletionSelector = mkSelector "readAttributeTankVolumeWithCompletion:"

-- | @Selector@ for @subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTankVolumeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTankVolumeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTankVolumeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTankVolumeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEstimatedHeatRequiredWithCompletion:@
readAttributeEstimatedHeatRequiredWithCompletionSelector :: Selector
readAttributeEstimatedHeatRequiredWithCompletionSelector = mkSelector "readAttributeEstimatedHeatRequiredWithCompletion:"

-- | @Selector@ for @subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEstimatedHeatRequiredWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEstimatedHeatRequiredWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEstimatedHeatRequiredWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEstimatedHeatRequiredWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTankPercentageWithCompletion:@
readAttributeTankPercentageWithCompletionSelector :: Selector
readAttributeTankPercentageWithCompletionSelector = mkSelector "readAttributeTankPercentageWithCompletion:"

-- | @Selector@ for @subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTankPercentageWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTankPercentageWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:@
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTankPercentageWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTankPercentageWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBoostStateWithCompletion:@
readAttributeBoostStateWithCompletionSelector :: Selector
readAttributeBoostStateWithCompletionSelector = mkSelector "readAttributeBoostStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBoostStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBoostStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBoostStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBoostStateWithClusterStateCache:endpoint:queue:completion:"

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

