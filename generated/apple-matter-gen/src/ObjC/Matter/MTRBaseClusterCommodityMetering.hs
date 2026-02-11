{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Metering
--
-- The Commodity Metering Cluster provides the mechanism for communicating commodity consumption information within a premises.
--
-- Generated bindings for @MTRBaseClusterCommodityMetering@.
module ObjC.Matter.MTRBaseClusterCommodityMetering
  ( MTRBaseClusterCommodityMetering
  , IsMTRBaseClusterCommodityMetering(..)
  , readAttributeMeteredQuantityWithCompletion
  , subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeteredQuantityTimestampWithCompletion
  , subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffUnitWithCompletion
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumMeteredQuantitiesWithCompletion
  , subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMeteredQuantityWithCompletionSelector
  , subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeteredQuantityTimestampWithCompletionSelector
  , subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffUnitWithCompletionSelector
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumMeteredQuantitiesWithCompletionSelector
  , subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeMeteredQuantityWithCompletion:@
readAttributeMeteredQuantityWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeMeteredQuantityWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeMeteredQuantityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMeteredQuantityTimestampWithCompletion:@
readAttributeMeteredQuantityTimestampWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeMeteredQuantityTimestampWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeMeteredQuantityTimestampWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeTariffUnitWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeTariffUnitWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaximumMeteredQuantitiesWithCompletion:@
readAttributeMaximumMeteredQuantitiesWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeMaximumMeteredQuantitiesWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeMaximumMeteredQuantitiesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCommodityMetering  completion =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRSubscribeParams params) => mtrBaseClusterCommodityMetering -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityMetering  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterCommodityMetering (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering => mtrBaseClusterCommodityMetering -> IO (Id MTRBaseClusterCommodityMetering)
init_ mtrBaseClusterCommodityMetering  =
    sendMsg mtrBaseClusterCommodityMetering (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterCommodityMetering)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityMetering"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCommodityMetering mtrBaseClusterCommodityMetering, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCommodityMetering -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCommodityMetering)
initWithDevice_endpointID_queue mtrBaseClusterCommodityMetering  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterCommodityMetering (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeteredQuantityWithCompletion:@
readAttributeMeteredQuantityWithCompletionSelector :: Selector
readAttributeMeteredQuantityWithCompletionSelector = mkSelector "readAttributeMeteredQuantityWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeteredQuantityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeteredQuantityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeteredQuantityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeteredQuantityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeteredQuantityTimestampWithCompletion:@
readAttributeMeteredQuantityTimestampWithCompletionSelector :: Selector
readAttributeMeteredQuantityTimestampWithCompletionSelector = mkSelector "readAttributeMeteredQuantityTimestampWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeteredQuantityTimestampWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeteredQuantityTimestampWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeteredQuantityTimestampWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeteredQuantityTimestampWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletionSelector :: Selector
readAttributeTariffUnitWithCompletionSelector = mkSelector "readAttributeTariffUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumMeteredQuantitiesWithCompletion:@
readAttributeMaximumMeteredQuantitiesWithCompletionSelector :: Selector
readAttributeMaximumMeteredQuantitiesWithCompletionSelector = mkSelector "readAttributeMaximumMeteredQuantitiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaximumMeteredQuantitiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumMeteredQuantitiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaximumMeteredQuantitiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumMeteredQuantitiesWithClusterStateCache:endpoint:queue:completion:"

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

