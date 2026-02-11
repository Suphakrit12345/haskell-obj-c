{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Meter Identification
--
-- This Meter Identification Cluster provides attributes for determining advanced information about utility metering device.
--
-- Generated bindings for @MTRBaseClusterMeterIdentification@.
module ObjC.Matter.MTRBaseClusterMeterIdentification
  ( MTRBaseClusterMeterIdentification
  , IsMTRBaseClusterMeterIdentification(..)
  , readAttributeMeterTypeWithCompletion
  , subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributePointOfDeliveryWithCompletion
  , subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandler
  , readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completion
  , readAttributeMeterSerialNumberWithCompletion
  , subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeProtocolVersionWithCompletion
  , subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerThresholdWithCompletion
  , subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMeterTypeWithCompletionSelector
  , subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePointOfDeliveryWithCompletionSelector
  , subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMeterSerialNumberWithCompletionSelector
  , subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProtocolVersionWithCompletionSelector
  , subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerThresholdWithCompletionSelector
  , subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeMeterTypeWithCompletion:@
readAttributeMeterTypeWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeMeterTypeWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeMeterTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePointOfDeliveryWithCompletion:@
readAttributePointOfDeliveryWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributePointOfDeliveryWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributePointOfDeliveryWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:@
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMeterSerialNumberWithCompletion:@
readAttributeMeterSerialNumberWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeMeterSerialNumberWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeMeterSerialNumberWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProtocolVersionWithCompletion:@
readAttributeProtocolVersionWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeProtocolVersionWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeProtocolVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePowerThresholdWithCompletion:@
readAttributePowerThresholdWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributePowerThresholdWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributePowerThresholdWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMeterIdentification  completion =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRSubscribeParams params) => mtrBaseClusterMeterIdentification -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMeterIdentification  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMeterIdentification (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification => mtrBaseClusterMeterIdentification -> IO (Id MTRBaseClusterMeterIdentification)
init_ mtrBaseClusterMeterIdentification  =
    sendMsg mtrBaseClusterMeterIdentification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterMeterIdentification)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMeterIdentification"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMeterIdentification mtrBaseClusterMeterIdentification, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMeterIdentification -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMeterIdentification)
initWithDevice_endpointID_queue mtrBaseClusterMeterIdentification  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterMeterIdentification (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeterTypeWithCompletion:@
readAttributeMeterTypeWithCompletionSelector :: Selector
readAttributeMeterTypeWithCompletionSelector = mkSelector "readAttributeMeterTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeterTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeterTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeterTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeterTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePointOfDeliveryWithCompletion:@
readAttributePointOfDeliveryWithCompletionSelector :: Selector
readAttributePointOfDeliveryWithCompletionSelector = mkSelector "readAttributePointOfDeliveryWithCompletion:"

-- | @Selector@ for @subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePointOfDeliveryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePointOfDeliveryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:@
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePointOfDeliveryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePointOfDeliveryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMeterSerialNumberWithCompletion:@
readAttributeMeterSerialNumberWithCompletionSelector :: Selector
readAttributeMeterSerialNumberWithCompletionSelector = mkSelector "readAttributeMeterSerialNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMeterSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMeterSerialNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMeterSerialNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMeterSerialNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProtocolVersionWithCompletion:@
readAttributeProtocolVersionWithCompletionSelector :: Selector
readAttributeProtocolVersionWithCompletionSelector = mkSelector "readAttributeProtocolVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProtocolVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProtocolVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProtocolVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProtocolVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerThresholdWithCompletion:@
readAttributePowerThresholdWithCompletionSelector :: Selector
readAttributePowerThresholdWithCompletionSelector = mkSelector "readAttributePowerThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePowerThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePowerThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerThresholdWithClusterStateCache:endpoint:queue:completion:"

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

