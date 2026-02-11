{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Service Area
--
-- The Service Area cluster provides an interface for controlling the areas where a device should operate, and for querying the current area being serviced.
--
-- Generated bindings for @MTRBaseClusterServiceArea@.
module ObjC.Matter.MTRBaseClusterServiceArea
  ( MTRBaseClusterServiceArea
  , IsMTRBaseClusterServiceArea(..)
  , selectAreasWithParams_completion
  , skipAreaWithParams_completion
  , readAttributeSupportedAreasWithCompletion
  , subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedMapsWithCompletion
  , subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedAreasWithCompletion
  , subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentAreaWithCompletion
  , subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completion
  , readAttributeEstimatedEndTimeWithCompletion
  , subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeProgressWithCompletion
  , subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandler
  , readAttributeProgressWithClusterStateCache_endpoint_queue_completion
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
  , selectAreasWithParams_completionSelector
  , skipAreaWithParams_completionSelector
  , readAttributeSupportedAreasWithCompletionSelector
  , subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedMapsWithCompletionSelector
  , subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedAreasWithCompletionSelector
  , subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentAreaWithCompletionSelector
  , subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEstimatedEndTimeWithCompletionSelector
  , subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProgressWithCompletionSelector
  , subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SelectAreas
--
-- This command is used to select a set of device areas, where the device is to operate.
--
-- ObjC selector: @- selectAreasWithParams:completion:@
selectAreasWithParams_completion :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRServiceAreaClusterSelectAreasParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> IO ()
selectAreasWithParams_completion mtrBaseClusterServiceArea  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "selectAreasWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SkipArea
--
-- This command is used to skip the given area, and to attempt operating at other areas on the SupportedAreas attribute list.
--
-- ObjC selector: @- skipAreaWithParams:completion:@
skipAreaWithParams_completion :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRServiceAreaClusterSkipAreaParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> IO ()
skipAreaWithParams_completion mtrBaseClusterServiceArea  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "skipAreaWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedAreasWithCompletion:@
readAttributeSupportedAreasWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeSupportedAreasWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeSupportedAreasWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedMapsWithCompletion:@
readAttributeSupportedMapsWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeSupportedMapsWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeSupportedMapsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSelectedAreasWithCompletion:@
readAttributeSelectedAreasWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeSelectedAreasWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeSelectedAreasWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentAreaWithCompletion:@
readAttributeCurrentAreaWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeCurrentAreaWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeCurrentAreaWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEstimatedEndTimeWithCompletion:@
readAttributeEstimatedEndTimeWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeEstimatedEndTimeWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeEstimatedEndTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProgressWithCompletion:@
readAttributeProgressWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeProgressWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeProgressWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeProgressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProgressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProgressWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterServiceArea  completion =
    sendMsg mtrBaseClusterServiceArea (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRSubscribeParams params) => mtrBaseClusterServiceArea -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterServiceArea  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterServiceArea (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea => mtrBaseClusterServiceArea -> IO (Id MTRBaseClusterServiceArea)
init_ mtrBaseClusterServiceArea  =
    sendMsg mtrBaseClusterServiceArea (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterServiceArea)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterServiceArea"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterServiceArea mtrBaseClusterServiceArea, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterServiceArea -> device -> endpointID -> queue -> IO (Id MTRBaseClusterServiceArea)
initWithDevice_endpointID_queue mtrBaseClusterServiceArea  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterServiceArea (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectAreasWithParams:completion:@
selectAreasWithParams_completionSelector :: Selector
selectAreasWithParams_completionSelector = mkSelector "selectAreasWithParams:completion:"

-- | @Selector@ for @skipAreaWithParams:completion:@
skipAreaWithParams_completionSelector :: Selector
skipAreaWithParams_completionSelector = mkSelector "skipAreaWithParams:completion:"

-- | @Selector@ for @readAttributeSupportedAreasWithCompletion:@
readAttributeSupportedAreasWithCompletionSelector :: Selector
readAttributeSupportedAreasWithCompletionSelector = mkSelector "readAttributeSupportedAreasWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedAreasWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedAreasWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedAreasWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedAreasWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedMapsWithCompletion:@
readAttributeSupportedMapsWithCompletionSelector :: Selector
readAttributeSupportedMapsWithCompletionSelector = mkSelector "readAttributeSupportedMapsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedMapsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedMapsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedMapsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedMapsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedAreasWithCompletion:@
readAttributeSelectedAreasWithCompletionSelector :: Selector
readAttributeSelectedAreasWithCompletionSelector = mkSelector "readAttributeSelectedAreasWithCompletion:"

-- | @Selector@ for @subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSelectedAreasWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedAreasWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSelectedAreasWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedAreasWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentAreaWithCompletion:@
readAttributeCurrentAreaWithCompletionSelector :: Selector
readAttributeCurrentAreaWithCompletionSelector = mkSelector "readAttributeCurrentAreaWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentAreaWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentAreaWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentAreaWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentAreaWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEstimatedEndTimeWithCompletion:@
readAttributeEstimatedEndTimeWithCompletionSelector :: Selector
readAttributeEstimatedEndTimeWithCompletionSelector = mkSelector "readAttributeEstimatedEndTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEstimatedEndTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEstimatedEndTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEstimatedEndTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEstimatedEndTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProgressWithCompletion:@
readAttributeProgressWithCompletionSelector :: Selector
readAttributeProgressWithCompletionSelector = mkSelector "readAttributeProgressWithCompletion:"

-- | @Selector@ for @subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProgressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProgressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProgressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProgressWithClusterStateCache:endpoint:queue:completion:"

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

