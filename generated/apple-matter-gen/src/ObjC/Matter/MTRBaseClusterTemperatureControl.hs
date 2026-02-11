{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Temperature Control
--
-- Attributes and commands for configuring the temperature control, and reporting temperature.
--
-- Generated bindings for @MTRBaseClusterTemperatureControl@.
module ObjC.Matter.MTRBaseClusterTemperatureControl
  ( MTRBaseClusterTemperatureControl
  , IsMTRBaseClusterTemperatureControl(..)
  , setTemperatureWithParams_completion
  , setTemperatureWithCompletion
  , readAttributeTemperatureSetpointWithCompletion
  , subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandler
  , readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinTemperatureWithCompletion
  , subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxTemperatureWithCompletion
  , subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completion
  , readAttributeStepWithCompletion
  , subscribeAttributeStepWithParams_subscriptionEstablished_reportHandler
  , readAttributeStepWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedTemperatureLevelWithCompletion
  , subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedTemperatureLevelsWithCompletion
  , subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completion
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
  , setTemperatureWithParams_completionSelector
  , setTemperatureWithCompletionSelector
  , readAttributeTemperatureSetpointWithCompletionSelector
  , subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinTemperatureWithCompletionSelector
  , subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxTemperatureWithCompletionSelector
  , subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStepWithCompletionSelector
  , subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedTemperatureLevelWithCompletionSelector
  , subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedTemperatureLevelsWithCompletionSelector
  , subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SetTemperature
--
-- The SetTemperature command SHALL have the following data fields:
--
-- ObjC selector: @- setTemperatureWithParams:completion:@
setTemperatureWithParams_completion :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRTemperatureControlClusterSetTemperatureParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> IO ()
setTemperatureWithParams_completion mtrBaseClusterTemperatureControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "setTemperatureWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTemperatureWithCompletion:@
setTemperatureWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
setTemperatureWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "setTemperatureWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTemperatureSetpointWithCompletion:@
readAttributeTemperatureSetpointWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeTemperatureSetpointWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeTemperatureSetpointWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:@
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinTemperatureWithCompletion:@
readAttributeMinTemperatureWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeMinTemperatureWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeMinTemperatureWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxTemperatureWithCompletion:@
readAttributeMaxTemperatureWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeMaxTemperatureWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeMaxTemperatureWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStepWithCompletion:@
readAttributeStepWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeStepWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeStepWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStepWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStepWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSelectedTemperatureLevelWithCompletion:@
readAttributeSelectedTemperatureLevelWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeSelectedTemperatureLevelWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeSelectedTemperatureLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedTemperatureLevelsWithCompletion:@
readAttributeSupportedTemperatureLevelsWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeSupportedTemperatureLevelsWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeSupportedTemperatureLevelsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTemperatureControl  completion =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRSubscribeParams params) => mtrBaseClusterTemperatureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTemperatureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTemperatureControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl => mtrBaseClusterTemperatureControl -> IO (Id MTRBaseClusterTemperatureControl)
init_ mtrBaseClusterTemperatureControl  =
    sendMsg mtrBaseClusterTemperatureControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterTemperatureControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTemperatureControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTemperatureControl mtrBaseClusterTemperatureControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTemperatureControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTemperatureControl)
initWithDevice_endpointID_queue mtrBaseClusterTemperatureControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterTemperatureControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTemperatureWithParams:completion:@
setTemperatureWithParams_completionSelector :: Selector
setTemperatureWithParams_completionSelector = mkSelector "setTemperatureWithParams:completion:"

-- | @Selector@ for @setTemperatureWithCompletion:@
setTemperatureWithCompletionSelector :: Selector
setTemperatureWithCompletionSelector = mkSelector "setTemperatureWithCompletion:"

-- | @Selector@ for @readAttributeTemperatureSetpointWithCompletion:@
readAttributeTemperatureSetpointWithCompletionSelector :: Selector
readAttributeTemperatureSetpointWithCompletionSelector = mkSelector "readAttributeTemperatureSetpointWithCompletion:"

-- | @Selector@ for @subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTemperatureSetpointWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTemperatureSetpointWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:@
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTemperatureSetpointWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTemperatureSetpointWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinTemperatureWithCompletion:@
readAttributeMinTemperatureWithCompletionSelector :: Selector
readAttributeMinTemperatureWithCompletionSelector = mkSelector "readAttributeMinTemperatureWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinTemperatureWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinTemperatureWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinTemperatureWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinTemperatureWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxTemperatureWithCompletion:@
readAttributeMaxTemperatureWithCompletionSelector :: Selector
readAttributeMaxTemperatureWithCompletionSelector = mkSelector "readAttributeMaxTemperatureWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxTemperatureWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxTemperatureWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxTemperatureWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxTemperatureWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStepWithCompletion:@
readAttributeStepWithCompletionSelector :: Selector
readAttributeStepWithCompletionSelector = mkSelector "readAttributeStepWithCompletion:"

-- | @Selector@ for @subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStepWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStepWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStepWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStepWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedTemperatureLevelWithCompletion:@
readAttributeSelectedTemperatureLevelWithCompletionSelector :: Selector
readAttributeSelectedTemperatureLevelWithCompletionSelector = mkSelector "readAttributeSelectedTemperatureLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSelectedTemperatureLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedTemperatureLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSelectedTemperatureLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedTemperatureLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedTemperatureLevelsWithCompletion:@
readAttributeSupportedTemperatureLevelsWithCompletionSelector :: Selector
readAttributeSupportedTemperatureLevelsWithCompletionSelector = mkSelector "readAttributeSupportedTemperatureLevelsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedTemperatureLevelsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedTemperatureLevelsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedTemperatureLevelsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedTemperatureLevelsWithClusterStateCache:endpoint:queue:completion:"

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

