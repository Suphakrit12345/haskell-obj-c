{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Dimension
--
-- This cluster provides an interface to reflect and control a closure's range of movement, usually involving a panel, by using 6-axis framework.
--
-- Generated bindings for @MTRBaseClusterClosureDimension@.
module ObjC.Matter.MTRBaseClusterClosureDimension
  ( MTRBaseClusterClosureDimension
  , IsMTRBaseClusterClosureDimension(..)
  , setTargetWithParams_completion
  , setTargetWithCompletion
  , stepWithParams_completion
  , readAttributeCurrentStateWithCompletion
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetStateWithCompletion
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeResolutionWithCompletion
  , subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandler
  , readAttributeResolutionWithClusterStateCache_endpoint_queue_completion
  , readAttributeStepValueWithCompletion
  , subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandler
  , readAttributeStepValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeUnitWithCompletion
  , subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeUnitRangeWithCompletion
  , subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandler
  , readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completion
  , readAttributeLimitRangeWithCompletion
  , subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTranslationDirectionWithCompletion
  , subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandler
  , readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completion
  , readAttributeRotationAxisWithCompletion
  , subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandler
  , readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverflowWithCompletion
  , subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverflowWithClusterStateCache_endpoint_queue_completion
  , readAttributeModulationTypeWithCompletion
  , subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeLatchControlModesWithCompletion
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion
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
  , setTargetWithParams_completionSelector
  , setTargetWithCompletionSelector
  , stepWithParams_completionSelector
  , readAttributeCurrentStateWithCompletionSelector
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetStateWithCompletionSelector
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeResolutionWithCompletionSelector
  , subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStepValueWithCompletionSelector
  , subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUnitWithCompletionSelector
  , subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUnitRangeWithCompletionSelector
  , subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLimitRangeWithCompletionSelector
  , subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTranslationDirectionWithCompletionSelector
  , subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRotationAxisWithCompletionSelector
  , subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverflowWithCompletionSelector
  , subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeModulationTypeWithCompletionSelector
  , subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLatchControlModesWithCompletionSelector
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SetTarget
--
-- This command is used to move a dimension of the closure to a target position.
--
-- ObjC selector: @- setTargetWithParams:completion:@
setTargetWithParams_completion :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRClosureDimensionClusterSetTargetParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> IO ()
setTargetWithParams_completion mtrBaseClusterClosureDimension  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "setTargetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTargetWithCompletion:@
setTargetWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
setTargetWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "setTargetWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Step
--
-- This command is used to move a dimension of the closure to a target position by a number of steps.
--
-- ObjC selector: @- stepWithParams:completion:@
stepWithParams_completion :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRClosureDimensionClusterStepParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> IO ()
stepWithParams_completion mtrBaseClusterClosureDimension  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "stepWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeCurrentStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeTargetStateWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeTargetStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeResolutionWithCompletion:@
readAttributeResolutionWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeResolutionWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeResolutionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:@
readAttributeResolutionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeResolutionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStepValueWithCompletion:@
readAttributeStepValueWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeStepValueWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeStepValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStepValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUnitWithCompletion:@
readAttributeUnitWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeUnitWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeUnitWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUnitWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUnitRangeWithCompletion:@
readAttributeUnitRangeWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeUnitRangeWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeUnitRangeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLimitRangeWithCompletion:@
readAttributeLimitRangeWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeLimitRangeWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeLimitRangeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTranslationDirectionWithCompletion:@
readAttributeTranslationDirectionWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeTranslationDirectionWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeTranslationDirectionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRotationAxisWithCompletion:@
readAttributeRotationAxisWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeRotationAxisWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeRotationAxisWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:@
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOverflowWithCompletion:@
readAttributeOverflowWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeOverflowWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeOverflowWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverflowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverflowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeModulationTypeWithCompletion:@
readAttributeModulationTypeWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeModulationTypeWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeModulationTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeLatchControlModesWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeLatchControlModesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterClosureDimension  completion =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRSubscribeParams params) => mtrBaseClusterClosureDimension -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureDimension  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureDimension (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension => mtrBaseClusterClosureDimension -> IO (Id MTRBaseClusterClosureDimension)
init_ mtrBaseClusterClosureDimension  =
    sendMsg mtrBaseClusterClosureDimension (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterClosureDimension)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureDimension"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterClosureDimension mtrBaseClusterClosureDimension, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterClosureDimension -> device -> endpointID -> queue -> IO (Id MTRBaseClusterClosureDimension)
initWithDevice_endpointID_queue mtrBaseClusterClosureDimension  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterClosureDimension (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTargetWithParams:completion:@
setTargetWithParams_completionSelector :: Selector
setTargetWithParams_completionSelector = mkSelector "setTargetWithParams:completion:"

-- | @Selector@ for @setTargetWithCompletion:@
setTargetWithCompletionSelector :: Selector
setTargetWithCompletionSelector = mkSelector "setTargetWithCompletion:"

-- | @Selector@ for @stepWithParams:completion:@
stepWithParams_completionSelector :: Selector
stepWithParams_completionSelector = mkSelector "stepWithParams:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletionSelector :: Selector
readAttributeCurrentStateWithCompletionSelector = mkSelector "readAttributeCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletionSelector :: Selector
readAttributeTargetStateWithCompletionSelector = mkSelector "readAttributeTargetStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeResolutionWithCompletion:@
readAttributeResolutionWithCompletionSelector :: Selector
readAttributeResolutionWithCompletionSelector = mkSelector "readAttributeResolutionWithCompletion:"

-- | @Selector@ for @subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeResolutionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeResolutionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:@
readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeResolutionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeResolutionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStepValueWithCompletion:@
readAttributeStepValueWithCompletionSelector :: Selector
readAttributeStepValueWithCompletionSelector = mkSelector "readAttributeStepValueWithCompletion:"

-- | @Selector@ for @subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStepValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStepValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:@
readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStepValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStepValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUnitWithCompletion:@
readAttributeUnitWithCompletionSelector :: Selector
readAttributeUnitWithCompletionSelector = mkSelector "readAttributeUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUnitRangeWithCompletion:@
readAttributeUnitRangeWithCompletionSelector :: Selector
readAttributeUnitRangeWithCompletionSelector = mkSelector "readAttributeUnitRangeWithCompletion:"

-- | @Selector@ for @subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUnitRangeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUnitRangeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUnitRangeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUnitRangeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLimitRangeWithCompletion:@
readAttributeLimitRangeWithCompletionSelector :: Selector
readAttributeLimitRangeWithCompletionSelector = mkSelector "readAttributeLimitRangeWithCompletion:"

-- | @Selector@ for @subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLimitRangeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLimitRangeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLimitRangeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLimitRangeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTranslationDirectionWithCompletion:@
readAttributeTranslationDirectionWithCompletionSelector :: Selector
readAttributeTranslationDirectionWithCompletionSelector = mkSelector "readAttributeTranslationDirectionWithCompletion:"

-- | @Selector@ for @subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTranslationDirectionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTranslationDirectionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:@
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTranslationDirectionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTranslationDirectionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRotationAxisWithCompletion:@
readAttributeRotationAxisWithCompletionSelector :: Selector
readAttributeRotationAxisWithCompletionSelector = mkSelector "readAttributeRotationAxisWithCompletion:"

-- | @Selector@ for @subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRotationAxisWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRotationAxisWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:@
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRotationAxisWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRotationAxisWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverflowWithCompletion:@
readAttributeOverflowWithCompletionSelector :: Selector
readAttributeOverflowWithCompletionSelector = mkSelector "readAttributeOverflowWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverflowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverflowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOverflowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverflowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeModulationTypeWithCompletion:@
readAttributeModulationTypeWithCompletionSelector :: Selector
readAttributeModulationTypeWithCompletionSelector = mkSelector "readAttributeModulationTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeModulationTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeModulationTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeModulationTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeModulationTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletionSelector :: Selector
readAttributeLatchControlModesWithCompletionSelector = mkSelector "readAttributeLatchControlModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:"

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

