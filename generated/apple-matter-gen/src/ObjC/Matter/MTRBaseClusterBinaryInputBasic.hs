{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Binary Input (Basic)
--
-- An interface for reading the value of a binary measurement and accessing various characteristics of that measurement.
--
-- Generated bindings for @MTRBaseClusterBinaryInputBasic@.
module ObjC.Matter.MTRBaseClusterBinaryInputBasic
  ( MTRBaseClusterBinaryInputBasic
  , IsMTRBaseClusterBinaryInputBasic(..)
  , readAttributeActiveTextWithCompletion
  , writeAttributeActiveTextWithValue_completion
  , writeAttributeActiveTextWithValue_params_completion
  , subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveTextWithClusterStateCache_endpoint_queue_completion
  , readAttributeDescriptionWithCompletion
  , writeAttributeDescriptionWithValue_completion
  , writeAttributeDescriptionWithValue_params_completion
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion
  , readAttributeInactiveTextWithCompletion
  , writeAttributeInactiveTextWithValue_completion
  , writeAttributeInactiveTextWithValue_params_completion
  , subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandler
  , readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completion
  , readAttributeOutOfServiceWithCompletion
  , writeAttributeOutOfServiceWithValue_completion
  , writeAttributeOutOfServiceWithValue_params_completion
  , subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandler
  , readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completion
  , readAttributePolarityWithCompletion
  , subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandler
  , readAttributePolarityWithClusterStateCache_endpoint_queue_completion
  , readAttributePresentValueWithCompletion
  , writeAttributePresentValueWithValue_completion
  , writeAttributePresentValueWithValue_params_completion
  , subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandler
  , readAttributePresentValueWithClusterStateCache_endpoint_queue_completion
  , readAttributeReliabilityWithCompletion
  , writeAttributeReliabilityWithValue_completion
  , writeAttributeReliabilityWithValue_params_completion
  , subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandler
  , readAttributeReliabilityWithClusterStateCache_endpoint_queue_completion
  , readAttributeStatusFlagsWithCompletion
  , subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandler
  , readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationTypeWithCompletion
  , subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeActiveTextWithCompletionHandler
  , writeAttributeActiveTextWithValue_completionHandler
  , writeAttributeActiveTextWithValue_params_completionHandler
  , subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeDescriptionWithCompletionHandler
  , writeAttributeDescriptionWithValue_completionHandler
  , writeAttributeDescriptionWithValue_params_completionHandler
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeInactiveTextWithCompletionHandler
  , writeAttributeInactiveTextWithValue_completionHandler
  , writeAttributeInactiveTextWithValue_params_completionHandler
  , subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOutOfServiceWithCompletionHandler
  , writeAttributeOutOfServiceWithValue_completionHandler
  , writeAttributeOutOfServiceWithValue_params_completionHandler
  , subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePolarityWithCompletionHandler
  , subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePolarityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePresentValueWithCompletionHandler
  , writeAttributePresentValueWithValue_completionHandler
  , writeAttributePresentValueWithValue_params_completionHandler
  , subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeReliabilityWithCompletionHandler
  , writeAttributeReliabilityWithValue_completionHandler
  , writeAttributeReliabilityWithValue_params_completionHandler
  , subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStatusFlagsWithCompletionHandler
  , subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationTypeWithCompletionHandler
  , subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeActiveTextWithCompletionSelector
  , writeAttributeActiveTextWithValue_completionSelector
  , writeAttributeActiveTextWithValue_params_completionSelector
  , subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDescriptionWithCompletionSelector
  , writeAttributeDescriptionWithValue_completionSelector
  , writeAttributeDescriptionWithValue_params_completionSelector
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInactiveTextWithCompletionSelector
  , writeAttributeInactiveTextWithValue_completionSelector
  , writeAttributeInactiveTextWithValue_params_completionSelector
  , subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOutOfServiceWithCompletionSelector
  , writeAttributeOutOfServiceWithValue_completionSelector
  , writeAttributeOutOfServiceWithValue_params_completionSelector
  , subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePolarityWithCompletionSelector
  , subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePresentValueWithCompletionSelector
  , writeAttributePresentValueWithValue_completionSelector
  , writeAttributePresentValueWithValue_params_completionSelector
  , subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReliabilityWithCompletionSelector
  , writeAttributeReliabilityWithValue_completionSelector
  , writeAttributeReliabilityWithValue_params_completionSelector
  , subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStatusFlagsWithCompletionSelector
  , subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationTypeWithCompletionSelector
  , subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeActiveTextWithCompletionHandlerSelector
  , writeAttributeActiveTextWithValue_completionHandlerSelector
  , writeAttributeActiveTextWithValue_params_completionHandlerSelector
  , subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDescriptionWithCompletionHandlerSelector
  , writeAttributeDescriptionWithValue_completionHandlerSelector
  , writeAttributeDescriptionWithValue_params_completionHandlerSelector
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeInactiveTextWithCompletionHandlerSelector
  , writeAttributeInactiveTextWithValue_completionHandlerSelector
  , writeAttributeInactiveTextWithValue_params_completionHandlerSelector
  , subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOutOfServiceWithCompletionHandlerSelector
  , writeAttributeOutOfServiceWithValue_completionHandlerSelector
  , writeAttributeOutOfServiceWithValue_params_completionHandlerSelector
  , subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePolarityWithCompletionHandlerSelector
  , subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePresentValueWithCompletionHandlerSelector
  , writeAttributePresentValueWithValue_completionHandlerSelector
  , writeAttributePresentValueWithValue_params_completionHandlerSelector
  , subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeReliabilityWithCompletionHandlerSelector
  , writeAttributeReliabilityWithValue_completionHandlerSelector
  , writeAttributeReliabilityWithValue_params_completionHandlerSelector
  , subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStatusFlagsWithCompletionHandlerSelector
  , subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationTypeWithCompletionHandlerSelector
  , subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributeActiveTextWithCompletion:@
readAttributeActiveTextWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeActiveTextWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeActiveTextWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeActiveTextWithValue:completion:@
writeAttributeActiveTextWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_completion mtrBaseClusterBinaryInputBasic  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeActiveTextWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeActiveTextWithValue:params:completion:@
writeAttributeActiveTextWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_params_completion mtrBaseClusterBinaryInputBasic  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeActiveTextWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeDescriptionWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeDescriptionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDescriptionWithValue:completion:@
writeAttributeDescriptionWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_completion mtrBaseClusterBinaryInputBasic  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeDescriptionWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDescriptionWithValue:params:completion:@
writeAttributeDescriptionWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_params_completion mtrBaseClusterBinaryInputBasic  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeDescriptionWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInactiveTextWithCompletion:@
readAttributeInactiveTextWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeInactiveTextWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeInactiveTextWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeInactiveTextWithValue:completion:@
writeAttributeInactiveTextWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_completion mtrBaseClusterBinaryInputBasic  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeInactiveTextWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeInactiveTextWithValue:params:completion:@
writeAttributeInactiveTextWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_params_completion mtrBaseClusterBinaryInputBasic  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeInactiveTextWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOutOfServiceWithCompletion:@
readAttributeOutOfServiceWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeOutOfServiceWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeOutOfServiceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOutOfServiceWithValue:completion:@
writeAttributeOutOfServiceWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_completion mtrBaseClusterBinaryInputBasic  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeOutOfServiceWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOutOfServiceWithValue:params:completion:@
writeAttributeOutOfServiceWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_params_completion mtrBaseClusterBinaryInputBasic  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeOutOfServiceWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:@
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePolarityWithCompletion:@
readAttributePolarityWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePolarityWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributePolarityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePolarityWithClusterStateCache:endpoint:queue:completion:@
readAttributePolarityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePolarityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePolarityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePresentValueWithCompletion:@
readAttributePresentValueWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePresentValueWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributePresentValueWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributePresentValueWithValue:completion:@
writeAttributePresentValueWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributePresentValueWithValue_completion mtrBaseClusterBinaryInputBasic  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributePresentValueWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributePresentValueWithValue:params:completion:@
writeAttributePresentValueWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributePresentValueWithValue_params_completion mtrBaseClusterBinaryInputBasic  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributePresentValueWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePresentValueWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePresentValueWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeReliabilityWithCompletion:@
readAttributeReliabilityWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeReliabilityWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeReliabilityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeReliabilityWithValue:completion:@
writeAttributeReliabilityWithValue_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_completion mtrBaseClusterBinaryInputBasic  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeReliabilityWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeReliabilityWithValue:params:completion:@
writeAttributeReliabilityWithValue_params_completion :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_params_completion mtrBaseClusterBinaryInputBasic  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeReliabilityWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStatusFlagsWithCompletion:@
readAttributeStatusFlagsWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeStatusFlagsWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeStatusFlagsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApplicationTypeWithCompletion:@
readAttributeApplicationTypeWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeApplicationTypeWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeApplicationTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBinaryInputBasic  completion =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> IO (Id MTRBaseClusterBinaryInputBasic)
init_ mtrBaseClusterBinaryInputBasic  =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterBinaryInputBasic)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterBinaryInputBasic -> device -> CUShort -> queue -> IO (Id MTRBaseClusterBinaryInputBasic)
initWithDevice_endpoint_queue mtrBaseClusterBinaryInputBasic  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeActiveTextWithCompletionHandler:@
readAttributeActiveTextWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeActiveTextWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeActiveTextWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeActiveTextWithValue:completionHandler:@
writeAttributeActiveTextWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_completionHandler mtrBaseClusterBinaryInputBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeActiveTextWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeActiveTextWithValue:params:completionHandler:@
writeAttributeActiveTextWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeActiveTextWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeActiveTextWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeDescriptionWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeDescriptionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeDescriptionWithValue:completionHandler:@
writeAttributeDescriptionWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_completionHandler mtrBaseClusterBinaryInputBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeDescriptionWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeDescriptionWithValue:params:completionHandler:@
writeAttributeDescriptionWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeDescriptionWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeDescriptionWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeInactiveTextWithCompletionHandler:@
readAttributeInactiveTextWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeInactiveTextWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeInactiveTextWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeInactiveTextWithValue:completionHandler:@
writeAttributeInactiveTextWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_completionHandler mtrBaseClusterBinaryInputBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeInactiveTextWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeInactiveTextWithValue:params:completionHandler:@
writeAttributeInactiveTextWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeInactiveTextWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeInactiveTextWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOutOfServiceWithCompletionHandler:@
readAttributeOutOfServiceWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeOutOfServiceWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeOutOfServiceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOutOfServiceWithValue:completionHandler:@
writeAttributeOutOfServiceWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_completionHandler mtrBaseClusterBinaryInputBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeOutOfServiceWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOutOfServiceWithValue:params:completionHandler:@
writeAttributeOutOfServiceWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeOutOfServiceWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeOutOfServiceWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePolarityWithCompletionHandler:@
readAttributePolarityWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePolarityWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributePolarityWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePresentValueWithCompletionHandler:@
readAttributePresentValueWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributePresentValueWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributePresentValueWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributePresentValueWithValue:completionHandler:@
writeAttributePresentValueWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributePresentValueWithValue_completionHandler mtrBaseClusterBinaryInputBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributePresentValueWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributePresentValueWithValue:params:completionHandler:@
writeAttributePresentValueWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributePresentValueWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributePresentValueWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeReliabilityWithCompletionHandler:@
readAttributeReliabilityWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeReliabilityWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeReliabilityWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeReliabilityWithValue:completionHandler:@
writeAttributeReliabilityWithValue_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value) => mtrBaseClusterBinaryInputBasic -> value -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_completionHandler mtrBaseClusterBinaryInputBasic  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeReliabilityWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeReliabilityWithValue:params:completionHandler:@
writeAttributeReliabilityWithValue_params_completionHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBinaryInputBasic -> value -> params -> Ptr () -> IO ()
writeAttributeReliabilityWithValue_params_completionHandler mtrBaseClusterBinaryInputBasic  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "writeAttributeReliabilityWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStatusFlagsWithCompletionHandler:@
readAttributeStatusFlagsWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeStatusFlagsWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeStatusFlagsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeApplicationTypeWithCompletionHandler:@
readAttributeApplicationTypeWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeApplicationTypeWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeApplicationTypeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic => mtrBaseClusterBinaryInputBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterBinaryInputBasic  completionHandler =
    sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBinaryInputBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBinaryInputBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBinaryInputBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBinaryInputBasic mtrBaseClusterBinaryInputBasic, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBinaryInputBasic -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBinaryInputBasic)
initWithDevice_endpointID_queue mtrBaseClusterBinaryInputBasic  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterBinaryInputBasic (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeActiveTextWithCompletion:@
readAttributeActiveTextWithCompletionSelector :: Selector
readAttributeActiveTextWithCompletionSelector = mkSelector "readAttributeActiveTextWithCompletion:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:completion:@
writeAttributeActiveTextWithValue_completionSelector :: Selector
writeAttributeActiveTextWithValue_completionSelector = mkSelector "writeAttributeActiveTextWithValue:completion:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:params:completion:@
writeAttributeActiveTextWithValue_params_completionSelector :: Selector
writeAttributeActiveTextWithValue_params_completionSelector = mkSelector "writeAttributeActiveTextWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveTextWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveTextWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveTextWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveTextWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletionSelector :: Selector
readAttributeDescriptionWithCompletionSelector = mkSelector "readAttributeDescriptionWithCompletion:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:completion:@
writeAttributeDescriptionWithValue_completionSelector :: Selector
writeAttributeDescriptionWithValue_completionSelector = mkSelector "writeAttributeDescriptionWithValue:completion:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:params:completion:@
writeAttributeDescriptionWithValue_params_completionSelector :: Selector
writeAttributeDescriptionWithValue_params_completionSelector = mkSelector "writeAttributeDescriptionWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInactiveTextWithCompletion:@
readAttributeInactiveTextWithCompletionSelector :: Selector
readAttributeInactiveTextWithCompletionSelector = mkSelector "readAttributeInactiveTextWithCompletion:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:completion:@
writeAttributeInactiveTextWithValue_completionSelector :: Selector
writeAttributeInactiveTextWithValue_completionSelector = mkSelector "writeAttributeInactiveTextWithValue:completion:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:params:completion:@
writeAttributeInactiveTextWithValue_params_completionSelector :: Selector
writeAttributeInactiveTextWithValue_params_completionSelector = mkSelector "writeAttributeInactiveTextWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInactiveTextWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInactiveTextWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:@
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInactiveTextWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInactiveTextWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOutOfServiceWithCompletion:@
readAttributeOutOfServiceWithCompletionSelector :: Selector
readAttributeOutOfServiceWithCompletionSelector = mkSelector "readAttributeOutOfServiceWithCompletion:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:completion:@
writeAttributeOutOfServiceWithValue_completionSelector :: Selector
writeAttributeOutOfServiceWithValue_completionSelector = mkSelector "writeAttributeOutOfServiceWithValue:completion:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:params:completion:@
writeAttributeOutOfServiceWithValue_params_completionSelector :: Selector
writeAttributeOutOfServiceWithValue_params_completionSelector = mkSelector "writeAttributeOutOfServiceWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOutOfServiceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOutOfServiceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:@
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOutOfServiceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOutOfServiceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePolarityWithCompletion:@
readAttributePolarityWithCompletionSelector :: Selector
readAttributePolarityWithCompletionSelector = mkSelector "readAttributePolarityWithCompletion:"

-- | @Selector@ for @subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePolarityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePolarityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePolarityWithClusterStateCache:endpoint:queue:completion:@
readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePolarityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePolarityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePresentValueWithCompletion:@
readAttributePresentValueWithCompletionSelector :: Selector
readAttributePresentValueWithCompletionSelector = mkSelector "readAttributePresentValueWithCompletion:"

-- | @Selector@ for @writeAttributePresentValueWithValue:completion:@
writeAttributePresentValueWithValue_completionSelector :: Selector
writeAttributePresentValueWithValue_completionSelector = mkSelector "writeAttributePresentValueWithValue:completion:"

-- | @Selector@ for @writeAttributePresentValueWithValue:params:completion:@
writeAttributePresentValueWithValue_params_completionSelector :: Selector
writeAttributePresentValueWithValue_params_completionSelector = mkSelector "writeAttributePresentValueWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePresentValueWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePresentValueWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:@
readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePresentValueWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePresentValueWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReliabilityWithCompletion:@
readAttributeReliabilityWithCompletionSelector :: Selector
readAttributeReliabilityWithCompletionSelector = mkSelector "readAttributeReliabilityWithCompletion:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:completion:@
writeAttributeReliabilityWithValue_completionSelector :: Selector
writeAttributeReliabilityWithValue_completionSelector = mkSelector "writeAttributeReliabilityWithValue:completion:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:params:completion:@
writeAttributeReliabilityWithValue_params_completionSelector :: Selector
writeAttributeReliabilityWithValue_params_completionSelector = mkSelector "writeAttributeReliabilityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReliabilityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReliabilityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeReliabilityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReliabilityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStatusFlagsWithCompletion:@
readAttributeStatusFlagsWithCompletionSelector :: Selector
readAttributeStatusFlagsWithCompletionSelector = mkSelector "readAttributeStatusFlagsWithCompletion:"

-- | @Selector@ for @subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStatusFlagsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusFlagsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStatusFlagsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStatusFlagsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationTypeWithCompletion:@
readAttributeApplicationTypeWithCompletionSelector :: Selector
readAttributeApplicationTypeWithCompletionSelector = mkSelector "readAttributeApplicationTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApplicationTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationTypeWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeActiveTextWithCompletionHandler:@
readAttributeActiveTextWithCompletionHandlerSelector :: Selector
readAttributeActiveTextWithCompletionHandlerSelector = mkSelector "readAttributeActiveTextWithCompletionHandler:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:completionHandler:@
writeAttributeActiveTextWithValue_completionHandlerSelector :: Selector
writeAttributeActiveTextWithValue_completionHandlerSelector = mkSelector "writeAttributeActiveTextWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:params:completionHandler:@
writeAttributeActiveTextWithValue_params_completionHandlerSelector :: Selector
writeAttributeActiveTextWithValue_params_completionHandlerSelector = mkSelector "writeAttributeActiveTextWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeActiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveTextWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandlerSelector :: Selector
readAttributeDescriptionWithCompletionHandlerSelector = mkSelector "readAttributeDescriptionWithCompletionHandler:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:completionHandler:@
writeAttributeDescriptionWithValue_completionHandlerSelector :: Selector
writeAttributeDescriptionWithValue_completionHandlerSelector = mkSelector "writeAttributeDescriptionWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:params:completionHandler:@
writeAttributeDescriptionWithValue_params_completionHandlerSelector :: Selector
writeAttributeDescriptionWithValue_params_completionHandlerSelector = mkSelector "writeAttributeDescriptionWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeInactiveTextWithCompletionHandler:@
readAttributeInactiveTextWithCompletionHandlerSelector :: Selector
readAttributeInactiveTextWithCompletionHandlerSelector = mkSelector "readAttributeInactiveTextWithCompletionHandler:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:completionHandler:@
writeAttributeInactiveTextWithValue_completionHandlerSelector :: Selector
writeAttributeInactiveTextWithValue_completionHandlerSelector = mkSelector "writeAttributeInactiveTextWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:params:completionHandler:@
writeAttributeInactiveTextWithValue_params_completionHandlerSelector :: Selector
writeAttributeInactiveTextWithValue_params_completionHandlerSelector = mkSelector "writeAttributeInactiveTextWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInactiveTextWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInactiveTextWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeInactiveTextWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeInactiveTextWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOutOfServiceWithCompletionHandler:@
readAttributeOutOfServiceWithCompletionHandlerSelector :: Selector
readAttributeOutOfServiceWithCompletionHandlerSelector = mkSelector "readAttributeOutOfServiceWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:completionHandler:@
writeAttributeOutOfServiceWithValue_completionHandlerSelector :: Selector
writeAttributeOutOfServiceWithValue_completionHandlerSelector = mkSelector "writeAttributeOutOfServiceWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:params:completionHandler:@
writeAttributeOutOfServiceWithValue_params_completionHandlerSelector :: Selector
writeAttributeOutOfServiceWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOutOfServiceWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOutOfServiceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOutOfServiceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOutOfServiceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOutOfServiceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePolarityWithCompletionHandler:@
readAttributePolarityWithCompletionHandlerSelector :: Selector
readAttributePolarityWithCompletionHandlerSelector = mkSelector "readAttributePolarityWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePolarityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePolarityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePolarityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePolarityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePresentValueWithCompletionHandler:@
readAttributePresentValueWithCompletionHandlerSelector :: Selector
readAttributePresentValueWithCompletionHandlerSelector = mkSelector "readAttributePresentValueWithCompletionHandler:"

-- | @Selector@ for @writeAttributePresentValueWithValue:completionHandler:@
writeAttributePresentValueWithValue_completionHandlerSelector :: Selector
writeAttributePresentValueWithValue_completionHandlerSelector = mkSelector "writeAttributePresentValueWithValue:completionHandler:"

-- | @Selector@ for @writeAttributePresentValueWithValue:params:completionHandler:@
writeAttributePresentValueWithValue_params_completionHandlerSelector :: Selector
writeAttributePresentValueWithValue_params_completionHandlerSelector = mkSelector "writeAttributePresentValueWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePresentValueWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePresentValueWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePresentValueWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePresentValueWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeReliabilityWithCompletionHandler:@
readAttributeReliabilityWithCompletionHandlerSelector :: Selector
readAttributeReliabilityWithCompletionHandlerSelector = mkSelector "readAttributeReliabilityWithCompletionHandler:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:completionHandler:@
writeAttributeReliabilityWithValue_completionHandlerSelector :: Selector
writeAttributeReliabilityWithValue_completionHandlerSelector = mkSelector "writeAttributeReliabilityWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:params:completionHandler:@
writeAttributeReliabilityWithValue_params_completionHandlerSelector :: Selector
writeAttributeReliabilityWithValue_params_completionHandlerSelector = mkSelector "writeAttributeReliabilityWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeReliabilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReliabilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeReliabilityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeReliabilityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStatusFlagsWithCompletionHandler:@
readAttributeStatusFlagsWithCompletionHandlerSelector :: Selector
readAttributeStatusFlagsWithCompletionHandlerSelector = mkSelector "readAttributeStatusFlagsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStatusFlagsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusFlagsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStatusFlagsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStatusFlagsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationTypeWithCompletionHandler:@
readAttributeApplicationTypeWithCompletionHandlerSelector :: Selector
readAttributeApplicationTypeWithCompletionHandlerSelector = mkSelector "readAttributeApplicationTypeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeApplicationTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationTypeWithAttributeCache:endpoint:queue:completionHandler:"

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

