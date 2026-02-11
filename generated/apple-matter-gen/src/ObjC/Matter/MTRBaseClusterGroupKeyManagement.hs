{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Group Key Management
--
-- The Group Key Management Cluster is the mechanism by which group keys are managed.
--
-- Generated bindings for @MTRBaseClusterGroupKeyManagement@.
module ObjC.Matter.MTRBaseClusterGroupKeyManagement
  ( MTRBaseClusterGroupKeyManagement
  , IsMTRBaseClusterGroupKeyManagement(..)
  , keySetWriteWithParams_completion
  , keySetReadWithParams_completion
  , keySetRemoveWithParams_completion
  , keySetReadAllIndicesWithParams_completion
  , keySetReadAllIndicesWithCompletion
  , readAttributeGroupKeyMapWithParams_completion
  , writeAttributeGroupKeyMapWithValue_completion
  , writeAttributeGroupKeyMapWithValue_params_completion
  , subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeGroupTableWithParams_completion
  , subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupTableWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxGroupsPerFabricWithCompletion
  , subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxGroupKeysPerFabricWithCompletion
  , subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completion
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
  , keySetWriteWithParams_completionHandler
  , keySetReadWithParams_completionHandler
  , keySetRemoveWithParams_completionHandler
  , keySetReadAllIndicesWithParams_completionHandler
  , readAttributeGroupKeyMapWithParams_completionHandler
  , writeAttributeGroupKeyMapWithValue_completionHandler
  , writeAttributeGroupKeyMapWithValue_params_completionHandler
  , subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGroupTableWithParams_completionHandler
  , subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxGroupsPerFabricWithCompletionHandler
  , subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeMaxGroupKeysPerFabricWithCompletionHandler
  , subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandler
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
  , keySetWriteWithParams_completionSelector
  , keySetReadWithParams_completionSelector
  , keySetRemoveWithParams_completionSelector
  , keySetReadAllIndicesWithParams_completionSelector
  , keySetReadAllIndicesWithCompletionSelector
  , readAttributeGroupKeyMapWithParams_completionSelector
  , writeAttributeGroupKeyMapWithValue_completionSelector
  , writeAttributeGroupKeyMapWithValue_params_completionSelector
  , subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupTableWithParams_completionSelector
  , subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxGroupsPerFabricWithCompletionSelector
  , subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxGroupKeysPerFabricWithCompletionSelector
  , subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector
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
  , keySetWriteWithParams_completionHandlerSelector
  , keySetReadWithParams_completionHandlerSelector
  , keySetRemoveWithParams_completionHandlerSelector
  , keySetReadAllIndicesWithParams_completionHandlerSelector
  , readAttributeGroupKeyMapWithParams_completionHandlerSelector
  , writeAttributeGroupKeyMapWithValue_completionHandlerSelector
  , writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector
  , subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGroupTableWithParams_completionHandlerSelector
  , subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector
  , subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector
  , subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command KeySetWrite
--
-- Write a new set of keys for the given key set id.
--
-- ObjC selector: @- keySetWriteWithParams:completion:@
keySetWriteWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetWriteWithParams_completion mtrBaseClusterGroupKeyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetWriteWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command KeySetRead
--
-- Read the keys for a given key set id.
--
-- ObjC selector: @- keySetReadWithParams:completion:@
keySetReadWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadWithParams_completion mtrBaseClusterGroupKeyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetReadWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command KeySetRemove
--
-- Revoke a Root Key from a Group
--
-- ObjC selector: @- keySetRemoveWithParams:completion:@
keySetRemoveWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetRemoveWithParams_completion mtrBaseClusterGroupKeyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetRemoveWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command KeySetReadAllIndices
--
-- Return the list of Group Key Sets associated with the accessing fabric
--
-- ObjC selector: @- keySetReadAllIndicesWithParams:completion:@
keySetReadAllIndicesWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_completion mtrBaseClusterGroupKeyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetReadAllIndicesWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- keySetReadAllIndicesWithCompletion:@
keySetReadAllIndicesWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
keySetReadAllIndicesWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetReadAllIndicesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGroupKeyMapWithParams:completion:@
readAttributeGroupKeyMapWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupKeyMapWithParams_completion mtrBaseClusterGroupKeyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeGroupKeyMapWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeGroupKeyMapWithValue:completion:@
writeAttributeGroupKeyMapWithValue_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value) => mtrBaseClusterGroupKeyManagement -> value -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_completion mtrBaseClusterGroupKeyManagement  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "writeAttributeGroupKeyMapWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeGroupKeyMapWithValue:params:completion:@
writeAttributeGroupKeyMapWithValue_params_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterGroupKeyManagement -> value -> params -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_params_completion mtrBaseClusterGroupKeyManagement  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "writeAttributeGroupKeyMapWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGroupTableWithParams:completion:@
readAttributeGroupTableWithParams_completion :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupTableWithParams_completion mtrBaseClusterGroupKeyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeGroupTableWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxGroupsPerFabricWithCompletion:@
readAttributeMaxGroupsPerFabricWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeMaxGroupsPerFabricWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxGroupKeysPerFabricWithCompletion:@
readAttributeMaxGroupKeysPerFabricWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeMaxGroupKeysPerFabricWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGroupKeyManagement  completion =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> IO (Id MTRBaseClusterGroupKeyManagement)
init_ mtrBaseClusterGroupKeyManagement  =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterGroupKeyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterGroupKeyManagement -> device -> CUShort -> queue -> IO (Id MTRBaseClusterGroupKeyManagement)
initWithDevice_endpoint_queue mtrBaseClusterGroupKeyManagement  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- keySetWriteWithParams:completionHandler:@
keySetWriteWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetWriteWithParams_completionHandler mtrBaseClusterGroupKeyManagement  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetWriteWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- keySetReadWithParams:completionHandler:@
keySetReadWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadWithParams_completionHandler mtrBaseClusterGroupKeyManagement  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetReadWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- keySetRemoveWithParams:completionHandler:@
keySetRemoveWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetRemoveWithParams_completionHandler mtrBaseClusterGroupKeyManagement  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetRemoveWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- keySetReadAllIndicesWithParams:completionHandler:@
keySetReadAllIndicesWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_completionHandler mtrBaseClusterGroupKeyManagement  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "keySetReadAllIndicesWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGroupKeyMapWithParams:completionHandler:@
readAttributeGroupKeyMapWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupKeyMapWithParams_completionHandler mtrBaseClusterGroupKeyManagement  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeGroupKeyMapWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeGroupKeyMapWithValue:completionHandler:@
writeAttributeGroupKeyMapWithValue_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value) => mtrBaseClusterGroupKeyManagement -> value -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_completionHandler mtrBaseClusterGroupKeyManagement  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "writeAttributeGroupKeyMapWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeGroupKeyMapWithValue:params:completionHandler:@
writeAttributeGroupKeyMapWithValue_params_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterGroupKeyManagement -> value -> params -> Ptr () -> IO ()
writeAttributeGroupKeyMapWithValue_params_completionHandler mtrBaseClusterGroupKeyManagement  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "writeAttributeGroupKeyMapWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGroupTableWithParams:completionHandler:@
readAttributeGroupTableWithParams_completionHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRReadParams params) => mtrBaseClusterGroupKeyManagement -> params -> Ptr () -> IO ()
readAttributeGroupTableWithParams_completionHandler mtrBaseClusterGroupKeyManagement  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeGroupTableWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxGroupsPerFabricWithCompletionHandler:@
readAttributeMaxGroupsPerFabricWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeMaxGroupsPerFabricWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeMaxGroupKeysPerFabricWithCompletionHandler:@
readAttributeMaxGroupKeysPerFabricWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeMaxGroupKeysPerFabricWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement => mtrBaseClusterGroupKeyManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterGroupKeyManagement  completionHandler =
    sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterGroupKeyManagement -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterGroupKeyManagement  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupKeyManagement"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGroupKeyManagement mtrBaseClusterGroupKeyManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGroupKeyManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGroupKeyManagement)
initWithDevice_endpointID_queue mtrBaseClusterGroupKeyManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterGroupKeyManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keySetWriteWithParams:completion:@
keySetWriteWithParams_completionSelector :: Selector
keySetWriteWithParams_completionSelector = mkSelector "keySetWriteWithParams:completion:"

-- | @Selector@ for @keySetReadWithParams:completion:@
keySetReadWithParams_completionSelector :: Selector
keySetReadWithParams_completionSelector = mkSelector "keySetReadWithParams:completion:"

-- | @Selector@ for @keySetRemoveWithParams:completion:@
keySetRemoveWithParams_completionSelector :: Selector
keySetRemoveWithParams_completionSelector = mkSelector "keySetRemoveWithParams:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:completion:@
keySetReadAllIndicesWithParams_completionSelector :: Selector
keySetReadAllIndicesWithParams_completionSelector = mkSelector "keySetReadAllIndicesWithParams:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithCompletion:@
keySetReadAllIndicesWithCompletionSelector :: Selector
keySetReadAllIndicesWithCompletionSelector = mkSelector "keySetReadAllIndicesWithCompletion:"

-- | @Selector@ for @readAttributeGroupKeyMapWithParams:completion:@
readAttributeGroupKeyMapWithParams_completionSelector :: Selector
readAttributeGroupKeyMapWithParams_completionSelector = mkSelector "readAttributeGroupKeyMapWithParams:completion:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:completion:@
writeAttributeGroupKeyMapWithValue_completionSelector :: Selector
writeAttributeGroupKeyMapWithValue_completionSelector = mkSelector "writeAttributeGroupKeyMapWithValue:completion:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:params:completion:@
writeAttributeGroupKeyMapWithValue_params_completionSelector :: Selector
writeAttributeGroupKeyMapWithValue_params_completionSelector = mkSelector "writeAttributeGroupKeyMapWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGroupKeyMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupKeyMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGroupKeyMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupKeyMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGroupTableWithParams:completion:@
readAttributeGroupTableWithParams_completionSelector :: Selector
readAttributeGroupTableWithParams_completionSelector = mkSelector "readAttributeGroupTableWithParams:completion:"

-- | @Selector@ for @subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGroupTableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupTableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGroupTableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupTableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithCompletion:@
readAttributeMaxGroupsPerFabricWithCompletionSelector :: Selector
readAttributeMaxGroupsPerFabricWithCompletionSelector = mkSelector "readAttributeMaxGroupsPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxGroupsPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupsPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxGroupsPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxGroupsPerFabricWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithCompletion:@
readAttributeMaxGroupKeysPerFabricWithCompletionSelector :: Selector
readAttributeMaxGroupKeysPerFabricWithCompletionSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxGroupKeysPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupKeysPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxGroupKeysPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @keySetWriteWithParams:completionHandler:@
keySetWriteWithParams_completionHandlerSelector :: Selector
keySetWriteWithParams_completionHandlerSelector = mkSelector "keySetWriteWithParams:completionHandler:"

-- | @Selector@ for @keySetReadWithParams:completionHandler:@
keySetReadWithParams_completionHandlerSelector :: Selector
keySetReadWithParams_completionHandlerSelector = mkSelector "keySetReadWithParams:completionHandler:"

-- | @Selector@ for @keySetRemoveWithParams:completionHandler:@
keySetRemoveWithParams_completionHandlerSelector :: Selector
keySetRemoveWithParams_completionHandlerSelector = mkSelector "keySetRemoveWithParams:completionHandler:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:completionHandler:@
keySetReadAllIndicesWithParams_completionHandlerSelector :: Selector
keySetReadAllIndicesWithParams_completionHandlerSelector = mkSelector "keySetReadAllIndicesWithParams:completionHandler:"

-- | @Selector@ for @readAttributeGroupKeyMapWithParams:completionHandler:@
readAttributeGroupKeyMapWithParams_completionHandlerSelector :: Selector
readAttributeGroupKeyMapWithParams_completionHandlerSelector = mkSelector "readAttributeGroupKeyMapWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:completionHandler:@
writeAttributeGroupKeyMapWithValue_completionHandlerSelector :: Selector
writeAttributeGroupKeyMapWithValue_completionHandlerSelector = mkSelector "writeAttributeGroupKeyMapWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:params:completionHandler:@
writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector :: Selector
writeAttributeGroupKeyMapWithValue_params_completionHandlerSelector = mkSelector "writeAttributeGroupKeyMapWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGroupKeyMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupKeyMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeGroupKeyMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGroupKeyMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGroupTableWithParams:completionHandler:@
readAttributeGroupTableWithParams_completionHandlerSelector :: Selector
readAttributeGroupTableWithParams_completionHandlerSelector = mkSelector "readAttributeGroupTableWithParams:completionHandler:"

-- | @Selector@ for @subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGroupTableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupTableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeGroupTableWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGroupTableWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithCompletionHandler:@
readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector :: Selector
readAttributeMaxGroupsPerFabricWithCompletionHandlerSelector = mkSelector "readAttributeMaxGroupsPerFabricWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxGroupsPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupsPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxGroupsPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxGroupsPerFabricWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithCompletionHandler:@
readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector :: Selector
readAttributeMaxGroupKeysPerFabricWithCompletionHandlerSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxGroupKeysPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxGroupKeysPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeMaxGroupKeysPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithAttributeCache:endpoint:queue:completionHandler:"

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

