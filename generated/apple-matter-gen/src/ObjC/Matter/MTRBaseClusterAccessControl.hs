{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Access Control
--
-- The Access Control Cluster exposes a data model view of a      Node's Access Control List (ACL), which codifies the rules used to manage      and enforce Access Control for the Node's endpoints and their associated      cluster instances.
--
-- Generated bindings for @MTRBaseClusterAccessControl@.
module ObjC.Matter.MTRBaseClusterAccessControl
  ( MTRBaseClusterAccessControl
  , IsMTRBaseClusterAccessControl(..)
  , reviewFabricRestrictionsWithParams_completion
  , readAttributeACLWithParams_completion
  , writeAttributeACLWithValue_completion
  , writeAttributeACLWithValue_params_completion
  , subscribeAttributeACLWithParams_subscriptionEstablished_reportHandler
  , readAttributeACLWithClusterStateCache_endpoint_queue_completion
  , readAttributeExtensionWithParams_completion
  , writeAttributeExtensionWithValue_completion
  , writeAttributeExtensionWithValue_params_completion
  , subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandler
  , readAttributeExtensionWithClusterStateCache_endpoint_queue_completion
  , readAttributeSubjectsPerAccessControlEntryWithCompletion
  , subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetsPerAccessControlEntryWithCompletion
  , subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeAccessControlEntriesPerFabricWithCompletion
  , subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandler
  , readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completion
  , readAttributeCommissioningARLWithCompletion
  , subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandler
  , readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completion
  , readAttributeARLWithParams_completion
  , subscribeAttributeARLWithParams_subscriptionEstablished_reportHandler
  , readAttributeARLWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeAclWithParams_completionHandler
  , writeAttributeAclWithValue_completionHandler
  , writeAttributeAclWithValue_params_completionHandler
  , subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAclWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeExtensionWithParams_completionHandler
  , writeAttributeExtensionWithValue_completionHandler
  , writeAttributeExtensionWithValue_params_completionHandler
  , subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSubjectsPerAccessControlEntryWithCompletionHandler
  , subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTargetsPerAccessControlEntryWithCompletionHandler
  , subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAccessControlEntriesPerFabricWithCompletionHandler
  , subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandler
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
  , reviewFabricRestrictionsWithParams_completionSelector
  , readAttributeACLWithParams_completionSelector
  , writeAttributeACLWithValue_completionSelector
  , writeAttributeACLWithValue_params_completionSelector
  , subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeExtensionWithParams_completionSelector
  , writeAttributeExtensionWithValue_completionSelector
  , writeAttributeExtensionWithValue_params_completionSelector
  , subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSubjectsPerAccessControlEntryWithCompletionSelector
  , subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetsPerAccessControlEntryWithCompletionSelector
  , subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAccessControlEntriesPerFabricWithCompletionSelector
  , subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCommissioningARLWithCompletionSelector
  , subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeARLWithParams_completionSelector
  , subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeAclWithParams_completionHandlerSelector
  , writeAttributeAclWithValue_completionHandlerSelector
  , writeAttributeAclWithValue_params_completionHandlerSelector
  , subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeExtensionWithParams_completionHandlerSelector
  , writeAttributeExtensionWithValue_completionHandlerSelector
  , writeAttributeExtensionWithValue_params_completionHandlerSelector
  , subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector
  , subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector
  , subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector
  , subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command ReviewFabricRestrictions
--
-- This command signals to the service associated with the device vendor that the fabric administrator would like a review of the current restrictions on the accessing fabric.
--
-- ObjC selector: @- reviewFabricRestrictionsWithParams:completion:@
reviewFabricRestrictionsWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRAccessControlClusterReviewFabricRestrictionsParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
reviewFabricRestrictionsWithParams_completion mtrBaseClusterAccessControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "reviewFabricRestrictionsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeACLWithParams:completion:@
readAttributeACLWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeACLWithParams_completion mtrBaseClusterAccessControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeACLWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeACLWithValue:completion:@
writeAttributeACLWithValue_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeACLWithValue_completion mtrBaseClusterAccessControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeACLWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeACLWithValue:params:completion:@
writeAttributeACLWithValue_params_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeACLWithValue_params_completion mtrBaseClusterAccessControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeACLWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeACLWithClusterStateCache:endpoint:queue:completion:@
readAttributeACLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeACLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeACLWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeExtensionWithParams:completion:@
readAttributeExtensionWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeExtensionWithParams_completion mtrBaseClusterAccessControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeExtensionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeExtensionWithValue:completion:@
writeAttributeExtensionWithValue_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeExtensionWithValue_completion mtrBaseClusterAccessControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeExtensionWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeExtensionWithValue:params:completion:@
writeAttributeExtensionWithValue_params_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeExtensionWithValue_params_completion mtrBaseClusterAccessControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeExtensionWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:@
readAttributeExtensionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExtensionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSubjectsPerAccessControlEntryWithCompletion:@
readAttributeSubjectsPerAccessControlEntryWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeSubjectsPerAccessControlEntryWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTargetsPerAccessControlEntryWithCompletion:@
readAttributeTargetsPerAccessControlEntryWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeTargetsPerAccessControlEntryWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAccessControlEntriesPerFabricWithCompletion:@
readAttributeAccessControlEntriesPerFabricWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAccessControlEntriesPerFabricWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCommissioningARLWithCompletion:@
readAttributeCommissioningARLWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeCommissioningARLWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeCommissioningARLWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeARLWithParams:completion:@
readAttributeARLWithParams_completion :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeARLWithParams_completion mtrBaseClusterAccessControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeARLWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeARLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeARLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeARLWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterAccessControl  completion =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> IO (Id MTRBaseClusterAccessControl)
init_ mtrBaseClusterAccessControl  =
    sendMsg mtrBaseClusterAccessControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterAccessControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterAccessControl -> device -> CUShort -> queue -> IO (Id MTRBaseClusterAccessControl)
initWithDevice_endpoint_queue mtrBaseClusterAccessControl  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterAccessControl (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeAclWithParams:completionHandler:@
readAttributeAclWithParams_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeAclWithParams_completionHandler mtrBaseClusterAccessControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAclWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeAclWithValue:completionHandler:@
writeAttributeAclWithValue_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeAclWithValue_completionHandler mtrBaseClusterAccessControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeAclWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeAclWithValue:params:completionHandler:@
writeAttributeAclWithValue_params_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeAclWithValue_params_completionHandler mtrBaseClusterAccessControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeAclWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAclWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAclWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeExtensionWithParams:completionHandler:@
readAttributeExtensionWithParams_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRReadParams params) => mtrBaseClusterAccessControl -> params -> Ptr () -> IO ()
readAttributeExtensionWithParams_completionHandler mtrBaseClusterAccessControl  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeExtensionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeExtensionWithValue:completionHandler:@
writeAttributeExtensionWithValue_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value) => mtrBaseClusterAccessControl -> value -> Ptr () -> IO ()
writeAttributeExtensionWithValue_completionHandler mtrBaseClusterAccessControl  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeExtensionWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeExtensionWithValue:params:completionHandler:@
writeAttributeExtensionWithValue_params_completionHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterAccessControl -> value -> params -> Ptr () -> IO ()
writeAttributeExtensionWithValue_params_completionHandler mtrBaseClusterAccessControl  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterAccessControl (mkSelector "writeAttributeExtensionWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:@
readAttributeSubjectsPerAccessControlEntryWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeTargetsPerAccessControlEntryWithCompletionHandler:@
readAttributeTargetsPerAccessControlEntryWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeTargetsPerAccessControlEntryWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAccessControlEntriesPerFabricWithCompletionHandler:@
readAttributeAccessControlEntriesPerFabricWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAccessControlEntriesPerFabricWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl => mtrBaseClusterAccessControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterAccessControl  completionHandler =
    sendMsg mtrBaseClusterAccessControl (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAccessControl -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAccessControl  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAccessControl"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterAccessControl mtrBaseClusterAccessControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterAccessControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterAccessControl)
initWithDevice_endpointID_queue mtrBaseClusterAccessControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterAccessControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reviewFabricRestrictionsWithParams:completion:@
reviewFabricRestrictionsWithParams_completionSelector :: Selector
reviewFabricRestrictionsWithParams_completionSelector = mkSelector "reviewFabricRestrictionsWithParams:completion:"

-- | @Selector@ for @readAttributeACLWithParams:completion:@
readAttributeACLWithParams_completionSelector :: Selector
readAttributeACLWithParams_completionSelector = mkSelector "readAttributeACLWithParams:completion:"

-- | @Selector@ for @writeAttributeACLWithValue:completion:@
writeAttributeACLWithValue_completionSelector :: Selector
writeAttributeACLWithValue_completionSelector = mkSelector "writeAttributeACLWithValue:completion:"

-- | @Selector@ for @writeAttributeACLWithValue:params:completion:@
writeAttributeACLWithValue_params_completionSelector :: Selector
writeAttributeACLWithValue_params_completionSelector = mkSelector "writeAttributeACLWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeACLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeACLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeACLWithClusterStateCache:endpoint:queue:completion:@
readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeACLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeACLWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeExtensionWithParams:completion:@
readAttributeExtensionWithParams_completionSelector :: Selector
readAttributeExtensionWithParams_completionSelector = mkSelector "readAttributeExtensionWithParams:completion:"

-- | @Selector@ for @writeAttributeExtensionWithValue:completion:@
writeAttributeExtensionWithValue_completionSelector :: Selector
writeAttributeExtensionWithValue_completionSelector = mkSelector "writeAttributeExtensionWithValue:completion:"

-- | @Selector@ for @writeAttributeExtensionWithValue:params:completion:@
writeAttributeExtensionWithValue_params_completionSelector :: Selector
writeAttributeExtensionWithValue_params_completionSelector = mkSelector "writeAttributeExtensionWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeExtensionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExtensionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:@
readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeExtensionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeExtensionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithCompletion:@
readAttributeSubjectsPerAccessControlEntryWithCompletionSelector :: Selector
readAttributeSubjectsPerAccessControlEntryWithCompletionSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSubjectsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSubjectsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSubjectsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithCompletion:@
readAttributeTargetsPerAccessControlEntryWithCompletionSelector :: Selector
readAttributeTargetsPerAccessControlEntryWithCompletionSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTargetsPerAccessControlEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetsPerAccessControlEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTargetsPerAccessControlEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithCompletion:@
readAttributeAccessControlEntriesPerFabricWithCompletionSelector :: Selector
readAttributeAccessControlEntriesPerFabricWithCompletionSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithCompletion:"

-- | @Selector@ for @subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAccessControlEntriesPerFabricWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccessControlEntriesPerFabricWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:@
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAccessControlEntriesPerFabricWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCommissioningARLWithCompletion:@
readAttributeCommissioningARLWithCompletionSelector :: Selector
readAttributeCommissioningARLWithCompletionSelector = mkSelector "readAttributeCommissioningARLWithCompletion:"

-- | @Selector@ for @subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCommissioningARLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCommissioningARLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCommissioningARLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCommissioningARLWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeARLWithParams:completion:@
readAttributeARLWithParams_completionSelector :: Selector
readAttributeARLWithParams_completionSelector = mkSelector "readAttributeARLWithParams:completion:"

-- | @Selector@ for @subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeARLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeARLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeARLWithClusterStateCache:endpoint:queue:completion:@
readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeARLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeARLWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeAclWithParams:completionHandler:@
readAttributeAclWithParams_completionHandlerSelector :: Selector
readAttributeAclWithParams_completionHandlerSelector = mkSelector "readAttributeAclWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeAclWithValue:completionHandler:@
writeAttributeAclWithValue_completionHandlerSelector :: Selector
writeAttributeAclWithValue_completionHandlerSelector = mkSelector "writeAttributeAclWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeAclWithValue:params:completionHandler:@
writeAttributeAclWithValue_params_completionHandlerSelector :: Selector
writeAttributeAclWithValue_params_completionHandlerSelector = mkSelector "writeAttributeAclWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAclWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAclWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAclWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAclWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeExtensionWithParams:completionHandler:@
readAttributeExtensionWithParams_completionHandlerSelector :: Selector
readAttributeExtensionWithParams_completionHandlerSelector = mkSelector "readAttributeExtensionWithParams:completionHandler:"

-- | @Selector@ for @writeAttributeExtensionWithValue:completionHandler:@
writeAttributeExtensionWithValue_completionHandlerSelector :: Selector
writeAttributeExtensionWithValue_completionHandlerSelector = mkSelector "writeAttributeExtensionWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeExtensionWithValue:params:completionHandler:@
writeAttributeExtensionWithValue_params_completionHandlerSelector :: Selector
writeAttributeExtensionWithValue_params_completionHandlerSelector = mkSelector "writeAttributeExtensionWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeExtensionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExtensionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeExtensionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeExtensionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:@
readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector :: Selector
readAttributeSubjectsPerAccessControlEntryWithCompletionHandlerSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSubjectsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSubjectsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithCompletionHandler:@
readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector :: Selector
readAttributeTargetsPerAccessControlEntryWithCompletionHandlerSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTargetsPerAccessControlEntryWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetsPerAccessControlEntryWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTargetsPerAccessControlEntryWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithCompletionHandler:@
readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector :: Selector
readAttributeAccessControlEntriesPerFabricWithCompletionHandlerSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAccessControlEntriesPerFabricWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAccessControlEntriesPerFabricWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAccessControlEntriesPerFabricWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithAttributeCache:endpoint:queue:completionHandler:"

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

