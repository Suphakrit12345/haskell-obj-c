{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Datastore
--
-- The Joint Fabric Datastore Cluster is a cluster that provides a mechanism for the Joint Fabric Administrators to manage the set of Nodes, Groups, and Group membership among Nodes in the Joint Fabric.
--
-- Generated bindings for @MTRBaseClusterJointFabricDatastore@.
module ObjC.Matter.MTRBaseClusterJointFabricDatastore
  ( MTRBaseClusterJointFabricDatastore
  , IsMTRBaseClusterJointFabricDatastore(..)
  , addKeySetWithParams_completion
  , updateKeySetWithParams_completion
  , removeKeySetWithParams_completion
  , addGroupWithParams_completion
  , updateGroupWithParams_completion
  , removeGroupWithParams_completion
  , addAdminWithParams_completion
  , updateAdminWithParams_completion
  , removeAdminWithParams_completion
  , addPendingNodeWithParams_completion
  , refreshNodeWithParams_completion
  , updateNodeWithParams_completion
  , removeNodeWithParams_completion
  , updateEndpointForNodeWithParams_completion
  , addGroupIDToEndpointForNodeWithParams_completion
  , removeGroupIDFromEndpointForNodeWithParams_completion
  , addBindingToEndpointForNodeWithParams_completion
  , removeBindingFromEndpointForNodeWithParams_completion
  , addACLToNodeWithParams_completion
  , removeACLFromNodeWithParams_completion
  , readAttributeAnchorRootCAWithCompletion
  , subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandler
  , readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completion
  , readAttributeAnchorNodeIDWithCompletion
  , subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeAnchorVendorIDWithCompletion
  , subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeFriendlyNameWithCompletion
  , subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeGroupKeySetListWithCompletion
  , subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completion
  , readAttributeGroupListWithCompletion
  , subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGroupListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeListWithCompletion
  , subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAdminListWithCompletion
  , subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdminListWithClusterStateCache_endpoint_queue_completion
  , readAttributeStatusWithCompletion
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointGroupIDListWithCompletion
  , subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointBindingListWithCompletion
  , subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeKeySetListWithCompletion
  , subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeACLListWithCompletion
  , subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeEndpointListWithCompletion
  , subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completion
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
  , addKeySetWithParams_completionSelector
  , updateKeySetWithParams_completionSelector
  , removeKeySetWithParams_completionSelector
  , addGroupWithParams_completionSelector
  , updateGroupWithParams_completionSelector
  , removeGroupWithParams_completionSelector
  , addAdminWithParams_completionSelector
  , updateAdminWithParams_completionSelector
  , removeAdminWithParams_completionSelector
  , addPendingNodeWithParams_completionSelector
  , refreshNodeWithParams_completionSelector
  , updateNodeWithParams_completionSelector
  , removeNodeWithParams_completionSelector
  , updateEndpointForNodeWithParams_completionSelector
  , addGroupIDToEndpointForNodeWithParams_completionSelector
  , removeGroupIDFromEndpointForNodeWithParams_completionSelector
  , addBindingToEndpointForNodeWithParams_completionSelector
  , removeBindingFromEndpointForNodeWithParams_completionSelector
  , addACLToNodeWithParams_completionSelector
  , removeACLFromNodeWithParams_completionSelector
  , readAttributeAnchorRootCAWithCompletionSelector
  , subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAnchorNodeIDWithCompletionSelector
  , subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAnchorVendorIDWithCompletionSelector
  , subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFriendlyNameWithCompletionSelector
  , subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupKeySetListWithCompletionSelector
  , subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGroupListWithCompletionSelector
  , subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeListWithCompletionSelector
  , subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdminListWithCompletionSelector
  , subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStatusWithCompletionSelector
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointGroupIDListWithCompletionSelector
  , subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointBindingListWithCompletionSelector
  , subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeKeySetListWithCompletionSelector
  , subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeACLListWithCompletionSelector
  , subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeEndpointListWithCompletionSelector
  , subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command AddKeySet
--
-- This command SHALL be used to add a KeySet to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addKeySetWithParams:completion:@
addKeySetWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddKeySetParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addKeySetWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addKeySetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateKeySet
--
-- This command SHALL be used to update a KeySet in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateKeySetWithParams:completion:@
updateKeySetWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateKeySetParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateKeySetWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "updateKeySetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveKeySet
--
-- This command SHALL be used to remove a KeySet from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeKeySetWithParams:completion:@
removeKeySetWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveKeySetParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeKeySetWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeKeySetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddGroup
--
-- This command SHALL be used to add a group to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addGroupWithParams:completion:@
addGroupWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addGroupWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addGroupWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateGroup
--
-- This command SHALL be used to update a group in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateGroupWithParams:completion:@
updateGroupWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateGroupParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateGroupWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "updateGroupWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveGroup
--
-- This command SHALL be used to remove a group from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeGroupWithParams:completion:@
removeGroupWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeGroupWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeGroupWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddAdmin
--
-- This command SHALL be used to add an admin to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addAdminWithParams:completion:@
addAdminWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddAdminParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addAdminWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addAdminWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateAdmin
--
-- This command SHALL be used to update an admin in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateAdminWithParams:completion:@
updateAdminWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateAdminParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateAdminWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "updateAdminWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveAdmin
--
-- This command SHALL be used to remove an admin from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeAdminWithParams:completion:@
removeAdminWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveAdminParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeAdminWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeAdminWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddPendingNode
--
-- The command SHALL be used to add a node to the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addPendingNodeWithParams:completion:@
addPendingNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddPendingNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addPendingNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addPendingNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RefreshNode
--
-- The command SHALL be used to request that Datastore information relating to a Node of the accessing fabric is refreshed.
--
-- ObjC selector: @- refreshNodeWithParams:completion:@
refreshNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRefreshNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
refreshNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "refreshNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateNode
--
-- The command SHALL be used to update the friendly name for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateNodeWithParams:completion:@
updateNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "updateNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveNode
--
-- This command SHALL be used to remove a node from the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeNodeWithParams:completion:@
removeNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateEndpointForNode
--
-- This command SHALL be used to update the state of an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- updateEndpointForNodeWithParams:completion:@
updateEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
updateEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "updateEndpointForNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddGroupIDToEndpointForNode
--
-- This command SHALL be used to add a Group ID to an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addGroupIDToEndpointForNodeWithParams:completion:@
addGroupIDToEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addGroupIDToEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addGroupIDToEndpointForNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveGroupIDFromEndpointForNode
--
-- This command SHALL be used to remove a Group ID from an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeGroupIDFromEndpointForNodeWithParams:completion:@
removeGroupIDFromEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeGroupIDFromEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeGroupIDFromEndpointForNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddBindingToEndpointForNode
--
-- This command SHALL be used to add a binding to an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addBindingToEndpointForNodeWithParams:completion:@
addBindingToEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addBindingToEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addBindingToEndpointForNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveBindingFromEndpointForNode
--
-- This command SHALL be used to remove a binding from an endpoint for a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeBindingFromEndpointForNodeWithParams:completion:@
removeBindingFromEndpointForNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeBindingFromEndpointForNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeBindingFromEndpointForNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command AddACLToNode
--
-- This command SHALL be used to add an ACL to a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- addACLToNodeWithParams:completion:@
addACLToNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddACLToNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
addACLToNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "addACLToNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveACLFromNode
--
-- This command SHALL be used to remove an ACL from a node in the Joint Fabric Datastore Cluster of the accessing fabric.
--
-- ObjC selector: @- removeACLFromNodeWithParams:completion:@
removeACLFromNodeWithParams_completion :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> IO ()
removeACLFromNodeWithParams_completion mtrBaseClusterJointFabricDatastore  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "removeACLFromNodeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAnchorRootCAWithCompletion:@
readAttributeAnchorRootCAWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAnchorRootCAWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeAnchorRootCAWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAnchorNodeIDWithCompletion:@
readAttributeAnchorNodeIDWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAnchorNodeIDWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeAnchorNodeIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAnchorVendorIDWithCompletion:@
readAttributeAnchorVendorIDWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAnchorVendorIDWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeAnchorVendorIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFriendlyNameWithCompletion:@
readAttributeFriendlyNameWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeFriendlyNameWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeFriendlyNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGroupKeySetListWithCompletion:@
readAttributeGroupKeySetListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeGroupKeySetListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeGroupKeySetListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGroupListWithCompletion:@
readAttributeGroupListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeGroupListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeGroupListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGroupListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNodeListWithCompletion:@
readAttributeNodeListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeNodeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAdminListWithCompletion:@
readAttributeAdminListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAdminListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeAdminListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeStatusWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeStatusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStatusWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEndpointGroupIDListWithCompletion:@
readAttributeEndpointGroupIDListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeEndpointGroupIDListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeEndpointGroupIDListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEndpointBindingListWithCompletion:@
readAttributeEndpointBindingListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeEndpointBindingListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeEndpointBindingListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNodeKeySetListWithCompletion:@
readAttributeNodeKeySetListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeKeySetListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeNodeKeySetListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNodeACLListWithCompletion:@
readAttributeNodeACLListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeACLListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeNodeACLListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNodeEndpointListWithCompletion:@
readAttributeNodeEndpointListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeNodeEndpointListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeNodeEndpointListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterJointFabricDatastore  completion =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricDatastore -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricDatastore  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore => mtrBaseClusterJointFabricDatastore -> IO (Id MTRBaseClusterJointFabricDatastore)
init_ mtrBaseClusterJointFabricDatastore  =
    sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterJointFabricDatastore)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricDatastore"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterJointFabricDatastore mtrBaseClusterJointFabricDatastore, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterJointFabricDatastore -> device -> endpointID -> queue -> IO (Id MTRBaseClusterJointFabricDatastore)
initWithDevice_endpointID_queue mtrBaseClusterJointFabricDatastore  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterJointFabricDatastore (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addKeySetWithParams:completion:@
addKeySetWithParams_completionSelector :: Selector
addKeySetWithParams_completionSelector = mkSelector "addKeySetWithParams:completion:"

-- | @Selector@ for @updateKeySetWithParams:completion:@
updateKeySetWithParams_completionSelector :: Selector
updateKeySetWithParams_completionSelector = mkSelector "updateKeySetWithParams:completion:"

-- | @Selector@ for @removeKeySetWithParams:completion:@
removeKeySetWithParams_completionSelector :: Selector
removeKeySetWithParams_completionSelector = mkSelector "removeKeySetWithParams:completion:"

-- | @Selector@ for @addGroupWithParams:completion:@
addGroupWithParams_completionSelector :: Selector
addGroupWithParams_completionSelector = mkSelector "addGroupWithParams:completion:"

-- | @Selector@ for @updateGroupWithParams:completion:@
updateGroupWithParams_completionSelector :: Selector
updateGroupWithParams_completionSelector = mkSelector "updateGroupWithParams:completion:"

-- | @Selector@ for @removeGroupWithParams:completion:@
removeGroupWithParams_completionSelector :: Selector
removeGroupWithParams_completionSelector = mkSelector "removeGroupWithParams:completion:"

-- | @Selector@ for @addAdminWithParams:completion:@
addAdminWithParams_completionSelector :: Selector
addAdminWithParams_completionSelector = mkSelector "addAdminWithParams:completion:"

-- | @Selector@ for @updateAdminWithParams:completion:@
updateAdminWithParams_completionSelector :: Selector
updateAdminWithParams_completionSelector = mkSelector "updateAdminWithParams:completion:"

-- | @Selector@ for @removeAdminWithParams:completion:@
removeAdminWithParams_completionSelector :: Selector
removeAdminWithParams_completionSelector = mkSelector "removeAdminWithParams:completion:"

-- | @Selector@ for @addPendingNodeWithParams:completion:@
addPendingNodeWithParams_completionSelector :: Selector
addPendingNodeWithParams_completionSelector = mkSelector "addPendingNodeWithParams:completion:"

-- | @Selector@ for @refreshNodeWithParams:completion:@
refreshNodeWithParams_completionSelector :: Selector
refreshNodeWithParams_completionSelector = mkSelector "refreshNodeWithParams:completion:"

-- | @Selector@ for @updateNodeWithParams:completion:@
updateNodeWithParams_completionSelector :: Selector
updateNodeWithParams_completionSelector = mkSelector "updateNodeWithParams:completion:"

-- | @Selector@ for @removeNodeWithParams:completion:@
removeNodeWithParams_completionSelector :: Selector
removeNodeWithParams_completionSelector = mkSelector "removeNodeWithParams:completion:"

-- | @Selector@ for @updateEndpointForNodeWithParams:completion:@
updateEndpointForNodeWithParams_completionSelector :: Selector
updateEndpointForNodeWithParams_completionSelector = mkSelector "updateEndpointForNodeWithParams:completion:"

-- | @Selector@ for @addGroupIDToEndpointForNodeWithParams:completion:@
addGroupIDToEndpointForNodeWithParams_completionSelector :: Selector
addGroupIDToEndpointForNodeWithParams_completionSelector = mkSelector "addGroupIDToEndpointForNodeWithParams:completion:"

-- | @Selector@ for @removeGroupIDFromEndpointForNodeWithParams:completion:@
removeGroupIDFromEndpointForNodeWithParams_completionSelector :: Selector
removeGroupIDFromEndpointForNodeWithParams_completionSelector = mkSelector "removeGroupIDFromEndpointForNodeWithParams:completion:"

-- | @Selector@ for @addBindingToEndpointForNodeWithParams:completion:@
addBindingToEndpointForNodeWithParams_completionSelector :: Selector
addBindingToEndpointForNodeWithParams_completionSelector = mkSelector "addBindingToEndpointForNodeWithParams:completion:"

-- | @Selector@ for @removeBindingFromEndpointForNodeWithParams:completion:@
removeBindingFromEndpointForNodeWithParams_completionSelector :: Selector
removeBindingFromEndpointForNodeWithParams_completionSelector = mkSelector "removeBindingFromEndpointForNodeWithParams:completion:"

-- | @Selector@ for @addACLToNodeWithParams:completion:@
addACLToNodeWithParams_completionSelector :: Selector
addACLToNodeWithParams_completionSelector = mkSelector "addACLToNodeWithParams:completion:"

-- | @Selector@ for @removeACLFromNodeWithParams:completion:@
removeACLFromNodeWithParams_completionSelector :: Selector
removeACLFromNodeWithParams_completionSelector = mkSelector "removeACLFromNodeWithParams:completion:"

-- | @Selector@ for @readAttributeAnchorRootCAWithCompletion:@
readAttributeAnchorRootCAWithCompletionSelector :: Selector
readAttributeAnchorRootCAWithCompletionSelector = mkSelector "readAttributeAnchorRootCAWithCompletion:"

-- | @Selector@ for @subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAnchorRootCAWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAnchorRootCAWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAnchorRootCAWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAnchorRootCAWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAnchorNodeIDWithCompletion:@
readAttributeAnchorNodeIDWithCompletionSelector :: Selector
readAttributeAnchorNodeIDWithCompletionSelector = mkSelector "readAttributeAnchorNodeIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAnchorNodeIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAnchorNodeIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAnchorNodeIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAnchorNodeIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAnchorVendorIDWithCompletion:@
readAttributeAnchorVendorIDWithCompletionSelector :: Selector
readAttributeAnchorVendorIDWithCompletionSelector = mkSelector "readAttributeAnchorVendorIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAnchorVendorIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAnchorVendorIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAnchorVendorIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAnchorVendorIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFriendlyNameWithCompletion:@
readAttributeFriendlyNameWithCompletionSelector :: Selector
readAttributeFriendlyNameWithCompletionSelector = mkSelector "readAttributeFriendlyNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFriendlyNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFriendlyNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFriendlyNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFriendlyNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGroupKeySetListWithCompletion:@
readAttributeGroupKeySetListWithCompletionSelector :: Selector
readAttributeGroupKeySetListWithCompletionSelector = mkSelector "readAttributeGroupKeySetListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGroupKeySetListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupKeySetListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGroupKeySetListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupKeySetListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGroupListWithCompletion:@
readAttributeGroupListWithCompletionSelector :: Selector
readAttributeGroupListWithCompletionSelector = mkSelector "readAttributeGroupListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGroupListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGroupListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGroupListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGroupListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeListWithCompletion:@
readAttributeNodeListWithCompletionSelector :: Selector
readAttributeNodeListWithCompletionSelector = mkSelector "readAttributeNodeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNodeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNodeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAdminListWithCompletion:@
readAttributeAdminListWithCompletionSelector :: Selector
readAttributeAdminListWithCompletionSelector = mkSelector "readAttributeAdminListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAdminListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAdminListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdminListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletionSelector :: Selector
readAttributeStatusWithCompletionSelector = mkSelector "readAttributeStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointGroupIDListWithCompletion:@
readAttributeEndpointGroupIDListWithCompletionSelector :: Selector
readAttributeEndpointGroupIDListWithCompletionSelector = mkSelector "readAttributeEndpointGroupIDListWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEndpointGroupIDListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointGroupIDListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEndpointGroupIDListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointGroupIDListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointBindingListWithCompletion:@
readAttributeEndpointBindingListWithCompletionSelector :: Selector
readAttributeEndpointBindingListWithCompletionSelector = mkSelector "readAttributeEndpointBindingListWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEndpointBindingListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointBindingListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEndpointBindingListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointBindingListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeKeySetListWithCompletion:@
readAttributeNodeKeySetListWithCompletionSelector :: Selector
readAttributeNodeKeySetListWithCompletionSelector = mkSelector "readAttributeNodeKeySetListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNodeKeySetListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeKeySetListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNodeKeySetListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeKeySetListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeACLListWithCompletion:@
readAttributeNodeACLListWithCompletionSelector :: Selector
readAttributeNodeACLListWithCompletionSelector = mkSelector "readAttributeNodeACLListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNodeACLListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeACLListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNodeACLListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeACLListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeEndpointListWithCompletion:@
readAttributeNodeEndpointListWithCompletionSelector :: Selector
readAttributeNodeEndpointListWithCompletionSelector = mkSelector "readAttributeNodeEndpointListWithCompletion:"

-- | @Selector@ for @subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNodeEndpointListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeEndpointListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNodeEndpointListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeEndpointListWithClusterStateCache:endpoint:queue:completion:"

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

