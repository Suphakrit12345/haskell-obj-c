{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Datastore    The Joint Fabric Datastore Cluster is a cluster that provides a mechanism for the Joint Fabric Administrators to manage the set of Nodes, Groups, and Group membership among Nodes in the Joint Fabric.
--
-- Generated bindings for @MTRClusterJointFabricDatastore@.
module ObjC.Matter.MTRClusterJointFabricDatastore
  ( MTRClusterJointFabricDatastore
  , IsMTRClusterJointFabricDatastore(..)
  , addKeySetWithParams_expectedValues_expectedValueInterval_completion
  , updateKeySetWithParams_expectedValues_expectedValueInterval_completion
  , removeKeySetWithParams_expectedValues_expectedValueInterval_completion
  , addGroupWithParams_expectedValues_expectedValueInterval_completion
  , updateGroupWithParams_expectedValues_expectedValueInterval_completion
  , removeGroupWithParams_expectedValues_expectedValueInterval_completion
  , addAdminWithParams_expectedValues_expectedValueInterval_completion
  , updateAdminWithParams_expectedValues_expectedValueInterval_completion
  , removeAdminWithParams_expectedValues_expectedValueInterval_completion
  , addPendingNodeWithParams_expectedValues_expectedValueInterval_completion
  , refreshNodeWithParams_expectedValues_expectedValueInterval_completion
  , updateNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeNodeWithParams_expectedValues_expectedValueInterval_completion
  , updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion
  , addACLToNodeWithParams_expectedValues_expectedValueInterval_completion
  , removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeAnchorRootCAWithParams
  , readAttributeAnchorNodeIDWithParams
  , readAttributeAnchorVendorIDWithParams
  , readAttributeFriendlyNameWithParams
  , readAttributeGroupKeySetListWithParams
  , readAttributeGroupListWithParams
  , readAttributeNodeListWithParams
  , readAttributeAdminListWithParams
  , readAttributeStatusWithParams
  , readAttributeEndpointGroupIDListWithParams
  , readAttributeEndpointBindingListWithParams
  , readAttributeNodeKeySetListWithParams
  , readAttributeNodeACLListWithParams
  , readAttributeNodeEndpointListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector
  , addGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector
  , addAdminWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector
  , addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAnchorRootCAWithParamsSelector
  , readAttributeAnchorNodeIDWithParamsSelector
  , readAttributeAnchorVendorIDWithParamsSelector
  , readAttributeFriendlyNameWithParamsSelector
  , readAttributeGroupKeySetListWithParamsSelector
  , readAttributeGroupListWithParamsSelector
  , readAttributeNodeListWithParamsSelector
  , readAttributeAdminListWithParamsSelector
  , readAttributeStatusWithParamsSelector
  , readAttributeEndpointGroupIDListWithParamsSelector
  , readAttributeEndpointBindingListWithParamsSelector
  , readAttributeNodeKeySetListWithParamsSelector
  , readAttributeNodeACLListWithParamsSelector
  , readAttributeNodeEndpointListWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
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

-- | @- addKeySetWithParams:expectedValues:expectedValueInterval:completion:@
addKeySetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddKeySetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addKeySetWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addKeySetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateKeySetWithParams:expectedValues:expectedValueInterval:completion:@
updateKeySetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateKeySetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateKeySetWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "updateKeySetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeKeySetWithParams:expectedValues:expectedValueInterval:completion:@
removeKeySetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveKeySetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeKeySetWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeKeySetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addGroupWithParams:expectedValues:expectedValueInterval:completion:@
addGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateGroupWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "updateGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeGroupWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeGroupWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addAdminWithParams:expectedValues:expectedValueInterval:completion:@
addAdminWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddAdminParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addAdminWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addAdminWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateAdminWithParams:expectedValues:expectedValueInterval:completion:@
updateAdminWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateAdminParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateAdminWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "updateAdminWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeAdminWithParams:expectedValues:expectedValueInterval:completion:@
removeAdminWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveAdminParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAdminWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeAdminWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:@
addPendingNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddPendingNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addPendingNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- refreshNodeWithParams:expectedValues:expectedValueInterval:completion:@
refreshNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRefreshNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
refreshNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "refreshNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "updateNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveBindingFromEndpointForNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:@
addACLToNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterAddACLToNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addACLToNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricDatastore -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricDatastore  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAnchorRootCAWithParams:@
readAttributeAnchorRootCAWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAnchorRootCAWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeAnchorRootCAWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAnchorNodeIDWithParams:@
readAttributeAnchorNodeIDWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAnchorNodeIDWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeAnchorNodeIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAnchorVendorIDWithParams:@
readAttributeAnchorVendorIDWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAnchorVendorIDWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeAnchorVendorIDWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFriendlyNameWithParams:@
readAttributeFriendlyNameWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeFriendlyNameWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeFriendlyNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGroupKeySetListWithParams:@
readAttributeGroupKeySetListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeGroupKeySetListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeGroupKeySetListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGroupListWithParams:@
readAttributeGroupListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeGroupListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeGroupListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNodeListWithParams:@
readAttributeNodeListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeNodeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAdminListWithParams:@
readAttributeAdminListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAdminListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeAdminListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStatusWithParams:@
readAttributeStatusWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeStatusWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeStatusWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndpointGroupIDListWithParams:@
readAttributeEndpointGroupIDListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeEndpointGroupIDListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeEndpointGroupIDListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndpointBindingListWithParams:@
readAttributeEndpointBindingListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeEndpointBindingListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeEndpointBindingListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNodeKeySetListWithParams:@
readAttributeNodeKeySetListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeKeySetListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeNodeKeySetListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNodeACLListWithParams:@
readAttributeNodeACLListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeACLListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeNodeACLListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNodeEndpointListWithParams:@
readAttributeNodeEndpointListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeNodeEndpointListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeNodeEndpointListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRReadParams params) => mtrClusterJointFabricDatastore -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterJointFabricDatastore  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterJointFabricDatastore (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore => mtrClusterJointFabricDatastore -> IO (Id MTRClusterJointFabricDatastore)
init_ mtrClusterJointFabricDatastore  =
    sendMsg mtrClusterJointFabricDatastore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterJointFabricDatastore)
new  =
  do
    cls' <- getRequiredClass "MTRClusterJointFabricDatastore"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterJointFabricDatastore mtrClusterJointFabricDatastore, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterJointFabricDatastore -> device -> endpointID -> queue -> IO (Id MTRClusterJointFabricDatastore)
initWithDevice_endpointID_queue mtrClusterJointFabricDatastore  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterJointFabricDatastore (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addKeySetWithParams:expectedValues:expectedValueInterval:completion:@
addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addKeySetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addKeySetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateKeySetWithParams:expectedValues:expectedValueInterval:completion:@
updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateKeySetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateKeySetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeKeySetWithParams:expectedValues:expectedValueInterval:completion:@
removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeKeySetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeKeySetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addGroupWithParams:expectedValues:expectedValueInterval:completion:@
addGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateGroupWithParams:expectedValues:expectedValueInterval:completion:@
updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeGroupWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeGroupWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeGroupWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addAdminWithParams:expectedValues:expectedValueInterval:completion:@
addAdminWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addAdminWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addAdminWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateAdminWithParams:expectedValues:expectedValueInterval:completion:@
updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateAdminWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateAdminWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeAdminWithParams:expectedValues:expectedValueInterval:completion:@
removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeAdminWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeAdminWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:@
addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addPendingNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addPendingNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @refreshNodeWithParams:expectedValues:expectedValueInterval:completion:@
refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
refreshNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "refreshNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
updateEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addGroupIDToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addGroupIDToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeGroupIDFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeGroupIDFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addBindingToEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addBindingToEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeBindingFromEndpointForNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeBindingFromEndpointForNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:@
addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
addACLToNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addACLToNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:@
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
removeACLFromNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeACLFromNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeAnchorRootCAWithParams:@
readAttributeAnchorRootCAWithParamsSelector :: Selector
readAttributeAnchorRootCAWithParamsSelector = mkSelector "readAttributeAnchorRootCAWithParams:"

-- | @Selector@ for @readAttributeAnchorNodeIDWithParams:@
readAttributeAnchorNodeIDWithParamsSelector :: Selector
readAttributeAnchorNodeIDWithParamsSelector = mkSelector "readAttributeAnchorNodeIDWithParams:"

-- | @Selector@ for @readAttributeAnchorVendorIDWithParams:@
readAttributeAnchorVendorIDWithParamsSelector :: Selector
readAttributeAnchorVendorIDWithParamsSelector = mkSelector "readAttributeAnchorVendorIDWithParams:"

-- | @Selector@ for @readAttributeFriendlyNameWithParams:@
readAttributeFriendlyNameWithParamsSelector :: Selector
readAttributeFriendlyNameWithParamsSelector = mkSelector "readAttributeFriendlyNameWithParams:"

-- | @Selector@ for @readAttributeGroupKeySetListWithParams:@
readAttributeGroupKeySetListWithParamsSelector :: Selector
readAttributeGroupKeySetListWithParamsSelector = mkSelector "readAttributeGroupKeySetListWithParams:"

-- | @Selector@ for @readAttributeGroupListWithParams:@
readAttributeGroupListWithParamsSelector :: Selector
readAttributeGroupListWithParamsSelector = mkSelector "readAttributeGroupListWithParams:"

-- | @Selector@ for @readAttributeNodeListWithParams:@
readAttributeNodeListWithParamsSelector :: Selector
readAttributeNodeListWithParamsSelector = mkSelector "readAttributeNodeListWithParams:"

-- | @Selector@ for @readAttributeAdminListWithParams:@
readAttributeAdminListWithParamsSelector :: Selector
readAttributeAdminListWithParamsSelector = mkSelector "readAttributeAdminListWithParams:"

-- | @Selector@ for @readAttributeStatusWithParams:@
readAttributeStatusWithParamsSelector :: Selector
readAttributeStatusWithParamsSelector = mkSelector "readAttributeStatusWithParams:"

-- | @Selector@ for @readAttributeEndpointGroupIDListWithParams:@
readAttributeEndpointGroupIDListWithParamsSelector :: Selector
readAttributeEndpointGroupIDListWithParamsSelector = mkSelector "readAttributeEndpointGroupIDListWithParams:"

-- | @Selector@ for @readAttributeEndpointBindingListWithParams:@
readAttributeEndpointBindingListWithParamsSelector :: Selector
readAttributeEndpointBindingListWithParamsSelector = mkSelector "readAttributeEndpointBindingListWithParams:"

-- | @Selector@ for @readAttributeNodeKeySetListWithParams:@
readAttributeNodeKeySetListWithParamsSelector :: Selector
readAttributeNodeKeySetListWithParamsSelector = mkSelector "readAttributeNodeKeySetListWithParams:"

-- | @Selector@ for @readAttributeNodeACLListWithParams:@
readAttributeNodeACLListWithParamsSelector :: Selector
readAttributeNodeACLListWithParamsSelector = mkSelector "readAttributeNodeACLListWithParams:"

-- | @Selector@ for @readAttributeNodeEndpointListWithParams:@
readAttributeNodeEndpointListWithParamsSelector :: Selector
readAttributeNodeEndpointListWithParamsSelector = mkSelector "readAttributeNodeEndpointListWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

