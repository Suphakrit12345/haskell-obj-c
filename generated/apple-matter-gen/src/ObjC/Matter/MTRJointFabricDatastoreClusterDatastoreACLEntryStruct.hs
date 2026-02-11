{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreACLEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreACLEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreACLEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct(..)
  , nodeID
  , setNodeID
  , listID
  , setListID
  , aclEntry
  , setAclEntry
  , statusEntry
  , setStatusEntry
  , nodeIDSelector
  , setNodeIDSelector
  , listIDSelector
  , setListIDSelector
  , aclEntrySelector
  , setAclEntrySelector
  , statusEntrySelector
  , setStatusEntrySelector


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

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- listID@
listID :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "listID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setListID mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "setListID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- aclEntry@
aclEntry :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct)
aclEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "aclEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAclEntry:@
setAclEntry :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setAclEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "setAclEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "statusEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreACLEntryStruct mtrJointFabricDatastoreClusterDatastoreACLEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreACLEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreACLEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreACLEntryStruct (mkSelector "setStatusEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @listID@
listIDSelector :: Selector
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector
setListIDSelector = mkSelector "setListID:"

-- | @Selector@ for @aclEntry@
aclEntrySelector :: Selector
aclEntrySelector = mkSelector "aclEntry"

-- | @Selector@ for @setAclEntry:@
setAclEntrySelector :: Selector
setAclEntrySelector = mkSelector "setAclEntry:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector
setStatusEntrySelector = mkSelector "setStatusEntry:"

