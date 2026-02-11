{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct(..)
  , nodeID
  , setNodeID
  , groupKeySetID
  , setGroupKeySetID
  , statusEntry
  , setStatusEntry
  , nodeIDSelector
  , setNodeIDSelector
  , groupKeySetIDSelector
  , setGroupKeySetIDSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct (mkSelector "groupKeySetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct (mkSelector "setGroupKeySetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct (mkSelector "statusEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreNodeKeySetEntryStruct (mkSelector "setStatusEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector
setStatusEntrySelector = mkSelector "setStatusEntry:"

