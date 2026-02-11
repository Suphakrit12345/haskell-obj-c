{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , groupID
  , setGroupID
  , statusEntry
  , setStatusEntry
  , nodeIDSelector
  , setNodeIDSelector
  , endpointIDSelector
  , setEndpointIDSelector
  , groupIDSelector
  , setGroupIDSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "statusEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointGroupIDEntryStruct (mkSelector "setStatusEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector
setStatusEntrySelector = mkSelector "setStatusEntry:"

