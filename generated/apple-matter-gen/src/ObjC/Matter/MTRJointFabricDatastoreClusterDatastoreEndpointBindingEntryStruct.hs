{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , listID
  , setListID
  , binding
  , setBinding
  , statusEntry
  , setStatusEntry
  , nodeIDSelector
  , setNodeIDSelector
  , endpointIDSelector
  , setEndpointIDSelector
  , listIDSelector
  , setListIDSelector
  , bindingSelector
  , setBindingSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- listID@
listID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "listID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setListID mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "setListID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- binding@
binding :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct)
binding mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "binding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBinding:@
setBinding :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setBinding mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "setBinding:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "statusEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointBindingEntryStruct (mkSelector "setStatusEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @listID@
listIDSelector :: Selector
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector
setListIDSelector = mkSelector "setListID:"

-- | @Selector@ for @binding@
bindingSelector :: Selector
bindingSelector = mkSelector "binding"

-- | @Selector@ for @setBinding:@
setBindingSelector :: Selector
setBindingSelector = mkSelector "setBinding:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector
setStatusEntrySelector = mkSelector "setStatusEntry:"

