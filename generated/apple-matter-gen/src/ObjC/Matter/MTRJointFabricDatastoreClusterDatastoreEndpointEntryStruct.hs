{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct(..)
  , endpointID
  , setEndpointID
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , statusEntry
  , setStatusEntry
  , endpointIDSelector
  , setEndpointIDSelector
  , nodeIDSelector
  , setNodeIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
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

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "setEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusEntry@
statusEntry :: IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
statusEntry mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "statusEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusEntry:@
setStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreEndpointEntryStruct mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct -> value -> IO ()
setStatusEntry mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreEndpointEntryStruct (mkSelector "setStatusEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector
setFriendlyNameSelector = mkSelector "setFriendlyName:"

-- | @Selector@ for @statusEntry@
statusEntrySelector :: Selector
statusEntrySelector = mkSelector "statusEntry"

-- | @Selector@ for @setStatusEntry:@
setStatusEntrySelector :: Selector
setStatusEntrySelector = mkSelector "setStatusEntry:"

