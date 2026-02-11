{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , commissioningStatusEntry
  , setCommissioningStatusEntry
  , nodeIDSelector
  , setNodeIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
  , commissioningStatusEntrySelector
  , setCommissioningStatusEntrySelector


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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- commissioningStatusEntry@
commissioningStatusEntry :: IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> IO (Id MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct)
commissioningStatusEntry mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct (mkSelector "commissioningStatusEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCommissioningStatusEntry:@
setCommissioningStatusEntry :: (IsMTRJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct, IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct value) => mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct -> value -> IO ()
setCommissioningStatusEntry mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreNodeInformationEntryStruct (mkSelector "setCommissioningStatusEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @commissioningStatusEntry@
commissioningStatusEntrySelector :: Selector
commissioningStatusEntrySelector = mkSelector "commissioningStatusEntry"

-- | @Selector@ for @setCommissioningStatusEntry:@
setCommissioningStatusEntrySelector :: Selector
setCommissioningStatusEntrySelector = mkSelector "setCommissioningStatusEntry:"

