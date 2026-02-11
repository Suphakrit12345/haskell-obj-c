{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , vendorID
  , setVendorID
  , icac
  , setIcac
  , nodeIDSelector
  , setNodeIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
  , vendorIDSelector
  , setVendorIDSelector
  , icacSelector
  , setIcacSelector


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
nodeID :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "nodeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "setNodeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vendorID@
vendorID :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSNumber)
vendorID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVendorID:@
setVendorID :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setVendorID mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "setVendorID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- icac@
icac :: IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> IO (Id NSData)
icac mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "icac") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcac:@
setIcac :: (IsMTRJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct -> value -> IO ()
setIcac mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreAdministratorInformationEntryStruct (mkSelector "setIcac:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @icac@
icacSelector :: Selector
icacSelector = mkSelector "icac"

-- | @Selector@ for @setIcac:@
setIcacSelector :: Selector
setIcacSelector = mkSelector "setIcac:"

