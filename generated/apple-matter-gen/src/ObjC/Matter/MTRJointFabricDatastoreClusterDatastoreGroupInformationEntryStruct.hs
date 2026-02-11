{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct(..)
  , groupID
  , setGroupID
  , friendlyName
  , setFriendlyName
  , groupKeySetID
  , setGroupKeySetID
  , groupCAT
  , setGroupCAT
  , groupCATVersion
  , setGroupCATVersion
  , groupPermission
  , setGroupPermission
  , groupIDSelector
  , setGroupIDSelector
  , friendlyNameSelector
  , setFriendlyNameSelector
  , groupKeySetIDSelector
  , setGroupKeySetIDSelector
  , groupCATSelector
  , setGroupCATSelector
  , groupCATVersionSelector
  , setGroupCATVersionSelector
  , groupPermissionSelector
  , setGroupPermissionSelector


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

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "groupID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "setGroupID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "friendlyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSString value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "setFriendlyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "groupKeySetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "setGroupKeySetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupCAT@
groupCAT :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupCAT mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "groupCAT") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupCAT:@
setGroupCAT :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupCAT mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "setGroupCAT:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupCATVersion@
groupCATVersion :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupCATVersion mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "groupCATVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupCATVersion:@
setGroupCATVersion :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupCATVersion mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "setGroupCATVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupPermission@
groupPermission :: IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> IO (Id NSNumber)
groupPermission mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "groupPermission") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupPermission:@
setGroupPermission :: (IsMTRJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct -> value -> IO ()
setGroupPermission mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreGroupInformationEntryStruct (mkSelector "setGroupPermission:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector
setFriendlyNameSelector = mkSelector "setFriendlyName:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @groupCAT@
groupCATSelector :: Selector
groupCATSelector = mkSelector "groupCAT"

-- | @Selector@ for @setGroupCAT:@
setGroupCATSelector :: Selector
setGroupCATSelector = mkSelector "setGroupCAT:"

-- | @Selector@ for @groupCATVersion@
groupCATVersionSelector :: Selector
groupCATVersionSelector = mkSelector "groupCATVersion"

-- | @Selector@ for @setGroupCATVersion:@
setGroupCATVersionSelector :: Selector
setGroupCATVersionSelector = mkSelector "setGroupCATVersion:"

-- | @Selector@ for @groupPermission@
groupPermissionSelector :: Selector
groupPermissionSelector = mkSelector "groupPermission"

-- | @Selector@ for @setGroupPermission:@
setGroupPermissionSelector :: Selector
setGroupPermissionSelector = mkSelector "setGroupPermission:"

