{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterGroupInfoMapStruct@.
module ObjC.Matter.MTRGroupKeyManagementClusterGroupInfoMapStruct
  ( MTRGroupKeyManagementClusterGroupInfoMapStruct
  , IsMTRGroupKeyManagementClusterGroupInfoMapStruct(..)
  , groupId
  , setGroupId
  , endpoints
  , setEndpoints
  , groupName
  , setGroupName
  , fabricIndex
  , setFabricIndex
  , groupIdSelector
  , setGroupIdSelector
  , endpointsSelector
  , setEndpointsSelector
  , groupNameSelector
  , setGroupNameSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- groupId@
groupId :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSNumber)
groupId mtrGroupKeyManagementClusterGroupInfoMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setGroupId mtrGroupKeyManagementClusterGroupInfoMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoints@
endpoints :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSArray)
endpoints mtrGroupKeyManagementClusterGroupInfoMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "endpoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSArray value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setEndpoints mtrGroupKeyManagementClusterGroupInfoMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "setEndpoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupName@
groupName :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSString)
groupName mtrGroupKeyManagementClusterGroupInfoMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupName:@
setGroupName :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSString value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setGroupName mtrGroupKeyManagementClusterGroupInfoMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "setGroupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct => mtrGroupKeyManagementClusterGroupInfoMapStruct -> IO (Id NSNumber)
fabricIndex mtrGroupKeyManagementClusterGroupInfoMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRGroupKeyManagementClusterGroupInfoMapStruct mtrGroupKeyManagementClusterGroupInfoMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupInfoMapStruct -> value -> IO ()
setFabricIndex mtrGroupKeyManagementClusterGroupInfoMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupInfoMapStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupId@
groupIdSelector :: Selector
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector
setGroupIdSelector = mkSelector "setGroupId:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector
setEndpointsSelector = mkSelector "setEndpoints:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @setGroupName:@
setGroupNameSelector :: Selector
setGroupNameSelector = mkSelector "setGroupName:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

