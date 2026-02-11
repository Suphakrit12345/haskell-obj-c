{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterGroupKeyMapStruct@.
module ObjC.Matter.MTRGroupKeyManagementClusterGroupKeyMapStruct
  ( MTRGroupKeyManagementClusterGroupKeyMapStruct
  , IsMTRGroupKeyManagementClusterGroupKeyMapStruct(..)
  , groupId
  , setGroupId
  , groupKeySetID
  , setGroupKeySetID
  , fabricIndex
  , setFabricIndex
  , groupIdSelector
  , setGroupIdSelector
  , groupKeySetIDSelector
  , setGroupKeySetIDSelector
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
groupId :: IsMTRGroupKeyManagementClusterGroupKeyMapStruct mtrGroupKeyManagementClusterGroupKeyMapStruct => mtrGroupKeyManagementClusterGroupKeyMapStruct -> IO (Id NSNumber)
groupId mtrGroupKeyManagementClusterGroupKeyMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupKeyMapStruct (mkSelector "groupId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupKeyManagementClusterGroupKeyMapStruct mtrGroupKeyManagementClusterGroupKeyMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeyMapStruct -> value -> IO ()
setGroupId mtrGroupKeyManagementClusterGroupKeyMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupKeyMapStruct (mkSelector "setGroupId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupKeySetID@
groupKeySetID :: IsMTRGroupKeyManagementClusterGroupKeyMapStruct mtrGroupKeyManagementClusterGroupKeyMapStruct => mtrGroupKeyManagementClusterGroupKeyMapStruct -> IO (Id NSNumber)
groupKeySetID mtrGroupKeyManagementClusterGroupKeyMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupKeyMapStruct (mkSelector "groupKeySetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRGroupKeyManagementClusterGroupKeyMapStruct mtrGroupKeyManagementClusterGroupKeyMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeyMapStruct -> value -> IO ()
setGroupKeySetID mtrGroupKeyManagementClusterGroupKeyMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupKeyMapStruct (mkSelector "setGroupKeySetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRGroupKeyManagementClusterGroupKeyMapStruct mtrGroupKeyManagementClusterGroupKeyMapStruct => mtrGroupKeyManagementClusterGroupKeyMapStruct -> IO (Id NSNumber)
fabricIndex mtrGroupKeyManagementClusterGroupKeyMapStruct  =
    sendMsg mtrGroupKeyManagementClusterGroupKeyMapStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRGroupKeyManagementClusterGroupKeyMapStruct mtrGroupKeyManagementClusterGroupKeyMapStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeyMapStruct -> value -> IO ()
setFabricIndex mtrGroupKeyManagementClusterGroupKeyMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterGroupKeyMapStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupId@
groupIdSelector :: Selector
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector
setGroupIdSelector = mkSelector "setGroupId:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

