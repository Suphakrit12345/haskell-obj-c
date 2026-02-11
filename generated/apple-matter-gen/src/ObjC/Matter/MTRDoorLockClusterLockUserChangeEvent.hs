{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterLockUserChangeEvent@.
module ObjC.Matter.MTRDoorLockClusterLockUserChangeEvent
  ( MTRDoorLockClusterLockUserChangeEvent
  , IsMTRDoorLockClusterLockUserChangeEvent(..)
  , lockDataType
  , setLockDataType
  , dataOperationType
  , setDataOperationType
  , operationSource
  , setOperationSource
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , sourceNode
  , setSourceNode
  , dataIndex
  , setDataIndex
  , lockDataTypeSelector
  , setLockDataTypeSelector
  , dataOperationTypeSelector
  , setDataOperationTypeSelector
  , operationSourceSelector
  , setOperationSourceSelector
  , userIndexSelector
  , setUserIndexSelector
  , fabricIndexSelector
  , setFabricIndexSelector
  , sourceNodeSelector
  , setSourceNodeSelector
  , dataIndexSelector
  , setDataIndexSelector


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

-- | @- lockDataType@
lockDataType :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
lockDataType mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "lockDataType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLockDataType:@
setLockDataType :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setLockDataType mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setLockDataType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dataOperationType@
dataOperationType :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
dataOperationType mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "dataOperationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataOperationType:@
setDataOperationType :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setDataOperationType mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setDataOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "operationSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setOperationSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "sourceNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setSourceNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dataIndex@
dataIndex :: IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent => mtrDoorLockClusterLockUserChangeEvent -> IO (Id NSNumber)
dataIndex mtrDoorLockClusterLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "dataIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataIndex:@
setDataIndex :: (IsMTRDoorLockClusterLockUserChangeEvent mtrDoorLockClusterLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterLockUserChangeEvent -> value -> IO ()
setDataIndex mtrDoorLockClusterLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockUserChangeEvent (mkSelector "setDataIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockDataType@
lockDataTypeSelector :: Selector
lockDataTypeSelector = mkSelector "lockDataType"

-- | @Selector@ for @setLockDataType:@
setLockDataTypeSelector :: Selector
setLockDataTypeSelector = mkSelector "setLockDataType:"

-- | @Selector@ for @dataOperationType@
dataOperationTypeSelector :: Selector
dataOperationTypeSelector = mkSelector "dataOperationType"

-- | @Selector@ for @setDataOperationType:@
setDataOperationTypeSelector :: Selector
setDataOperationTypeSelector = mkSelector "setDataOperationType:"

-- | @Selector@ for @operationSource@
operationSourceSelector :: Selector
operationSourceSelector = mkSelector "operationSource"

-- | @Selector@ for @setOperationSource:@
setOperationSourceSelector :: Selector
setOperationSourceSelector = mkSelector "setOperationSource:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @sourceNode@
sourceNodeSelector :: Selector
sourceNodeSelector = mkSelector "sourceNode"

-- | @Selector@ for @setSourceNode:@
setSourceNodeSelector :: Selector
setSourceNodeSelector = mkSelector "setSourceNode:"

-- | @Selector@ for @dataIndex@
dataIndexSelector :: Selector
dataIndexSelector = mkSelector "dataIndex"

-- | @Selector@ for @setDataIndex:@
setDataIndexSelector :: Selector
setDataIndexSelector = mkSelector "setDataIndex:"

