{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroLockUserChangeEvent@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroLockUserChangeEvent
  ( MTRDoorLockClusterAppleAliroLockUserChangeEvent
  , IsMTRDoorLockClusterAppleAliroLockUserChangeEvent(..)
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
lockDataType :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
lockDataType mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "lockDataType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLockDataType:@
setLockDataType :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setLockDataType mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setLockDataType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dataOperationType@
dataOperationType :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
dataOperationType mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "dataOperationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataOperationType:@
setDataOperationType :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setDataOperationType mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setDataOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "operationSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setOperationSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "sourceNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setSourceNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dataIndex@
dataIndex :: IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> IO (Id NSNumber)
dataIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "dataIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataIndex:@
setDataIndex :: (IsMTRDoorLockClusterAppleAliroLockUserChangeEvent mtrDoorLockClusterAppleAliroLockUserChangeEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockUserChangeEvent -> value -> IO ()
setDataIndex mtrDoorLockClusterAppleAliroLockUserChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockUserChangeEvent (mkSelector "setDataIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

