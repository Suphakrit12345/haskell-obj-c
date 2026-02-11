{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterLockOperationEvent@.
module ObjC.Matter.MTRDoorLockClusterLockOperationEvent
  ( MTRDoorLockClusterLockOperationEvent
  , IsMTRDoorLockClusterLockOperationEvent(..)
  , lockOperationType
  , setLockOperationType
  , operationSource
  , setOperationSource
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , sourceNode
  , setSourceNode
  , credentials
  , setCredentials
  , lockOperationTypeSelector
  , setLockOperationTypeSelector
  , operationSourceSelector
  , setOperationSourceSelector
  , userIndexSelector
  , setUserIndexSelector
  , fabricIndexSelector
  , setFabricIndexSelector
  , sourceNodeSelector
  , setSourceNodeSelector
  , credentialsSelector
  , setCredentialsSelector


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

-- | @- lockOperationType@
lockOperationType :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterLockOperationEvent  =
    sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "lockOperationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "setLockOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterLockOperationEvent  =
    sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "operationSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "setOperationSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterLockOperationEvent  =
    sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterLockOperationEvent  =
    sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterLockOperationEvent  =
    sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "sourceNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "setSourceNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentials@
credentials :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterLockOperationEvent  =
    sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "credentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSArray value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setCredentials mtrDoorLockClusterLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationEvent (mkSelector "setCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockOperationType@
lockOperationTypeSelector :: Selector
lockOperationTypeSelector = mkSelector "lockOperationType"

-- | @Selector@ for @setLockOperationType:@
setLockOperationTypeSelector :: Selector
setLockOperationTypeSelector = mkSelector "setLockOperationType:"

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

-- | @Selector@ for @credentials@
credentialsSelector :: Selector
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector
setCredentialsSelector = mkSelector "setCredentials:"

