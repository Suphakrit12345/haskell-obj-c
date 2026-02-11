{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterLockOperationErrorEvent@.
module ObjC.Matter.MTRDoorLockClusterLockOperationErrorEvent
  ( MTRDoorLockClusterLockOperationErrorEvent
  , IsMTRDoorLockClusterLockOperationErrorEvent(..)
  , lockOperationType
  , setLockOperationType
  , operationSource
  , setOperationSource
  , operationError
  , setOperationError
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
  , operationErrorSelector
  , setOperationErrorSelector
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
lockOperationType :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "lockOperationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setLockOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "operationSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setOperationSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationError@
operationError :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
operationError mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "operationError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationError:@
setOperationError :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setOperationError mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setOperationError:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "sourceNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setSourceNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentials@
credentials :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "credentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSArray value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setCredentials mtrDoorLockClusterLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterLockOperationErrorEvent (mkSelector "setCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @operationError@
operationErrorSelector :: Selector
operationErrorSelector = mkSelector "operationError"

-- | @Selector@ for @setOperationError:@
setOperationErrorSelector :: Selector
setOperationErrorSelector = mkSelector "setOperationError:"

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

