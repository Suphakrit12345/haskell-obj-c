{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroLockOperationErrorEvent@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroLockOperationErrorEvent
  ( MTRDoorLockClusterAppleAliroLockOperationErrorEvent
  , IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent(..)
  , lockOperationType
  , setLockOperationType
  , operationError
  , setOperationError
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , credentials
  , setCredentials
  , lockOperationTypeSelector
  , setLockOperationTypeSelector
  , operationErrorSelector
  , setOperationErrorSelector
  , userIndexSelector
  , setUserIndexSelector
  , fabricIndexSelector
  , setFabricIndexSelector
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
lockOperationType :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterAppleAliroLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "lockOperationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterAppleAliroLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "setLockOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationError@
operationError :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
operationError mtrDoorLockClusterAppleAliroLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "operationError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationError:@
setOperationError :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setOperationError mtrDoorLockClusterAppleAliroLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "setOperationError:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentials@
credentials :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterAppleAliroLockOperationErrorEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "credentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSArray value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setCredentials mtrDoorLockClusterAppleAliroLockOperationErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationErrorEvent (mkSelector "setCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockOperationType@
lockOperationTypeSelector :: Selector
lockOperationTypeSelector = mkSelector "lockOperationType"

-- | @Selector@ for @setLockOperationType:@
setLockOperationTypeSelector :: Selector
setLockOperationTypeSelector = mkSelector "setLockOperationType:"

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

-- | @Selector@ for @credentials@
credentialsSelector :: Selector
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector
setCredentialsSelector = mkSelector "setCredentials:"

