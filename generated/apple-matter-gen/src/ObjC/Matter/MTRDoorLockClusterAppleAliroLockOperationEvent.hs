{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroLockOperationEvent@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroLockOperationEvent
  ( MTRDoorLockClusterAppleAliroLockOperationEvent
  , IsMTRDoorLockClusterAppleAliroLockOperationEvent(..)
  , lockOperationType
  , setLockOperationType
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , credentials
  , setCredentials
  , lockOperationTypeSelector
  , setLockOperationTypeSelector
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
lockOperationType :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterAppleAliroLockOperationEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "lockOperationType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterAppleAliroLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "setLockOperationType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleAliroLockOperationEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleAliroLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterAppleAliroLockOperationEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterAppleAliroLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentials@
credentials :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterAppleAliroLockOperationEvent  =
    sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "credentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSArray value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setCredentials mtrDoorLockClusterAppleAliroLockOperationEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroLockOperationEvent (mkSelector "setCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockOperationType@
lockOperationTypeSelector :: Selector
lockOperationTypeSelector = mkSelector "lockOperationType"

-- | @Selector@ for @setLockOperationType:@
setLockOperationTypeSelector :: Selector
setLockOperationTypeSelector = mkSelector "setLockOperationType:"

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

