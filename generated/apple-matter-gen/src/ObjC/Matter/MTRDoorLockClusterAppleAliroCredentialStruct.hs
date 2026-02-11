{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroCredentialStruct@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroCredentialStruct
  ( MTRDoorLockClusterAppleAliroCredentialStruct
  , IsMTRDoorLockClusterAppleAliroCredentialStruct(..)
  , credentialType
  , setCredentialType
  , credentialIndex
  , setCredentialIndex
  , credentialTypeSelector
  , setCredentialTypeSelector
  , credentialIndexSelector
  , setCredentialIndexSelector


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

-- | @- credentialType@
credentialType :: IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct => mtrDoorLockClusterAppleAliroCredentialStruct -> IO (Id NSNumber)
credentialType mtrDoorLockClusterAppleAliroCredentialStruct  =
    sendMsg mtrDoorLockClusterAppleAliroCredentialStruct (mkSelector "credentialType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialType:@
setCredentialType :: (IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct, IsNSNumber value) => mtrDoorLockClusterAppleAliroCredentialStruct -> value -> IO ()
setCredentialType mtrDoorLockClusterAppleAliroCredentialStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroCredentialStruct (mkSelector "setCredentialType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialIndex@
credentialIndex :: IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct => mtrDoorLockClusterAppleAliroCredentialStruct -> IO (Id NSNumber)
credentialIndex mtrDoorLockClusterAppleAliroCredentialStruct  =
    sendMsg mtrDoorLockClusterAppleAliroCredentialStruct (mkSelector "credentialIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialIndex:@
setCredentialIndex :: (IsMTRDoorLockClusterAppleAliroCredentialStruct mtrDoorLockClusterAppleAliroCredentialStruct, IsNSNumber value) => mtrDoorLockClusterAppleAliroCredentialStruct -> value -> IO ()
setCredentialIndex mtrDoorLockClusterAppleAliroCredentialStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterAppleAliroCredentialStruct (mkSelector "setCredentialIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @credentialType@
credentialTypeSelector :: Selector
credentialTypeSelector = mkSelector "credentialType"

-- | @Selector@ for @setCredentialType:@
setCredentialTypeSelector :: Selector
setCredentialTypeSelector = mkSelector "setCredentialType:"

-- | @Selector@ for @credentialIndex@
credentialIndexSelector :: Selector
credentialIndexSelector = mkSelector "credentialIndex"

-- | @Selector@ for @setCredentialIndex:@
setCredentialIndexSelector :: Selector
setCredentialIndexSelector = mkSelector "setCredentialIndex:"

