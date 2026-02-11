{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterCredentialStruct@.
module ObjC.Matter.MTRDoorLockClusterCredentialStruct
  ( MTRDoorLockClusterCredentialStruct
  , IsMTRDoorLockClusterCredentialStruct(..)
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
credentialType :: IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct => mtrDoorLockClusterCredentialStruct -> IO (Id NSNumber)
credentialType mtrDoorLockClusterCredentialStruct  =
    sendMsg mtrDoorLockClusterCredentialStruct (mkSelector "credentialType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialType:@
setCredentialType :: (IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct, IsNSNumber value) => mtrDoorLockClusterCredentialStruct -> value -> IO ()
setCredentialType mtrDoorLockClusterCredentialStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterCredentialStruct (mkSelector "setCredentialType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialIndex@
credentialIndex :: IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct => mtrDoorLockClusterCredentialStruct -> IO (Id NSNumber)
credentialIndex mtrDoorLockClusterCredentialStruct  =
    sendMsg mtrDoorLockClusterCredentialStruct (mkSelector "credentialIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialIndex:@
setCredentialIndex :: (IsMTRDoorLockClusterCredentialStruct mtrDoorLockClusterCredentialStruct, IsNSNumber value) => mtrDoorLockClusterCredentialStruct -> value -> IO ()
setCredentialIndex mtrDoorLockClusterCredentialStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterCredentialStruct (mkSelector "setCredentialIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

