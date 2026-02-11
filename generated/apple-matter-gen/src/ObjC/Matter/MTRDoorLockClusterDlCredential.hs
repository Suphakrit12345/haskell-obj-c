{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterDlCredential@.
module ObjC.Matter.MTRDoorLockClusterDlCredential
  ( MTRDoorLockClusterDlCredential
  , IsMTRDoorLockClusterDlCredential(..)
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
credentialType :: IsMTRDoorLockClusterDlCredential mtrDoorLockClusterDlCredential => mtrDoorLockClusterDlCredential -> IO (Id NSNumber)
credentialType mtrDoorLockClusterDlCredential  =
    sendMsg mtrDoorLockClusterDlCredential (mkSelector "credentialType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialType:@
setCredentialType :: (IsMTRDoorLockClusterDlCredential mtrDoorLockClusterDlCredential, IsNSNumber value) => mtrDoorLockClusterDlCredential -> value -> IO ()
setCredentialType mtrDoorLockClusterDlCredential  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterDlCredential (mkSelector "setCredentialType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credentialIndex@
credentialIndex :: IsMTRDoorLockClusterDlCredential mtrDoorLockClusterDlCredential => mtrDoorLockClusterDlCredential -> IO (Id NSNumber)
credentialIndex mtrDoorLockClusterDlCredential  =
    sendMsg mtrDoorLockClusterDlCredential (mkSelector "credentialIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredentialIndex:@
setCredentialIndex :: (IsMTRDoorLockClusterDlCredential mtrDoorLockClusterDlCredential, IsNSNumber value) => mtrDoorLockClusterDlCredential -> value -> IO ()
setCredentialIndex mtrDoorLockClusterDlCredential  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterDlCredential (mkSelector "setCredentialIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

