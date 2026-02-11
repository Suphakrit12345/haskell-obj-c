{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterSecurityPolicy@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterSecurityPolicy
  ( MTRThreadNetworkDiagnosticsClusterSecurityPolicy
  , IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy(..)
  , rotationTime
  , setRotationTime
  , flags
  , setFlags
  , rotationTimeSelector
  , setRotationTimeSelector
  , flagsSelector
  , setFlagsSelector


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

-- | @- rotationTime@
rotationTime :: IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> IO (Id NSNumber)
rotationTime mtrThreadNetworkDiagnosticsClusterSecurityPolicy  =
    sendMsg mtrThreadNetworkDiagnosticsClusterSecurityPolicy (mkSelector "rotationTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRotationTime:@
setRotationTime :: (IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> value -> IO ()
setRotationTime mtrThreadNetworkDiagnosticsClusterSecurityPolicy  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterSecurityPolicy (mkSelector "setRotationTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- flags@
flags :: IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> IO (Id NSNumber)
flags mtrThreadNetworkDiagnosticsClusterSecurityPolicy  =
    sendMsg mtrThreadNetworkDiagnosticsClusterSecurityPolicy (mkSelector "flags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFlags:@
setFlags :: (IsMTRThreadNetworkDiagnosticsClusterSecurityPolicy mtrThreadNetworkDiagnosticsClusterSecurityPolicy, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterSecurityPolicy -> value -> IO ()
setFlags mtrThreadNetworkDiagnosticsClusterSecurityPolicy  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterSecurityPolicy (mkSelector "setFlags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rotationTime@
rotationTimeSelector :: Selector
rotationTimeSelector = mkSelector "rotationTime"

-- | @Selector@ for @setRotationTime:@
setRotationTimeSelector :: Selector
setRotationTimeSelector = mkSelector "setRotationTime:"

-- | @Selector@ for @flags@
flagsSelector :: Selector
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector
setFlagsSelector = mkSelector "setFlags:"

