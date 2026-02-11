{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterConnectionStatusEvent@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterConnectionStatusEvent
  ( MTRThreadNetworkDiagnosticsClusterConnectionStatusEvent
  , IsMTRThreadNetworkDiagnosticsClusterConnectionStatusEvent(..)
  , connectionStatus
  , setConnectionStatus
  , connectionStatusSelector
  , setConnectionStatusSelector


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

-- | @- connectionStatus@
connectionStatus :: IsMTRThreadNetworkDiagnosticsClusterConnectionStatusEvent mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent => mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent -> IO (Id NSNumber)
connectionStatus mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent  =
    sendMsg mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent (mkSelector "connectionStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionStatus:@
setConnectionStatus :: (IsMTRThreadNetworkDiagnosticsClusterConnectionStatusEvent mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent -> value -> IO ()
setConnectionStatus mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterConnectionStatusEvent (mkSelector "setConnectionStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionStatus@
connectionStatusSelector :: Selector
connectionStatusSelector = mkSelector "connectionStatus"

-- | @Selector@ for @setConnectionStatus:@
setConnectionStatusSelector :: Selector
setConnectionStatusSelector = mkSelector "setConnectionStatus:"

