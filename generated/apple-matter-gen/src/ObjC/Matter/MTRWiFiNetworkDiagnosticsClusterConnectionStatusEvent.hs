{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent@.
module ObjC.Matter.MTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent
  ( MTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent
  , IsMTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent(..)
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
connectionStatus :: IsMTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent => mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent -> IO (Id NSNumber)
connectionStatus mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent  =
    sendMsg mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent (mkSelector "connectionStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionStatus:@
setConnectionStatus :: (IsMTRWiFiNetworkDiagnosticsClusterConnectionStatusEvent mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent -> value -> IO ()
setConnectionStatus mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWiFiNetworkDiagnosticsClusterConnectionStatusEvent (mkSelector "setConnectionStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionStatus@
connectionStatusSelector :: Selector
connectionStatusSelector = mkSelector "connectionStatus"

-- | @Selector@ for @setConnectionStatus:@
setConnectionStatusSelector :: Selector
setConnectionStatusSelector = mkSelector "setConnectionStatus:"

