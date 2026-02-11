{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkDiagnosticsClusterDisconnectionEvent@.
module ObjC.Matter.MTRWiFiNetworkDiagnosticsClusterDisconnectionEvent
  ( MTRWiFiNetworkDiagnosticsClusterDisconnectionEvent
  , IsMTRWiFiNetworkDiagnosticsClusterDisconnectionEvent(..)
  , reasonCode
  , setReasonCode
  , reasonCodeSelector
  , setReasonCodeSelector


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

-- | @- reasonCode@
reasonCode :: IsMTRWiFiNetworkDiagnosticsClusterDisconnectionEvent mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent => mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent -> IO (Id NSNumber)
reasonCode mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent  =
    sendMsg mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent (mkSelector "reasonCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReasonCode:@
setReasonCode :: (IsMTRWiFiNetworkDiagnosticsClusterDisconnectionEvent mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent -> value -> IO ()
setReasonCode mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWiFiNetworkDiagnosticsClusterDisconnectionEvent (mkSelector "setReasonCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reasonCode@
reasonCodeSelector :: Selector
reasonCodeSelector = mkSelector "reasonCode"

-- | @Selector@ for @setReasonCode:@
setReasonCodeSelector :: Selector
setReasonCodeSelector = mkSelector "setReasonCode:"

