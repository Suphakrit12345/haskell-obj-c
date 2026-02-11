{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent@.
module ObjC.Matter.MTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent
  ( MTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent
  , IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent(..)
  , associationFailureCause
  , setAssociationFailureCause
  , associationFailure
  , setAssociationFailure
  , status
  , setStatus
  , associationFailureCauseSelector
  , setAssociationFailureCauseSelector
  , associationFailureSelector
  , setAssociationFailureSelector
  , statusSelector
  , setStatusSelector


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

-- | @- associationFailureCause@
associationFailureCause :: IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> IO (Id NSNumber)
associationFailureCause mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent  =
    sendMsg mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent (mkSelector "associationFailureCause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAssociationFailureCause:@
setAssociationFailureCause :: (IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> value -> IO ()
setAssociationFailureCause mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent (mkSelector "setAssociationFailureCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- associationFailure@
associationFailure :: IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> IO (Id NSNumber)
associationFailure mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent  =
    sendMsg mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent (mkSelector "associationFailure") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAssociationFailure:@
setAssociationFailure :: (IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> value -> IO ()
setAssociationFailure mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent (mkSelector "setAssociationFailure:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> IO (Id NSNumber)
status mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent  =
    sendMsg mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRWiFiNetworkDiagnosticsClusterAssociationFailureEvent mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent, IsNSNumber value) => mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent -> value -> IO ()
setStatus mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWiFiNetworkDiagnosticsClusterAssociationFailureEvent (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @associationFailureCause@
associationFailureCauseSelector :: Selector
associationFailureCauseSelector = mkSelector "associationFailureCause"

-- | @Selector@ for @setAssociationFailureCause:@
setAssociationFailureCauseSelector :: Selector
setAssociationFailureCauseSelector = mkSelector "setAssociationFailureCause:"

-- | @Selector@ for @associationFailure@
associationFailureSelector :: Selector
associationFailureSelector = mkSelector "associationFailure"

-- | @Selector@ for @setAssociationFailure:@
setAssociationFailureSelector :: Selector
setAssociationFailureSelector = mkSelector "setAssociationFailure:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

