{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnergyTransferStoppedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEnergyTransferStoppedEvent
  ( MTREnergyEVSEClusterEnergyTransferStoppedEvent
  , IsMTREnergyEVSEClusterEnergyTransferStoppedEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , reason
  , setReason
  , energyTransferred
  , setEnergyTransferred
  , energyDischarged
  , setEnergyDischarged
  , sessionIDSelector
  , setSessionIDSelector
  , stateSelector
  , setStateSelector
  , reasonSelector
  , setReasonSelector
  , energyTransferredSelector
  , setEnergyTransferredSelector
  , energyDischargedSelector
  , setEnergyDischargedSelector


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

-- | @- sessionID@
sessionID :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEnergyTransferStoppedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEnergyTransferStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "setSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterEnergyTransferStoppedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setState mtrEnergyEVSEClusterEnergyTransferStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reason@
reason :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
reason mtrEnergyEVSEClusterEnergyTransferStoppedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReason:@
setReason :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setReason mtrEnergyEVSEClusterEnergyTransferStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "setReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- energyTransferred@
energyTransferred :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
energyTransferred mtrEnergyEVSEClusterEnergyTransferStoppedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "energyTransferred") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyTransferred:@
setEnergyTransferred :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setEnergyTransferred mtrEnergyEVSEClusterEnergyTransferStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "setEnergyTransferred:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- energyDischarged@
energyDischarged :: IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> IO (Id NSNumber)
energyDischarged mtrEnergyEVSEClusterEnergyTransferStoppedEvent  =
    sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "energyDischarged") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyDischarged:@
setEnergyDischarged :: (IsMTREnergyEVSEClusterEnergyTransferStoppedEvent mtrEnergyEVSEClusterEnergyTransferStoppedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEnergyTransferStoppedEvent -> value -> IO ()
setEnergyDischarged mtrEnergyEVSEClusterEnergyTransferStoppedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEnergyTransferStoppedEvent (mkSelector "setEnergyDischarged:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @setSessionID:@
setSessionIDSelector :: Selector
setSessionIDSelector = mkSelector "setSessionID:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector
setReasonSelector = mkSelector "setReason:"

-- | @Selector@ for @energyTransferred@
energyTransferredSelector :: Selector
energyTransferredSelector = mkSelector "energyTransferred"

-- | @Selector@ for @setEnergyTransferred:@
setEnergyTransferredSelector :: Selector
setEnergyTransferredSelector = mkSelector "setEnergyTransferred:"

-- | @Selector@ for @energyDischarged@
energyDischargedSelector :: Selector
energyDischargedSelector = mkSelector "energyDischarged"

-- | @Selector@ for @setEnergyDischarged:@
setEnergyDischargedSelector :: Selector
setEnergyDischargedSelector = mkSelector "setEnergyDischarged:"

