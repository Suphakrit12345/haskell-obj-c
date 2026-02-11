{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEVNotDetectedEvent@.
module ObjC.Matter.MTREnergyEVSEClusterEVNotDetectedEvent
  ( MTREnergyEVSEClusterEVNotDetectedEvent
  , IsMTREnergyEVSEClusterEVNotDetectedEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , sessionDuration
  , setSessionDuration
  , sessionEnergyCharged
  , setSessionEnergyCharged
  , sessionEnergyDischarged
  , setSessionEnergyDischarged
  , sessionIDSelector
  , setSessionIDSelector
  , stateSelector
  , setStateSelector
  , sessionDurationSelector
  , setSessionDurationSelector
  , sessionEnergyChargedSelector
  , setSessionEnergyChargedSelector
  , sessionEnergyDischargedSelector
  , setSessionEnergyDischargedSelector


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
sessionID :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterEVNotDetectedEvent  =
    sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterEVNotDetectedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "setSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterEVNotDetectedEvent  =
    sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setState mtrEnergyEVSEClusterEVNotDetectedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sessionDuration@
sessionDuration :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionDuration mtrEnergyEVSEClusterEVNotDetectedEvent  =
    sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "sessionDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionDuration:@
setSessionDuration :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionDuration mtrEnergyEVSEClusterEVNotDetectedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "setSessionDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sessionEnergyCharged@
sessionEnergyCharged :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionEnergyCharged mtrEnergyEVSEClusterEVNotDetectedEvent  =
    sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "sessionEnergyCharged") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionEnergyCharged:@
setSessionEnergyCharged :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionEnergyCharged mtrEnergyEVSEClusterEVNotDetectedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "setSessionEnergyCharged:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sessionEnergyDischarged@
sessionEnergyDischarged :: IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent => mtrEnergyEVSEClusterEVNotDetectedEvent -> IO (Id NSNumber)
sessionEnergyDischarged mtrEnergyEVSEClusterEVNotDetectedEvent  =
    sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "sessionEnergyDischarged") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionEnergyDischarged:@
setSessionEnergyDischarged :: (IsMTREnergyEVSEClusterEVNotDetectedEvent mtrEnergyEVSEClusterEVNotDetectedEvent, IsNSNumber value) => mtrEnergyEVSEClusterEVNotDetectedEvent -> value -> IO ()
setSessionEnergyDischarged mtrEnergyEVSEClusterEVNotDetectedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterEVNotDetectedEvent (mkSelector "setSessionEnergyDischarged:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @sessionDuration@
sessionDurationSelector :: Selector
sessionDurationSelector = mkSelector "sessionDuration"

-- | @Selector@ for @setSessionDuration:@
setSessionDurationSelector :: Selector
setSessionDurationSelector = mkSelector "setSessionDuration:"

-- | @Selector@ for @sessionEnergyCharged@
sessionEnergyChargedSelector :: Selector
sessionEnergyChargedSelector = mkSelector "sessionEnergyCharged"

-- | @Selector@ for @setSessionEnergyCharged:@
setSessionEnergyChargedSelector :: Selector
setSessionEnergyChargedSelector = mkSelector "setSessionEnergyCharged:"

-- | @Selector@ for @sessionEnergyDischarged@
sessionEnergyDischargedSelector :: Selector
sessionEnergyDischargedSelector = mkSelector "sessionEnergyDischarged"

-- | @Selector@ for @setSessionEnergyDischarged:@
setSessionEnergyDischargedSelector :: Selector
setSessionEnergyDischargedSelector = mkSelector "setSessionEnergyDischarged:"

