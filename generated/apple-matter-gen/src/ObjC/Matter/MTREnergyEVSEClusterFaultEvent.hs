{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterFaultEvent@.
module ObjC.Matter.MTREnergyEVSEClusterFaultEvent
  ( MTREnergyEVSEClusterFaultEvent
  , IsMTREnergyEVSEClusterFaultEvent(..)
  , sessionID
  , setSessionID
  , state
  , setState
  , faultStatePreviousState
  , setFaultStatePreviousState
  , faultStateCurrentState
  , setFaultStateCurrentState
  , sessionIDSelector
  , setSessionIDSelector
  , stateSelector
  , setStateSelector
  , faultStatePreviousStateSelector
  , setFaultStatePreviousStateSelector
  , faultStateCurrentStateSelector
  , setFaultStateCurrentStateSelector


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
sessionID :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
sessionID mtrEnergyEVSEClusterFaultEvent  =
    sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionID:@
setSessionID :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setSessionID mtrEnergyEVSEClusterFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "setSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
state mtrEnergyEVSEClusterFaultEvent  =
    sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setState mtrEnergyEVSEClusterFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- faultStatePreviousState@
faultStatePreviousState :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
faultStatePreviousState mtrEnergyEVSEClusterFaultEvent  =
    sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "faultStatePreviousState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFaultStatePreviousState:@
setFaultStatePreviousState :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setFaultStatePreviousState mtrEnergyEVSEClusterFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "setFaultStatePreviousState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- faultStateCurrentState@
faultStateCurrentState :: IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent => mtrEnergyEVSEClusterFaultEvent -> IO (Id NSNumber)
faultStateCurrentState mtrEnergyEVSEClusterFaultEvent  =
    sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "faultStateCurrentState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFaultStateCurrentState:@
setFaultStateCurrentState :: (IsMTREnergyEVSEClusterFaultEvent mtrEnergyEVSEClusterFaultEvent, IsNSNumber value) => mtrEnergyEVSEClusterFaultEvent -> value -> IO ()
setFaultStateCurrentState mtrEnergyEVSEClusterFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterFaultEvent (mkSelector "setFaultStateCurrentState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @faultStatePreviousState@
faultStatePreviousStateSelector :: Selector
faultStatePreviousStateSelector = mkSelector "faultStatePreviousState"

-- | @Selector@ for @setFaultStatePreviousState:@
setFaultStatePreviousStateSelector :: Selector
setFaultStatePreviousStateSelector = mkSelector "setFaultStatePreviousState:"

-- | @Selector@ for @faultStateCurrentState@
faultStateCurrentStateSelector :: Selector
faultStateCurrentStateSelector = mkSelector "faultStateCurrentState"

-- | @Selector@ for @setFaultStateCurrentState:@
setFaultStateCurrentStateSelector :: Selector
setFaultStateCurrentStateSelector = mkSelector "setFaultStateCurrentState:"

