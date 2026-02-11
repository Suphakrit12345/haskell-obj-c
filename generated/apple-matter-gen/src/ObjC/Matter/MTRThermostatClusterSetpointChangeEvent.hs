{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSetpointChangeEvent@.
module ObjC.Matter.MTRThermostatClusterSetpointChangeEvent
  ( MTRThermostatClusterSetpointChangeEvent
  , IsMTRThermostatClusterSetpointChangeEvent(..)
  , systemMode
  , setSystemMode
  , occupancy
  , setOccupancy
  , previousSetpoint
  , setPreviousSetpoint
  , currentSetpoint
  , setCurrentSetpoint
  , systemModeSelector
  , setSystemModeSelector
  , occupancySelector
  , setOccupancySelector
  , previousSetpointSelector
  , setPreviousSetpointSelector
  , currentSetpointSelector
  , setCurrentSetpointSelector


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

-- | @- systemMode@
systemMode :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
systemMode mtrThermostatClusterSetpointChangeEvent  =
    sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "systemMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setSystemMode mtrThermostatClusterSetpointChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "setSystemMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- occupancy@
occupancy :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
occupancy mtrThermostatClusterSetpointChangeEvent  =
    sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "occupancy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOccupancy:@
setOccupancy :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setOccupancy mtrThermostatClusterSetpointChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "setOccupancy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previousSetpoint@
previousSetpoint :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
previousSetpoint mtrThermostatClusterSetpointChangeEvent  =
    sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "previousSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousSetpoint:@
setPreviousSetpoint :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setPreviousSetpoint mtrThermostatClusterSetpointChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "setPreviousSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentSetpoint@
currentSetpoint :: IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent => mtrThermostatClusterSetpointChangeEvent -> IO (Id NSNumber)
currentSetpoint mtrThermostatClusterSetpointChangeEvent  =
    sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "currentSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentSetpoint:@
setCurrentSetpoint :: (IsMTRThermostatClusterSetpointChangeEvent mtrThermostatClusterSetpointChangeEvent, IsNSNumber value) => mtrThermostatClusterSetpointChangeEvent -> value -> IO ()
setCurrentSetpoint mtrThermostatClusterSetpointChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterSetpointChangeEvent (mkSelector "setCurrentSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @occupancy@
occupancySelector :: Selector
occupancySelector = mkSelector "occupancy"

-- | @Selector@ for @setOccupancy:@
setOccupancySelector :: Selector
setOccupancySelector = mkSelector "setOccupancy:"

-- | @Selector@ for @previousSetpoint@
previousSetpointSelector :: Selector
previousSetpointSelector = mkSelector "previousSetpoint"

-- | @Selector@ for @setPreviousSetpoint:@
setPreviousSetpointSelector :: Selector
setPreviousSetpointSelector = mkSelector "setPreviousSetpoint:"

-- | @Selector@ for @currentSetpoint@
currentSetpointSelector :: Selector
currentSetpointSelector = mkSelector "currentSetpoint"

-- | @Selector@ for @setCurrentSetpoint:@
setCurrentSetpointSelector :: Selector
setCurrentSetpointSelector = mkSelector "setCurrentSetpoint:"

