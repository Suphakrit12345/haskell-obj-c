{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterScheduleTransitionStruct@.
module ObjC.Matter.MTRThermostatClusterScheduleTransitionStruct
  ( MTRThermostatClusterScheduleTransitionStruct
  , IsMTRThermostatClusterScheduleTransitionStruct(..)
  , dayOfWeek
  , setDayOfWeek
  , transitionTime
  , setTransitionTime
  , presetHandle
  , setPresetHandle
  , systemMode
  , setSystemMode
  , coolingSetpoint
  , setCoolingSetpoint
  , heatingSetpoint
  , setHeatingSetpoint
  , dayOfWeekSelector
  , setDayOfWeekSelector
  , transitionTimeSelector
  , setTransitionTimeSelector
  , presetHandleSelector
  , setPresetHandleSelector
  , systemModeSelector
  , setSystemModeSelector
  , coolingSetpointSelector
  , setCoolingSetpointSelector
  , heatingSetpointSelector
  , setHeatingSetpointSelector


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

-- | @- dayOfWeek@
dayOfWeek :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
dayOfWeek mtrThermostatClusterScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "dayOfWeek") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayOfWeek:@
setDayOfWeek :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setDayOfWeek mtrThermostatClusterScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "setDayOfWeek:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transitionTime@
transitionTime :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
transitionTime mtrThermostatClusterScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setTransitionTime mtrThermostatClusterScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSData)
presetHandle mtrThermostatClusterScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "presetHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSData value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setPresetHandle mtrThermostatClusterScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "setPresetHandle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- systemMode@
systemMode :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
systemMode mtrThermostatClusterScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "systemMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setSystemMode mtrThermostatClusterScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "setSystemMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coolingSetpoint@
coolingSetpoint :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
coolingSetpoint mtrThermostatClusterScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "coolingSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoolingSetpoint:@
setCoolingSetpoint :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setCoolingSetpoint mtrThermostatClusterScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "setCoolingSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- heatingSetpoint@
heatingSetpoint :: IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct => mtrThermostatClusterScheduleTransitionStruct -> IO (Id NSNumber)
heatingSetpoint mtrThermostatClusterScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "heatingSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeatingSetpoint:@
setHeatingSetpoint :: (IsMTRThermostatClusterScheduleTransitionStruct mtrThermostatClusterScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterScheduleTransitionStruct -> value -> IO ()
setHeatingSetpoint mtrThermostatClusterScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTransitionStruct (mkSelector "setHeatingSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfWeek@
dayOfWeekSelector :: Selector
dayOfWeekSelector = mkSelector "dayOfWeek"

-- | @Selector@ for @setDayOfWeek:@
setDayOfWeekSelector :: Selector
setDayOfWeekSelector = mkSelector "setDayOfWeek:"

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @coolingSetpoint@
coolingSetpointSelector :: Selector
coolingSetpointSelector = mkSelector "coolingSetpoint"

-- | @Selector@ for @setCoolingSetpoint:@
setCoolingSetpointSelector :: Selector
setCoolingSetpointSelector = mkSelector "setCoolingSetpoint:"

-- | @Selector@ for @heatingSetpoint@
heatingSetpointSelector :: Selector
heatingSetpointSelector = mkSelector "heatingSetpoint"

-- | @Selector@ for @setHeatingSetpoint:@
setHeatingSetpointSelector :: Selector
setHeatingSetpointSelector = mkSelector "setHeatingSetpoint:"

