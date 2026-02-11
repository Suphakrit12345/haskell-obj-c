{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterWeeklyScheduleTransitionStruct@.
module ObjC.Matter.MTRThermostatClusterWeeklyScheduleTransitionStruct
  ( MTRThermostatClusterWeeklyScheduleTransitionStruct
  , IsMTRThermostatClusterWeeklyScheduleTransitionStruct(..)
  , transitionTime
  , setTransitionTime
  , heatSetpoint
  , setHeatSetpoint
  , coolSetpoint
  , setCoolSetpoint
  , transitionTimeSelector
  , setTransitionTimeSelector
  , heatSetpointSelector
  , setHeatSetpointSelector
  , coolSetpointSelector
  , setCoolSetpointSelector


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

-- | @- transitionTime@
transitionTime :: IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct => mtrThermostatClusterWeeklyScheduleTransitionStruct -> IO (Id NSNumber)
transitionTime mtrThermostatClusterWeeklyScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterWeeklyScheduleTransitionStruct (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterWeeklyScheduleTransitionStruct -> value -> IO ()
setTransitionTime mtrThermostatClusterWeeklyScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterWeeklyScheduleTransitionStruct (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- heatSetpoint@
heatSetpoint :: IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct => mtrThermostatClusterWeeklyScheduleTransitionStruct -> IO (Id NSNumber)
heatSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterWeeklyScheduleTransitionStruct (mkSelector "heatSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeatSetpoint:@
setHeatSetpoint :: (IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterWeeklyScheduleTransitionStruct -> value -> IO ()
setHeatSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterWeeklyScheduleTransitionStruct (mkSelector "setHeatSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coolSetpoint@
coolSetpoint :: IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct => mtrThermostatClusterWeeklyScheduleTransitionStruct -> IO (Id NSNumber)
coolSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct  =
    sendMsg mtrThermostatClusterWeeklyScheduleTransitionStruct (mkSelector "coolSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoolSetpoint:@
setCoolSetpoint :: (IsMTRThermostatClusterWeeklyScheduleTransitionStruct mtrThermostatClusterWeeklyScheduleTransitionStruct, IsNSNumber value) => mtrThermostatClusterWeeklyScheduleTransitionStruct -> value -> IO ()
setCoolSetpoint mtrThermostatClusterWeeklyScheduleTransitionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterWeeklyScheduleTransitionStruct (mkSelector "setCoolSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @heatSetpoint@
heatSetpointSelector :: Selector
heatSetpointSelector = mkSelector "heatSetpoint"

-- | @Selector@ for @setHeatSetpoint:@
setHeatSetpointSelector :: Selector
setHeatSetpointSelector = mkSelector "setHeatSetpoint:"

-- | @Selector@ for @coolSetpoint@
coolSetpointSelector :: Selector
coolSetpointSelector = mkSelector "coolSetpoint"

-- | @Selector@ for @setCoolSetpoint:@
setCoolSetpointSelector :: Selector
setCoolSetpointSelector = mkSelector "setCoolSetpoint:"

