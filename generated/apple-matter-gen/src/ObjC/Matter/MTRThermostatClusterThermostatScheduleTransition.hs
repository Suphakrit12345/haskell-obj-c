{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterThermostatScheduleTransition@.
module ObjC.Matter.MTRThermostatClusterThermostatScheduleTransition
  ( MTRThermostatClusterThermostatScheduleTransition
  , IsMTRThermostatClusterThermostatScheduleTransition(..)
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
transitionTime :: IsMTRThermostatClusterThermostatScheduleTransition mtrThermostatClusterThermostatScheduleTransition => mtrThermostatClusterThermostatScheduleTransition -> IO (Id NSNumber)
transitionTime mtrThermostatClusterThermostatScheduleTransition  =
    sendMsg mtrThermostatClusterThermostatScheduleTransition (mkSelector "transitionTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRThermostatClusterThermostatScheduleTransition mtrThermostatClusterThermostatScheduleTransition, IsNSNumber value) => mtrThermostatClusterThermostatScheduleTransition -> value -> IO ()
setTransitionTime mtrThermostatClusterThermostatScheduleTransition  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatScheduleTransition (mkSelector "setTransitionTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- heatSetpoint@
heatSetpoint :: IsMTRThermostatClusterThermostatScheduleTransition mtrThermostatClusterThermostatScheduleTransition => mtrThermostatClusterThermostatScheduleTransition -> IO (Id NSNumber)
heatSetpoint mtrThermostatClusterThermostatScheduleTransition  =
    sendMsg mtrThermostatClusterThermostatScheduleTransition (mkSelector "heatSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeatSetpoint:@
setHeatSetpoint :: (IsMTRThermostatClusterThermostatScheduleTransition mtrThermostatClusterThermostatScheduleTransition, IsNSNumber value) => mtrThermostatClusterThermostatScheduleTransition -> value -> IO ()
setHeatSetpoint mtrThermostatClusterThermostatScheduleTransition  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatScheduleTransition (mkSelector "setHeatSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coolSetpoint@
coolSetpoint :: IsMTRThermostatClusterThermostatScheduleTransition mtrThermostatClusterThermostatScheduleTransition => mtrThermostatClusterThermostatScheduleTransition -> IO (Id NSNumber)
coolSetpoint mtrThermostatClusterThermostatScheduleTransition  =
    sendMsg mtrThermostatClusterThermostatScheduleTransition (mkSelector "coolSetpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoolSetpoint:@
setCoolSetpoint :: (IsMTRThermostatClusterThermostatScheduleTransition mtrThermostatClusterThermostatScheduleTransition, IsNSNumber value) => mtrThermostatClusterThermostatScheduleTransition -> value -> IO ()
setCoolSetpoint mtrThermostatClusterThermostatScheduleTransition  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterThermostatScheduleTransition (mkSelector "setCoolSetpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

