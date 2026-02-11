{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterScheduleTypeStruct@.
module ObjC.Matter.MTRThermostatClusterScheduleTypeStruct
  ( MTRThermostatClusterScheduleTypeStruct
  , IsMTRThermostatClusterScheduleTypeStruct(..)
  , systemMode
  , setSystemMode
  , numberOfSchedules
  , setNumberOfSchedules
  , scheduleTypeFeatures
  , setScheduleTypeFeatures
  , systemModeSelector
  , setSystemModeSelector
  , numberOfSchedulesSelector
  , setNumberOfSchedulesSelector
  , scheduleTypeFeaturesSelector
  , setScheduleTypeFeaturesSelector


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
systemMode :: IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct => mtrThermostatClusterScheduleTypeStruct -> IO (Id NSNumber)
systemMode mtrThermostatClusterScheduleTypeStruct  =
    sendMsg mtrThermostatClusterScheduleTypeStruct (mkSelector "systemMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemMode:@
setSystemMode :: (IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct, IsNSNumber value) => mtrThermostatClusterScheduleTypeStruct -> value -> IO ()
setSystemMode mtrThermostatClusterScheduleTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTypeStruct (mkSelector "setSystemMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfSchedules@
numberOfSchedules :: IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct => mtrThermostatClusterScheduleTypeStruct -> IO (Id NSNumber)
numberOfSchedules mtrThermostatClusterScheduleTypeStruct  =
    sendMsg mtrThermostatClusterScheduleTypeStruct (mkSelector "numberOfSchedules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfSchedules:@
setNumberOfSchedules :: (IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct, IsNSNumber value) => mtrThermostatClusterScheduleTypeStruct -> value -> IO ()
setNumberOfSchedules mtrThermostatClusterScheduleTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTypeStruct (mkSelector "setNumberOfSchedules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scheduleTypeFeatures@
scheduleTypeFeatures :: IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct => mtrThermostatClusterScheduleTypeStruct -> IO (Id NSNumber)
scheduleTypeFeatures mtrThermostatClusterScheduleTypeStruct  =
    sendMsg mtrThermostatClusterScheduleTypeStruct (mkSelector "scheduleTypeFeatures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScheduleTypeFeatures:@
setScheduleTypeFeatures :: (IsMTRThermostatClusterScheduleTypeStruct mtrThermostatClusterScheduleTypeStruct, IsNSNumber value) => mtrThermostatClusterScheduleTypeStruct -> value -> IO ()
setScheduleTypeFeatures mtrThermostatClusterScheduleTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterScheduleTypeStruct (mkSelector "setScheduleTypeFeatures:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @systemMode@
systemModeSelector :: Selector
systemModeSelector = mkSelector "systemMode"

-- | @Selector@ for @setSystemMode:@
setSystemModeSelector :: Selector
setSystemModeSelector = mkSelector "setSystemMode:"

-- | @Selector@ for @numberOfSchedules@
numberOfSchedulesSelector :: Selector
numberOfSchedulesSelector = mkSelector "numberOfSchedules"

-- | @Selector@ for @setNumberOfSchedules:@
setNumberOfSchedulesSelector :: Selector
setNumberOfSchedulesSelector = mkSelector "setNumberOfSchedules:"

-- | @Selector@ for @scheduleTypeFeatures@
scheduleTypeFeaturesSelector :: Selector
scheduleTypeFeaturesSelector = mkSelector "scheduleTypeFeatures"

-- | @Selector@ for @setScheduleTypeFeatures:@
setScheduleTypeFeaturesSelector :: Selector
setScheduleTypeFeaturesSelector = mkSelector "setScheduleTypeFeatures:"

