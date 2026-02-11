{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterForecastStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterForecastStruct
  ( MTRDeviceEnergyManagementClusterForecastStruct
  , IsMTRDeviceEnergyManagementClusterForecastStruct(..)
  , forecastID
  , setForecastID
  , activeSlotNumber
  , setActiveSlotNumber
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , earliestStartTime
  , setEarliestStartTime
  , latestEndTime
  , setLatestEndTime
  , isPausable
  , setIsPausable
  , slots
  , setSlots
  , forecastUpdateReason
  , setForecastUpdateReason
  , forecastIDSelector
  , setForecastIDSelector
  , activeSlotNumberSelector
  , setActiveSlotNumberSelector
  , startTimeSelector
  , setStartTimeSelector
  , endTimeSelector
  , setEndTimeSelector
  , earliestStartTimeSelector
  , setEarliestStartTimeSelector
  , latestEndTimeSelector
  , setLatestEndTimeSelector
  , isPausableSelector
  , setIsPausableSelector
  , slotsSelector
  , setSlotsSelector
  , forecastUpdateReasonSelector
  , setForecastUpdateReasonSelector


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

-- | @- forecastID@
forecastID :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
forecastID mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "forecastID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setForecastID:@
setForecastID :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setForecastID mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setForecastID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activeSlotNumber@
activeSlotNumber :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
activeSlotNumber mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "activeSlotNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActiveSlotNumber:@
setActiveSlotNumber :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setActiveSlotNumber mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setActiveSlotNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTime@
startTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
startTime mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setStartTime mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endTime@
endTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
endTime mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "endTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndTime:@
setEndTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setEndTime mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- earliestStartTime@
earliestStartTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
earliestStartTime mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "earliestStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEarliestStartTime:@
setEarliestStartTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setEarliestStartTime mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setEarliestStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- latestEndTime@
latestEndTime :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
latestEndTime mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "latestEndTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLatestEndTime:@
setLatestEndTime :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setLatestEndTime mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setLatestEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isPausable@
isPausable :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
isPausable mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "isPausable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIsPausable:@
setIsPausable :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setIsPausable mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setIsPausable:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- slots@
slots :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSArray)
slots mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "slots") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSlots:@
setSlots :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSArray value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setSlots mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setSlots:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- forecastUpdateReason@
forecastUpdateReason :: IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct => mtrDeviceEnergyManagementClusterForecastStruct -> IO (Id NSNumber)
forecastUpdateReason mtrDeviceEnergyManagementClusterForecastStruct  =
    sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "forecastUpdateReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setForecastUpdateReason:@
setForecastUpdateReason :: (IsMTRDeviceEnergyManagementClusterForecastStruct mtrDeviceEnergyManagementClusterForecastStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterForecastStruct -> value -> IO ()
setForecastUpdateReason mtrDeviceEnergyManagementClusterForecastStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterForecastStruct (mkSelector "setForecastUpdateReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forecastID@
forecastIDSelector :: Selector
forecastIDSelector = mkSelector "forecastID"

-- | @Selector@ for @setForecastID:@
setForecastIDSelector :: Selector
setForecastIDSelector = mkSelector "setForecastID:"

-- | @Selector@ for @activeSlotNumber@
activeSlotNumberSelector :: Selector
activeSlotNumberSelector = mkSelector "activeSlotNumber"

-- | @Selector@ for @setActiveSlotNumber:@
setActiveSlotNumberSelector :: Selector
setActiveSlotNumberSelector = mkSelector "setActiveSlotNumber:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @earliestStartTime@
earliestStartTimeSelector :: Selector
earliestStartTimeSelector = mkSelector "earliestStartTime"

-- | @Selector@ for @setEarliestStartTime:@
setEarliestStartTimeSelector :: Selector
setEarliestStartTimeSelector = mkSelector "setEarliestStartTime:"

-- | @Selector@ for @latestEndTime@
latestEndTimeSelector :: Selector
latestEndTimeSelector = mkSelector "latestEndTime"

-- | @Selector@ for @setLatestEndTime:@
setLatestEndTimeSelector :: Selector
setLatestEndTimeSelector = mkSelector "setLatestEndTime:"

-- | @Selector@ for @isPausable@
isPausableSelector :: Selector
isPausableSelector = mkSelector "isPausable"

-- | @Selector@ for @setIsPausable:@
setIsPausableSelector :: Selector
setIsPausableSelector = mkSelector "setIsPausable:"

-- | @Selector@ for @slots@
slotsSelector :: Selector
slotsSelector = mkSelector "slots"

-- | @Selector@ for @setSlots:@
setSlotsSelector :: Selector
setSlotsSelector = mkSelector "setSlots:"

-- | @Selector@ for @forecastUpdateReason@
forecastUpdateReasonSelector :: Selector
forecastUpdateReasonSelector = mkSelector "forecastUpdateReason"

-- | @Selector@ for @setForecastUpdateReason:@
setForecastUpdateReasonSelector :: Selector
setForecastUpdateReasonSelector = mkSelector "setForecastUpdateReason:"

