{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterSlotStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterSlotStruct
  ( MTRDeviceEnergyManagementClusterSlotStruct
  , IsMTRDeviceEnergyManagementClusterSlotStruct(..)
  , minDuration
  , setMinDuration
  , maxDuration
  , setMaxDuration
  , defaultDuration
  , setDefaultDuration
  , elapsedSlotTime
  , setElapsedSlotTime
  , remainingSlotTime
  , setRemainingSlotTime
  , slotIsPausable
  , setSlotIsPausable
  , minPauseDuration
  , setMinPauseDuration
  , maxPauseDuration
  , setMaxPauseDuration
  , manufacturerESAState
  , setManufacturerESAState
  , nominalPower
  , setNominalPower
  , minPower
  , setMinPower
  , maxPower
  , setMaxPower
  , nominalEnergy
  , setNominalEnergy
  , costs
  , setCosts
  , minPowerAdjustment
  , setMinPowerAdjustment
  , maxPowerAdjustment
  , setMaxPowerAdjustment
  , minDurationAdjustment
  , setMinDurationAdjustment
  , maxDurationAdjustment
  , setMaxDurationAdjustment
  , minDurationSelector
  , setMinDurationSelector
  , maxDurationSelector
  , setMaxDurationSelector
  , defaultDurationSelector
  , setDefaultDurationSelector
  , elapsedSlotTimeSelector
  , setElapsedSlotTimeSelector
  , remainingSlotTimeSelector
  , setRemainingSlotTimeSelector
  , slotIsPausableSelector
  , setSlotIsPausableSelector
  , minPauseDurationSelector
  , setMinPauseDurationSelector
  , maxPauseDurationSelector
  , setMaxPauseDurationSelector
  , manufacturerESAStateSelector
  , setManufacturerESAStateSelector
  , nominalPowerSelector
  , setNominalPowerSelector
  , minPowerSelector
  , setMinPowerSelector
  , maxPowerSelector
  , setMaxPowerSelector
  , nominalEnergySelector
  , setNominalEnergySelector
  , costsSelector
  , setCostsSelector
  , minPowerAdjustmentSelector
  , setMinPowerAdjustmentSelector
  , maxPowerAdjustmentSelector
  , setMaxPowerAdjustmentSelector
  , minDurationAdjustmentSelector
  , setMinDurationAdjustmentSelector
  , maxDurationAdjustmentSelector
  , setMaxDurationAdjustmentSelector


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

-- | @- minDuration@
minDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minDuration mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "minDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinDuration:@
setMinDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinDuration mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMinDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDuration@
maxDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxDuration mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "maxDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxDuration mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMaxDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultDuration@
defaultDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
defaultDuration mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "defaultDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultDuration:@
setDefaultDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setDefaultDuration mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setDefaultDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elapsedSlotTime@
elapsedSlotTime :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
elapsedSlotTime mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "elapsedSlotTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElapsedSlotTime:@
setElapsedSlotTime :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setElapsedSlotTime mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setElapsedSlotTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- remainingSlotTime@
remainingSlotTime :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
remainingSlotTime mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "remainingSlotTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRemainingSlotTime:@
setRemainingSlotTime :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setRemainingSlotTime mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setRemainingSlotTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- slotIsPausable@
slotIsPausable :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
slotIsPausable mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "slotIsPausable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSlotIsPausable:@
setSlotIsPausable :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setSlotIsPausable mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setSlotIsPausable:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minPauseDuration@
minPauseDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minPauseDuration mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "minPauseDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinPauseDuration:@
setMinPauseDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinPauseDuration mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMinPauseDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxPauseDuration@
maxPauseDuration :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxPauseDuration mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "maxPauseDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxPauseDuration:@
setMaxPauseDuration :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxPauseDuration mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMaxPauseDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- manufacturerESAState@
manufacturerESAState :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
manufacturerESAState mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "manufacturerESAState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManufacturerESAState:@
setManufacturerESAState :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setManufacturerESAState mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setManufacturerESAState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nominalPower@
nominalPower :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
nominalPower mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "nominalPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNominalPower:@
setNominalPower :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setNominalPower mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setNominalPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minPower@
minPower :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minPower mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "minPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinPower:@
setMinPower :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinPower mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMinPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxPower@
maxPower :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxPower mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "maxPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxPower:@
setMaxPower :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxPower mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMaxPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nominalEnergy@
nominalEnergy :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
nominalEnergy mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "nominalEnergy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNominalEnergy:@
setNominalEnergy :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setNominalEnergy mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setNominalEnergy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- costs@
costs :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSArray)
costs mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "costs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCosts:@
setCosts :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSArray value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setCosts mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setCosts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minPowerAdjustment@
minPowerAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "minPowerAdjustment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinPowerAdjustment:@
setMinPowerAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMinPowerAdjustment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxPowerAdjustment@
maxPowerAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "maxPowerAdjustment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxPowerAdjustment:@
setMaxPowerAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxPowerAdjustment mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMaxPowerAdjustment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minDurationAdjustment@
minDurationAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
minDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "minDurationAdjustment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinDurationAdjustment:@
setMinDurationAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMinDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMinDurationAdjustment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDurationAdjustment@
maxDurationAdjustment :: IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct => mtrDeviceEnergyManagementClusterSlotStruct -> IO (Id NSNumber)
maxDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "maxDurationAdjustment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDurationAdjustment:@
setMaxDurationAdjustment :: (IsMTRDeviceEnergyManagementClusterSlotStruct mtrDeviceEnergyManagementClusterSlotStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotStruct -> value -> IO ()
setMaxDurationAdjustment mtrDeviceEnergyManagementClusterSlotStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotStruct (mkSelector "setMaxDurationAdjustment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minDuration@
minDurationSelector :: Selector
minDurationSelector = mkSelector "minDuration"

-- | @Selector@ for @setMinDuration:@
setMinDurationSelector :: Selector
setMinDurationSelector = mkSelector "setMinDuration:"

-- | @Selector@ for @maxDuration@
maxDurationSelector :: Selector
maxDurationSelector = mkSelector "maxDuration"

-- | @Selector@ for @setMaxDuration:@
setMaxDurationSelector :: Selector
setMaxDurationSelector = mkSelector "setMaxDuration:"

-- | @Selector@ for @defaultDuration@
defaultDurationSelector :: Selector
defaultDurationSelector = mkSelector "defaultDuration"

-- | @Selector@ for @setDefaultDuration:@
setDefaultDurationSelector :: Selector
setDefaultDurationSelector = mkSelector "setDefaultDuration:"

-- | @Selector@ for @elapsedSlotTime@
elapsedSlotTimeSelector :: Selector
elapsedSlotTimeSelector = mkSelector "elapsedSlotTime"

-- | @Selector@ for @setElapsedSlotTime:@
setElapsedSlotTimeSelector :: Selector
setElapsedSlotTimeSelector = mkSelector "setElapsedSlotTime:"

-- | @Selector@ for @remainingSlotTime@
remainingSlotTimeSelector :: Selector
remainingSlotTimeSelector = mkSelector "remainingSlotTime"

-- | @Selector@ for @setRemainingSlotTime:@
setRemainingSlotTimeSelector :: Selector
setRemainingSlotTimeSelector = mkSelector "setRemainingSlotTime:"

-- | @Selector@ for @slotIsPausable@
slotIsPausableSelector :: Selector
slotIsPausableSelector = mkSelector "slotIsPausable"

-- | @Selector@ for @setSlotIsPausable:@
setSlotIsPausableSelector :: Selector
setSlotIsPausableSelector = mkSelector "setSlotIsPausable:"

-- | @Selector@ for @minPauseDuration@
minPauseDurationSelector :: Selector
minPauseDurationSelector = mkSelector "minPauseDuration"

-- | @Selector@ for @setMinPauseDuration:@
setMinPauseDurationSelector :: Selector
setMinPauseDurationSelector = mkSelector "setMinPauseDuration:"

-- | @Selector@ for @maxPauseDuration@
maxPauseDurationSelector :: Selector
maxPauseDurationSelector = mkSelector "maxPauseDuration"

-- | @Selector@ for @setMaxPauseDuration:@
setMaxPauseDurationSelector :: Selector
setMaxPauseDurationSelector = mkSelector "setMaxPauseDuration:"

-- | @Selector@ for @manufacturerESAState@
manufacturerESAStateSelector :: Selector
manufacturerESAStateSelector = mkSelector "manufacturerESAState"

-- | @Selector@ for @setManufacturerESAState:@
setManufacturerESAStateSelector :: Selector
setManufacturerESAStateSelector = mkSelector "setManufacturerESAState:"

-- | @Selector@ for @nominalPower@
nominalPowerSelector :: Selector
nominalPowerSelector = mkSelector "nominalPower"

-- | @Selector@ for @setNominalPower:@
setNominalPowerSelector :: Selector
setNominalPowerSelector = mkSelector "setNominalPower:"

-- | @Selector@ for @minPower@
minPowerSelector :: Selector
minPowerSelector = mkSelector "minPower"

-- | @Selector@ for @setMinPower:@
setMinPowerSelector :: Selector
setMinPowerSelector = mkSelector "setMinPower:"

-- | @Selector@ for @maxPower@
maxPowerSelector :: Selector
maxPowerSelector = mkSelector "maxPower"

-- | @Selector@ for @setMaxPower:@
setMaxPowerSelector :: Selector
setMaxPowerSelector = mkSelector "setMaxPower:"

-- | @Selector@ for @nominalEnergy@
nominalEnergySelector :: Selector
nominalEnergySelector = mkSelector "nominalEnergy"

-- | @Selector@ for @setNominalEnergy:@
setNominalEnergySelector :: Selector
setNominalEnergySelector = mkSelector "setNominalEnergy:"

-- | @Selector@ for @costs@
costsSelector :: Selector
costsSelector = mkSelector "costs"

-- | @Selector@ for @setCosts:@
setCostsSelector :: Selector
setCostsSelector = mkSelector "setCosts:"

-- | @Selector@ for @minPowerAdjustment@
minPowerAdjustmentSelector :: Selector
minPowerAdjustmentSelector = mkSelector "minPowerAdjustment"

-- | @Selector@ for @setMinPowerAdjustment:@
setMinPowerAdjustmentSelector :: Selector
setMinPowerAdjustmentSelector = mkSelector "setMinPowerAdjustment:"

-- | @Selector@ for @maxPowerAdjustment@
maxPowerAdjustmentSelector :: Selector
maxPowerAdjustmentSelector = mkSelector "maxPowerAdjustment"

-- | @Selector@ for @setMaxPowerAdjustment:@
setMaxPowerAdjustmentSelector :: Selector
setMaxPowerAdjustmentSelector = mkSelector "setMaxPowerAdjustment:"

-- | @Selector@ for @minDurationAdjustment@
minDurationAdjustmentSelector :: Selector
minDurationAdjustmentSelector = mkSelector "minDurationAdjustment"

-- | @Selector@ for @setMinDurationAdjustment:@
setMinDurationAdjustmentSelector :: Selector
setMinDurationAdjustmentSelector = mkSelector "setMinDurationAdjustment:"

-- | @Selector@ for @maxDurationAdjustment@
maxDurationAdjustmentSelector :: Selector
maxDurationAdjustmentSelector = mkSelector "maxDurationAdjustment"

-- | @Selector@ for @setMaxDurationAdjustment:@
setMaxDurationAdjustmentSelector :: Selector
setMaxDurationAdjustmentSelector = mkSelector "setMaxDurationAdjustment:"

