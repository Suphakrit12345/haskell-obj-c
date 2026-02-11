{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterConstraintsStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterConstraintsStruct
  ( MTRDeviceEnergyManagementClusterConstraintsStruct
  , IsMTRDeviceEnergyManagementClusterConstraintsStruct(..)
  , startTime
  , setStartTime
  , duration
  , setDuration
  , nominalPower
  , setNominalPower
  , maximumEnergy
  , setMaximumEnergy
  , loadControl
  , setLoadControl
  , startTimeSelector
  , setStartTimeSelector
  , durationSelector
  , setDurationSelector
  , nominalPowerSelector
  , setNominalPowerSelector
  , maximumEnergySelector
  , setMaximumEnergySelector
  , loadControlSelector
  , setLoadControlSelector


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

-- | @- startTime@
startTime :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
startTime mtrDeviceEnergyManagementClusterConstraintsStruct  =
    sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setStartTime mtrDeviceEnergyManagementClusterConstraintsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterConstraintsStruct  =
    sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterConstraintsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nominalPower@
nominalPower :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
nominalPower mtrDeviceEnergyManagementClusterConstraintsStruct  =
    sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "nominalPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNominalPower:@
setNominalPower :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setNominalPower mtrDeviceEnergyManagementClusterConstraintsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "setNominalPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maximumEnergy@
maximumEnergy :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
maximumEnergy mtrDeviceEnergyManagementClusterConstraintsStruct  =
    sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "maximumEnergy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumEnergy:@
setMaximumEnergy :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setMaximumEnergy mtrDeviceEnergyManagementClusterConstraintsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "setMaximumEnergy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- loadControl@
loadControl :: IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct => mtrDeviceEnergyManagementClusterConstraintsStruct -> IO (Id NSNumber)
loadControl mtrDeviceEnergyManagementClusterConstraintsStruct  =
    sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "loadControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLoadControl:@
setLoadControl :: (IsMTRDeviceEnergyManagementClusterConstraintsStruct mtrDeviceEnergyManagementClusterConstraintsStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterConstraintsStruct -> value -> IO ()
setLoadControl mtrDeviceEnergyManagementClusterConstraintsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterConstraintsStruct (mkSelector "setLoadControl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @nominalPower@
nominalPowerSelector :: Selector
nominalPowerSelector = mkSelector "nominalPower"

-- | @Selector@ for @setNominalPower:@
setNominalPowerSelector :: Selector
setNominalPowerSelector = mkSelector "setNominalPower:"

-- | @Selector@ for @maximumEnergy@
maximumEnergySelector :: Selector
maximumEnergySelector = mkSelector "maximumEnergy"

-- | @Selector@ for @setMaximumEnergy:@
setMaximumEnergySelector :: Selector
setMaximumEnergySelector = mkSelector "setMaximumEnergy:"

-- | @Selector@ for @loadControl@
loadControlSelector :: Selector
loadControlSelector = mkSelector "loadControl"

-- | @Selector@ for @setLoadControl:@
setLoadControlSelector :: Selector
setLoadControlSelector = mkSelector "setLoadControl:"

