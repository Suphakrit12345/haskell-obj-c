{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterSlotAdjustmentStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterSlotAdjustmentStruct
  ( MTRDeviceEnergyManagementClusterSlotAdjustmentStruct
  , IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct(..)
  , slotIndex
  , setSlotIndex
  , nominalPower
  , setNominalPower
  , duration
  , setDuration
  , slotIndexSelector
  , setSlotIndexSelector
  , nominalPowerSelector
  , setNominalPowerSelector
  , durationSelector
  , setDurationSelector


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

-- | @- slotIndex@
slotIndex :: IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> IO (Id NSNumber)
slotIndex mtrDeviceEnergyManagementClusterSlotAdjustmentStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotAdjustmentStruct (mkSelector "slotIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSlotIndex:@
setSlotIndex :: (IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> value -> IO ()
setSlotIndex mtrDeviceEnergyManagementClusterSlotAdjustmentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotAdjustmentStruct (mkSelector "setSlotIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nominalPower@
nominalPower :: IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> IO (Id NSNumber)
nominalPower mtrDeviceEnergyManagementClusterSlotAdjustmentStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotAdjustmentStruct (mkSelector "nominalPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNominalPower:@
setNominalPower :: (IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> value -> IO ()
setNominalPower mtrDeviceEnergyManagementClusterSlotAdjustmentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotAdjustmentStruct (mkSelector "setNominalPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterSlotAdjustmentStruct  =
    sendMsg mtrDeviceEnergyManagementClusterSlotAdjustmentStruct (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterSlotAdjustmentStruct mtrDeviceEnergyManagementClusterSlotAdjustmentStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterSlotAdjustmentStruct -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterSlotAdjustmentStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterSlotAdjustmentStruct (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @slotIndex@
slotIndexSelector :: Selector
slotIndexSelector = mkSelector "slotIndex"

-- | @Selector@ for @setSlotIndex:@
setSlotIndexSelector :: Selector
setSlotIndexSelector = mkSelector "setSlotIndex:"

-- | @Selector@ for @nominalPower@
nominalPowerSelector :: Selector
nominalPowerSelector = mkSelector "nominalPower"

-- | @Selector@ for @setNominalPower:@
setNominalPowerSelector :: Selector
setNominalPowerSelector = mkSelector "setNominalPower:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

