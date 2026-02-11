{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustStruct
  ( MTRDeviceEnergyManagementClusterPowerAdjustStruct
  , IsMTRDeviceEnergyManagementClusterPowerAdjustStruct(..)
  , minPower
  , setMinPower
  , maxPower
  , setMaxPower
  , minDuration
  , setMinDuration
  , maxDuration
  , setMaxDuration
  , minPowerSelector
  , setMinPowerSelector
  , maxPowerSelector
  , setMaxPowerSelector
  , minDurationSelector
  , setMinDurationSelector
  , maxDurationSelector
  , setMaxDurationSelector


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

-- | @- minPower@
minPower :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
minPower mtrDeviceEnergyManagementClusterPowerAdjustStruct  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "minPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinPower:@
setMinPower :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMinPower mtrDeviceEnergyManagementClusterPowerAdjustStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "setMinPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxPower@
maxPower :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
maxPower mtrDeviceEnergyManagementClusterPowerAdjustStruct  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "maxPower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxPower:@
setMaxPower :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMaxPower mtrDeviceEnergyManagementClusterPowerAdjustStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "setMaxPower:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minDuration@
minDuration :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
minDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "minDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinDuration:@
setMinDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMinDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "setMinDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxDuration@
maxDuration :: IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> IO (Id NSNumber)
maxDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "maxDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustStruct mtrDeviceEnergyManagementClusterPowerAdjustStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustStruct -> value -> IO ()
setMaxDuration mtrDeviceEnergyManagementClusterPowerAdjustStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustStruct (mkSelector "setMaxDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

