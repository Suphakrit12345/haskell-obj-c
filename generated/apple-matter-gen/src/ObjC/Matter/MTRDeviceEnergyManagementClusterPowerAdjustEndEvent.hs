{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustEndEvent@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustEndEvent
  ( MTRDeviceEnergyManagementClusterPowerAdjustEndEvent
  , IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent(..)
  , cause
  , setCause
  , duration
  , setDuration
  , energyUse
  , setEnergyUse
  , causeSelector
  , setCauseSelector
  , durationSelector
  , setDurationSelector
  , energyUseSelector
  , setEnergyUseSelector


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

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPowerAdjustEndEvent  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustEndEvent (mkSelector "cause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPowerAdjustEndEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustEndEvent (mkSelector "setCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterPowerAdjustEndEvent  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustEndEvent (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterPowerAdjustEndEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustEndEvent (mkSelector "setDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- energyUse@
energyUse :: IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> IO (Id NSNumber)
energyUse mtrDeviceEnergyManagementClusterPowerAdjustEndEvent  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustEndEvent (mkSelector "energyUse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnergyUse:@
setEnergyUse :: (IsMTRDeviceEnergyManagementClusterPowerAdjustEndEvent mtrDeviceEnergyManagementClusterPowerAdjustEndEvent, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustEndEvent -> value -> IO ()
setEnergyUse mtrDeviceEnergyManagementClusterPowerAdjustEndEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustEndEvent (mkSelector "setEnergyUse:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cause@
causeSelector :: Selector
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector
setCauseSelector = mkSelector "setCause:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @energyUse@
energyUseSelector :: Selector
energyUseSelector = mkSelector "energyUse"

-- | @Selector@ for @setEnergyUse:@
setEnergyUseSelector :: Selector
setEnergyUseSelector = mkSelector "setEnergyUse:"

