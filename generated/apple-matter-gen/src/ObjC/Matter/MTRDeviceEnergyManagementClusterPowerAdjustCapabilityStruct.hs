{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct
  ( MTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct
  , IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct(..)
  , powerAdjustCapability
  , setPowerAdjustCapability
  , cause
  , setCause
  , powerAdjustCapabilitySelector
  , setPowerAdjustCapabilitySelector
  , causeSelector
  , setCauseSelector


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

-- | @- powerAdjustCapability@
powerAdjustCapability :: IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> IO (Id NSArray)
powerAdjustCapability mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct (mkSelector "powerAdjustCapability") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPowerAdjustCapability:@
setPowerAdjustCapability :: (IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct, IsNSArray value) => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> value -> IO ()
setPowerAdjustCapability mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct (mkSelector "setPowerAdjustCapability:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct  =
    sendMsg mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct (mkSelector "cause") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPowerAdjustCapabilityStruct mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementClusterPowerAdjustCapabilityStruct (mkSelector "setCause:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerAdjustCapability@
powerAdjustCapabilitySelector :: Selector
powerAdjustCapabilitySelector = mkSelector "powerAdjustCapability"

-- | @Selector@ for @setPowerAdjustCapability:@
setPowerAdjustCapabilitySelector :: Selector
setPowerAdjustCapabilitySelector = mkSelector "setPowerAdjustCapability:"

-- | @Selector@ for @cause@
causeSelector :: Selector
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector
setCauseSelector = mkSelector "setCause:"

