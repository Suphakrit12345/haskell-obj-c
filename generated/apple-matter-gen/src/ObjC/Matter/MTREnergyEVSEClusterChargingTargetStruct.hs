{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterChargingTargetStruct@.
module ObjC.Matter.MTREnergyEVSEClusterChargingTargetStruct
  ( MTREnergyEVSEClusterChargingTargetStruct
  , IsMTREnergyEVSEClusterChargingTargetStruct(..)
  , targetTimeMinutesPastMidnight
  , setTargetTimeMinutesPastMidnight
  , targetSoC
  , setTargetSoC
  , addedEnergy
  , setAddedEnergy
  , targetTimeMinutesPastMidnightSelector
  , setTargetTimeMinutesPastMidnightSelector
  , targetSoCSelector
  , setTargetSoCSelector
  , addedEnergySelector
  , setAddedEnergySelector


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

-- | @- targetTimeMinutesPastMidnight@
targetTimeMinutesPastMidnight :: IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct => mtrEnergyEVSEClusterChargingTargetStruct -> IO (Id NSNumber)
targetTimeMinutesPastMidnight mtrEnergyEVSEClusterChargingTargetStruct  =
    sendMsg mtrEnergyEVSEClusterChargingTargetStruct (mkSelector "targetTimeMinutesPastMidnight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetTimeMinutesPastMidnight:@
setTargetTimeMinutesPastMidnight :: (IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetStruct -> value -> IO ()
setTargetTimeMinutesPastMidnight mtrEnergyEVSEClusterChargingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterChargingTargetStruct (mkSelector "setTargetTimeMinutesPastMidnight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- targetSoC@
targetSoC :: IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct => mtrEnergyEVSEClusterChargingTargetStruct -> IO (Id NSNumber)
targetSoC mtrEnergyEVSEClusterChargingTargetStruct  =
    sendMsg mtrEnergyEVSEClusterChargingTargetStruct (mkSelector "targetSoC") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTargetSoC:@
setTargetSoC :: (IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetStruct -> value -> IO ()
setTargetSoC mtrEnergyEVSEClusterChargingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterChargingTargetStruct (mkSelector "setTargetSoC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- addedEnergy@
addedEnergy :: IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct => mtrEnergyEVSEClusterChargingTargetStruct -> IO (Id NSNumber)
addedEnergy mtrEnergyEVSEClusterChargingTargetStruct  =
    sendMsg mtrEnergyEVSEClusterChargingTargetStruct (mkSelector "addedEnergy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAddedEnergy:@
setAddedEnergy :: (IsMTREnergyEVSEClusterChargingTargetStruct mtrEnergyEVSEClusterChargingTargetStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetStruct -> value -> IO ()
setAddedEnergy mtrEnergyEVSEClusterChargingTargetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterChargingTargetStruct (mkSelector "setAddedEnergy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetTimeMinutesPastMidnight@
targetTimeMinutesPastMidnightSelector :: Selector
targetTimeMinutesPastMidnightSelector = mkSelector "targetTimeMinutesPastMidnight"

-- | @Selector@ for @setTargetTimeMinutesPastMidnight:@
setTargetTimeMinutesPastMidnightSelector :: Selector
setTargetTimeMinutesPastMidnightSelector = mkSelector "setTargetTimeMinutesPastMidnight:"

-- | @Selector@ for @targetSoC@
targetSoCSelector :: Selector
targetSoCSelector = mkSelector "targetSoC"

-- | @Selector@ for @setTargetSoC:@
setTargetSoCSelector :: Selector
setTargetSoCSelector = mkSelector "setTargetSoC:"

-- | @Selector@ for @addedEnergy@
addedEnergySelector :: Selector
addedEnergySelector = mkSelector "addedEnergy"

-- | @Selector@ for @setAddedEnergy:@
setAddedEnergySelector :: Selector
setAddedEnergySelector = mkSelector "setAddedEnergy:"

