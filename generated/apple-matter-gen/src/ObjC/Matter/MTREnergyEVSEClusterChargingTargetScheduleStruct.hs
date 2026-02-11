{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterChargingTargetScheduleStruct@.
module ObjC.Matter.MTREnergyEVSEClusterChargingTargetScheduleStruct
  ( MTREnergyEVSEClusterChargingTargetScheduleStruct
  , IsMTREnergyEVSEClusterChargingTargetScheduleStruct(..)
  , dayOfWeekForSequence
  , setDayOfWeekForSequence
  , chargingTargets
  , setChargingTargets
  , dayOfWeekForSequenceSelector
  , setDayOfWeekForSequenceSelector
  , chargingTargetsSelector
  , setChargingTargetsSelector


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

-- | @- dayOfWeekForSequence@
dayOfWeekForSequence :: IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> IO (Id NSNumber)
dayOfWeekForSequence mtrEnergyEVSEClusterChargingTargetScheduleStruct  =
    sendMsg mtrEnergyEVSEClusterChargingTargetScheduleStruct (mkSelector "dayOfWeekForSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayOfWeekForSequence:@
setDayOfWeekForSequence :: (IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct, IsNSNumber value) => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> value -> IO ()
setDayOfWeekForSequence mtrEnergyEVSEClusterChargingTargetScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterChargingTargetScheduleStruct (mkSelector "setDayOfWeekForSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chargingTargets@
chargingTargets :: IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> IO (Id NSArray)
chargingTargets mtrEnergyEVSEClusterChargingTargetScheduleStruct  =
    sendMsg mtrEnergyEVSEClusterChargingTargetScheduleStruct (mkSelector "chargingTargets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChargingTargets:@
setChargingTargets :: (IsMTREnergyEVSEClusterChargingTargetScheduleStruct mtrEnergyEVSEClusterChargingTargetScheduleStruct, IsNSArray value) => mtrEnergyEVSEClusterChargingTargetScheduleStruct -> value -> IO ()
setChargingTargets mtrEnergyEVSEClusterChargingTargetScheduleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterChargingTargetScheduleStruct (mkSelector "setChargingTargets:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfWeekForSequence@
dayOfWeekForSequenceSelector :: Selector
dayOfWeekForSequenceSelector = mkSelector "dayOfWeekForSequence"

-- | @Selector@ for @setDayOfWeekForSequence:@
setDayOfWeekForSequenceSelector :: Selector
setDayOfWeekForSequenceSelector = mkSelector "setDayOfWeekForSequence:"

-- | @Selector@ for @chargingTargets@
chargingTargetsSelector :: Selector
chargingTargetsSelector = mkSelector "chargingTargets"

-- | @Selector@ for @setChargingTargets:@
setChargingTargetsSelector :: Selector
setChargingTargetsSelector = mkSelector "setChargingTargets:"

