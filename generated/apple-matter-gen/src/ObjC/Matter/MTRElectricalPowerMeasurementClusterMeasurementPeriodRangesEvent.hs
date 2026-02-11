{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent
  ( MTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent
  , IsMTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent(..)
  , ranges
  , setRanges
  , rangesSelector
  , setRangesSelector


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

-- | @- ranges@
ranges :: IsMTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent => mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent -> IO (Id NSArray)
ranges mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent (mkSelector "ranges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRanges:@
setRanges :: (IsMTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent, IsNSArray value) => mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent -> value -> IO ()
setRanges mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent (mkSelector "setRanges:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ranges@
rangesSelector :: Selector
rangesSelector = mkSelector "ranges"

-- | @Selector@ for @setRanges:@
setRangesSelector :: Selector
setRangesSelector = mkSelector "setRanges:"

