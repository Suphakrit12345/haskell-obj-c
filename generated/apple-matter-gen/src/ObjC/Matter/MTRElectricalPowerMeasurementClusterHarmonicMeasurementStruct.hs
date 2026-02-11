{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct
  ( MTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct
  , IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct(..)
  , order
  , setOrder
  , measurement
  , setMeasurement
  , orderSelector
  , setOrderSelector
  , measurementSelector
  , setMeasurementSelector


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

-- | @- order@
order :: IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> IO (Id NSNumber)
order mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct (mkSelector "order") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrder:@
setOrder :: (IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> value -> IO ()
setOrder mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct (mkSelector "setOrder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- measurement@
measurement :: IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> IO (Id NSNumber)
measurement mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct (mkSelector "measurement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasurement:@
setMeasurement :: (IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> value -> IO ()
setMeasurement mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct (mkSelector "setMeasurement:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @order@
orderSelector :: Selector
orderSelector = mkSelector "order"

-- | @Selector@ for @setOrder:@
setOrderSelector :: Selector
setOrderSelector = mkSelector "setOrder:"

-- | @Selector@ for @measurement@
measurementSelector :: Selector
measurementSelector = mkSelector "measurement"

-- | @Selector@ for @setMeasurement:@
setMeasurementSelector :: Selector
setMeasurementSelector = mkSelector "setMeasurement:"

