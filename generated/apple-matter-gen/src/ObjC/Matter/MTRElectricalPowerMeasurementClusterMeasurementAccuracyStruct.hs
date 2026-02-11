{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct
  ( MTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct
  , IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct(..)
  , measurementType
  , setMeasurementType
  , measured
  , setMeasured
  , minMeasuredValue
  , setMinMeasuredValue
  , maxMeasuredValue
  , setMaxMeasuredValue
  , accuracyRanges
  , setAccuracyRanges
  , measurementTypeSelector
  , setMeasurementTypeSelector
  , measuredSelector
  , setMeasuredSelector
  , minMeasuredValueSelector
  , setMinMeasuredValueSelector
  , maxMeasuredValueSelector
  , setMaxMeasuredValueSelector
  , accuracyRangesSelector
  , setAccuracyRangesSelector


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

-- | @- measurementType@
measurementType :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measurementType mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "measurementType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasurementType mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMeasurementType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- measured@
measured :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measured mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "measured") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasured:@
setMeasured :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasured mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMeasured:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minMeasuredValue@
minMeasuredValue :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
minMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "minMeasuredValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinMeasuredValue:@
setMinMeasuredValue :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMinMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMinMeasuredValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxMeasuredValue@
maxMeasuredValue :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
maxMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "maxMeasuredValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxMeasuredValue:@
setMaxMeasuredValue :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMaxMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMaxMeasuredValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accuracyRanges@
accuracyRanges :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSArray)
accuracyRanges mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "accuracyRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccuracyRanges:@
setAccuracyRanges :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSArray value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setAccuracyRanges mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct (mkSelector "setAccuracyRanges:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @measurementType@
measurementTypeSelector :: Selector
measurementTypeSelector = mkSelector "measurementType"

-- | @Selector@ for @setMeasurementType:@
setMeasurementTypeSelector :: Selector
setMeasurementTypeSelector = mkSelector "setMeasurementType:"

-- | @Selector@ for @measured@
measuredSelector :: Selector
measuredSelector = mkSelector "measured"

-- | @Selector@ for @setMeasured:@
setMeasuredSelector :: Selector
setMeasuredSelector = mkSelector "setMeasured:"

-- | @Selector@ for @minMeasuredValue@
minMeasuredValueSelector :: Selector
minMeasuredValueSelector = mkSelector "minMeasuredValue"

-- | @Selector@ for @setMinMeasuredValue:@
setMinMeasuredValueSelector :: Selector
setMinMeasuredValueSelector = mkSelector "setMinMeasuredValue:"

-- | @Selector@ for @maxMeasuredValue@
maxMeasuredValueSelector :: Selector
maxMeasuredValueSelector = mkSelector "maxMeasuredValue"

-- | @Selector@ for @setMaxMeasuredValue:@
setMaxMeasuredValueSelector :: Selector
setMaxMeasuredValueSelector = mkSelector "setMaxMeasuredValue:"

-- | @Selector@ for @accuracyRanges@
accuracyRangesSelector :: Selector
accuracyRangesSelector = mkSelector "accuracyRanges"

-- | @Selector@ for @setAccuracyRanges:@
setAccuracyRangesSelector :: Selector
setAccuracyRangesSelector = mkSelector "setAccuracyRanges:"

