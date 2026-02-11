{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct
  ( MTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct
  , IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct(..)
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
measurementType :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measurementType mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "measurementType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasurementType mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMeasurementType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- measured@
measured :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measured mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "measured") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasured:@
setMeasured :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasured mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMeasured:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minMeasuredValue@
minMeasuredValue :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
minMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "minMeasuredValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinMeasuredValue:@
setMinMeasuredValue :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMinMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMinMeasuredValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxMeasuredValue@
maxMeasuredValue :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
maxMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "maxMeasuredValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxMeasuredValue:@
setMaxMeasuredValue :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMaxMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "setMaxMeasuredValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accuracyRanges@
accuracyRanges :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSArray)
accuracyRanges mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "accuracyRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccuracyRanges:@
setAccuracyRanges :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSArray value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setAccuracyRanges mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct (mkSelector "setAccuracyRanges:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

