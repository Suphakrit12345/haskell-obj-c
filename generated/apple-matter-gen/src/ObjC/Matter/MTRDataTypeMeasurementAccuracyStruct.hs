{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeMeasurementAccuracyStruct@.
module ObjC.Matter.MTRDataTypeMeasurementAccuracyStruct
  ( MTRDataTypeMeasurementAccuracyStruct
  , IsMTRDataTypeMeasurementAccuracyStruct(..)
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
measurementType :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
measurementType mtrDataTypeMeasurementAccuracyStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "measurementType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMeasurementType mtrDataTypeMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "setMeasurementType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- measured@
measured :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
measured mtrDataTypeMeasurementAccuracyStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "measured") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasured:@
setMeasured :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMeasured mtrDataTypeMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "setMeasured:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minMeasuredValue@
minMeasuredValue :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
minMeasuredValue mtrDataTypeMeasurementAccuracyStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "minMeasuredValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinMeasuredValue:@
setMinMeasuredValue :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMinMeasuredValue mtrDataTypeMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "setMinMeasuredValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxMeasuredValue@
maxMeasuredValue :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
maxMeasuredValue mtrDataTypeMeasurementAccuracyStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "maxMeasuredValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxMeasuredValue:@
setMaxMeasuredValue :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMaxMeasuredValue mtrDataTypeMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "setMaxMeasuredValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accuracyRanges@
accuracyRanges :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSArray)
accuracyRanges mtrDataTypeMeasurementAccuracyStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "accuracyRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccuracyRanges:@
setAccuracyRanges :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSArray value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setAccuracyRanges mtrDataTypeMeasurementAccuracyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyStruct (mkSelector "setAccuracyRanges:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

