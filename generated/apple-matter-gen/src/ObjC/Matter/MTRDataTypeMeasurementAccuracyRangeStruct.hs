{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeMeasurementAccuracyRangeStruct@.
module ObjC.Matter.MTRDataTypeMeasurementAccuracyRangeStruct
  ( MTRDataTypeMeasurementAccuracyRangeStruct
  , IsMTRDataTypeMeasurementAccuracyRangeStruct(..)
  , rangeMin
  , setRangeMin
  , rangeMax
  , setRangeMax
  , percentMax
  , setPercentMax
  , percentMin
  , setPercentMin
  , percentTypical
  , setPercentTypical
  , fixedMax
  , setFixedMax
  , fixedMin
  , setFixedMin
  , fixedTypical
  , setFixedTypical
  , rangeMinSelector
  , setRangeMinSelector
  , rangeMaxSelector
  , setRangeMaxSelector
  , percentMaxSelector
  , setPercentMaxSelector
  , percentMinSelector
  , setPercentMinSelector
  , percentTypicalSelector
  , setPercentTypicalSelector
  , fixedMaxSelector
  , setFixedMaxSelector
  , fixedMinSelector
  , setFixedMinSelector
  , fixedTypicalSelector
  , setFixedTypicalSelector


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

-- | @- rangeMin@
rangeMin :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMin mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "rangeMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRangeMin:@
setRangeMin :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMin mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setRangeMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rangeMax@
rangeMax :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMax mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "rangeMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRangeMax:@
setRangeMax :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMax mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setRangeMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentMax@
percentMax :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMax mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "percentMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentMax:@
setPercentMax :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMax mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setPercentMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentMin@
percentMin :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMin mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "percentMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentMin:@
setPercentMin :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMin mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setPercentMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentTypical@
percentTypical :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentTypical mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "percentTypical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentTypical:@
setPercentTypical :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentTypical mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setPercentTypical:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedMax@
fixedMax :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMax mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "fixedMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedMax:@
setFixedMax :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMax mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setFixedMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedMin@
fixedMin :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMin mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "fixedMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedMin:@
setFixedMin :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMin mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setFixedMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedTypical@
fixedTypical :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedTypical mtrDataTypeMeasurementAccuracyRangeStruct  =
    sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "fixedTypical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedTypical:@
setFixedTypical :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedTypical mtrDataTypeMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeMeasurementAccuracyRangeStruct (mkSelector "setFixedTypical:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rangeMin@
rangeMinSelector :: Selector
rangeMinSelector = mkSelector "rangeMin"

-- | @Selector@ for @setRangeMin:@
setRangeMinSelector :: Selector
setRangeMinSelector = mkSelector "setRangeMin:"

-- | @Selector@ for @rangeMax@
rangeMaxSelector :: Selector
rangeMaxSelector = mkSelector "rangeMax"

-- | @Selector@ for @setRangeMax:@
setRangeMaxSelector :: Selector
setRangeMaxSelector = mkSelector "setRangeMax:"

-- | @Selector@ for @percentMax@
percentMaxSelector :: Selector
percentMaxSelector = mkSelector "percentMax"

-- | @Selector@ for @setPercentMax:@
setPercentMaxSelector :: Selector
setPercentMaxSelector = mkSelector "setPercentMax:"

-- | @Selector@ for @percentMin@
percentMinSelector :: Selector
percentMinSelector = mkSelector "percentMin"

-- | @Selector@ for @setPercentMin:@
setPercentMinSelector :: Selector
setPercentMinSelector = mkSelector "setPercentMin:"

-- | @Selector@ for @percentTypical@
percentTypicalSelector :: Selector
percentTypicalSelector = mkSelector "percentTypical"

-- | @Selector@ for @setPercentTypical:@
setPercentTypicalSelector :: Selector
setPercentTypicalSelector = mkSelector "setPercentTypical:"

-- | @Selector@ for @fixedMax@
fixedMaxSelector :: Selector
fixedMaxSelector = mkSelector "fixedMax"

-- | @Selector@ for @setFixedMax:@
setFixedMaxSelector :: Selector
setFixedMaxSelector = mkSelector "setFixedMax:"

-- | @Selector@ for @fixedMin@
fixedMinSelector :: Selector
fixedMinSelector = mkSelector "fixedMin"

-- | @Selector@ for @setFixedMin:@
setFixedMinSelector :: Selector
setFixedMinSelector = mkSelector "setFixedMin:"

-- | @Selector@ for @fixedTypical@
fixedTypicalSelector :: Selector
fixedTypicalSelector = mkSelector "fixedTypical"

-- | @Selector@ for @setFixedTypical:@
setFixedTypicalSelector :: Selector
setFixedTypicalSelector = mkSelector "setFixedTypical:"

