{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct
  ( MTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct
  , IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct(..)
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
rangeMin :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "rangeMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRangeMin:@
setRangeMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setRangeMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rangeMax@
rangeMax :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "rangeMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRangeMax:@
setRangeMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setRangeMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentMax@
percentMax :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "percentMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentMax:@
setPercentMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setPercentMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentMin@
percentMin :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "percentMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentMin:@
setPercentMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setPercentMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentTypical@
percentTypical :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "percentTypical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentTypical:@
setPercentTypical :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setPercentTypical:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedMax@
fixedMax :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "fixedMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedMax:@
setFixedMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setFixedMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedMin@
fixedMin :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "fixedMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedMin:@
setFixedMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setFixedMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedTypical@
fixedTypical :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "fixedTypical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedTypical:@
setFixedTypical :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setFixedTypical:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

