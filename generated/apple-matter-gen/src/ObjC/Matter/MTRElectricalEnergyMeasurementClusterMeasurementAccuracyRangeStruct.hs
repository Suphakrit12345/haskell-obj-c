{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct
  ( MTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct
  , IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct(..)
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
rangeMin :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "rangeMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRangeMin:@
setRangeMin :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setRangeMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rangeMax@
rangeMax :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "rangeMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRangeMax:@
setRangeMax :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setRangeMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentMax@
percentMax :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "percentMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentMax:@
setPercentMax :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setPercentMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentMin@
percentMin :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "percentMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentMin:@
setPercentMin :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setPercentMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentTypical@
percentTypical :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "percentTypical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentTypical:@
setPercentTypical :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setPercentTypical:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedMax@
fixedMax :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "fixedMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedMax:@
setFixedMax :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setFixedMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedMin@
fixedMin :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "fixedMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedMin:@
setFixedMin :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setFixedMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fixedTypical@
fixedTypical :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  =
    sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "fixedTypical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFixedTypical:@
setFixedTypical :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct (mkSelector "setFixedTypical:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

