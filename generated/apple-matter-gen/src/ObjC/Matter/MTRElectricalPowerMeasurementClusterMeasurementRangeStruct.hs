{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementRangeStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementRangeStruct
  ( MTRElectricalPowerMeasurementClusterMeasurementRangeStruct
  , IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct(..)
  , measurementType
  , setMeasurementType
  , min_
  , setMin
  , max_
  , setMax
  , startTimestamp
  , setStartTimestamp
  , endTimestamp
  , setEndTimestamp
  , minTimestamp
  , setMinTimestamp
  , maxTimestamp
  , setMaxTimestamp
  , startSystime
  , setStartSystime
  , endSystime
  , setEndSystime
  , minSystime
  , setMinSystime
  , maxSystime
  , setMaxSystime
  , measurementTypeSelector
  , setMeasurementTypeSelector
  , minSelector
  , setMinSelector
  , maxSelector
  , setMaxSelector
  , startTimestampSelector
  , setStartTimestampSelector
  , endTimestampSelector
  , setEndTimestampSelector
  , minTimestampSelector
  , setMinTimestampSelector
  , maxTimestampSelector
  , setMaxTimestampSelector
  , startSystimeSelector
  , setStartSystimeSelector
  , endSystimeSelector
  , setEndSystimeSelector
  , minSystimeSelector
  , setMinSystimeSelector
  , maxSystimeSelector
  , setMaxSystimeSelector


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
measurementType :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
measurementType mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "measurementType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMeasurementType mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMeasurementType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- min@
min_ :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
min_ mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "min") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMin:@
setMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMin mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- max@
max_ :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
max_ mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "max") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMax:@
setMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMax mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMax:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startTimestamp@
startTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
startTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "startTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTimestamp:@
setStartTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setStartTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setStartTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endTimestamp@
endTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
endTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "endTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndTimestamp:@
setEndTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setEndTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setEndTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minTimestamp@
minTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
minTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "minTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinTimestamp:@
setMinTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMinTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMinTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxTimestamp@
maxTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
maxTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "maxTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxTimestamp:@
setMaxTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMaxTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMaxTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startSystime@
startSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
startSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "startSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartSystime:@
setStartSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setStartSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setStartSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endSystime@
endSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
endSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "endSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndSystime:@
setEndSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setEndSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setEndSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minSystime@
minSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
minSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "minSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinSystime:@
setMinSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMinSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMinSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxSystime@
maxSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
maxSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  =
    sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "maxSystime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxSystime:@
setMaxSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMaxSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrElectricalPowerMeasurementClusterMeasurementRangeStruct (mkSelector "setMaxSystime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @measurementType@
measurementTypeSelector :: Selector
measurementTypeSelector = mkSelector "measurementType"

-- | @Selector@ for @setMeasurementType:@
setMeasurementTypeSelector :: Selector
setMeasurementTypeSelector = mkSelector "setMeasurementType:"

-- | @Selector@ for @min@
minSelector :: Selector
minSelector = mkSelector "min"

-- | @Selector@ for @setMin:@
setMinSelector :: Selector
setMinSelector = mkSelector "setMin:"

-- | @Selector@ for @max@
maxSelector :: Selector
maxSelector = mkSelector "max"

-- | @Selector@ for @setMax:@
setMaxSelector :: Selector
setMaxSelector = mkSelector "setMax:"

-- | @Selector@ for @startTimestamp@
startTimestampSelector :: Selector
startTimestampSelector = mkSelector "startTimestamp"

-- | @Selector@ for @setStartTimestamp:@
setStartTimestampSelector :: Selector
setStartTimestampSelector = mkSelector "setStartTimestamp:"

-- | @Selector@ for @endTimestamp@
endTimestampSelector :: Selector
endTimestampSelector = mkSelector "endTimestamp"

-- | @Selector@ for @setEndTimestamp:@
setEndTimestampSelector :: Selector
setEndTimestampSelector = mkSelector "setEndTimestamp:"

-- | @Selector@ for @minTimestamp@
minTimestampSelector :: Selector
minTimestampSelector = mkSelector "minTimestamp"

-- | @Selector@ for @setMinTimestamp:@
setMinTimestampSelector :: Selector
setMinTimestampSelector = mkSelector "setMinTimestamp:"

-- | @Selector@ for @maxTimestamp@
maxTimestampSelector :: Selector
maxTimestampSelector = mkSelector "maxTimestamp"

-- | @Selector@ for @setMaxTimestamp:@
setMaxTimestampSelector :: Selector
setMaxTimestampSelector = mkSelector "setMaxTimestamp:"

-- | @Selector@ for @startSystime@
startSystimeSelector :: Selector
startSystimeSelector = mkSelector "startSystime"

-- | @Selector@ for @setStartSystime:@
setStartSystimeSelector :: Selector
setStartSystimeSelector = mkSelector "setStartSystime:"

-- | @Selector@ for @endSystime@
endSystimeSelector :: Selector
endSystimeSelector = mkSelector "endSystime"

-- | @Selector@ for @setEndSystime:@
setEndSystimeSelector :: Selector
setEndSystimeSelector = mkSelector "setEndSystime:"

-- | @Selector@ for @minSystime@
minSystimeSelector :: Selector
minSystimeSelector = mkSelector "minSystime"

-- | @Selector@ for @setMinSystime:@
setMinSystimeSelector :: Selector
setMinSystimeSelector = mkSelector "setMinSystime:"

-- | @Selector@ for @maxSystime@
maxSystimeSelector :: Selector
maxSystimeSelector = mkSelector "maxSystime"

-- | @Selector@ for @setMaxSystime:@
setMaxSystimeSelector :: Selector
setMaxSystimeSelector = mkSelector "setMaxSystime:"

