{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWristTemperature@.
module ObjC.SensorKit.SRWristTemperature
  ( SRWristTemperature
  , IsSRWristTemperature(..)
  , init_
  , new
  , timestamp
  , value
  , condition
  , errorEstimate
  , initSelector
  , newSelector
  , timestampSelector
  , valueSelector
  , conditionSelector
  , errorEstimateSelector

  -- * Enum types
  , SRWristTemperatureCondition(SRWristTemperatureCondition)
  , pattern SRWristTemperatureConditionNone
  , pattern SRWristTemperatureConditionOffWrist
  , pattern SRWristTemperatureConditionOnCharger
  , pattern SRWristTemperatureConditionInMotion

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id SRWristTemperature)
init_ srWristTemperature  =
    sendMsg srWristTemperature (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRWristTemperature)
new  =
  do
    cls' <- getRequiredClass "SRWristTemperature"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | timestamp
--
-- Timestamp of when temperature measurement was taken.
--
-- ObjC selector: @- timestamp@
timestamp :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id NSDate)
timestamp srWristTemperature  =
    sendMsg srWristTemperature (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | value
--
-- Temperature sensor value in celsius
--
-- ObjC selector: @- value@
value :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id NSMeasurement)
value srWristTemperature  =
    sendMsg srWristTemperature (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | condition
--
-- Indicate system conditions that may impact the temperature sample.
--
-- ObjC selector: @- condition@
condition :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO SRWristTemperatureCondition
condition srWristTemperature  =
    fmap (coerce :: CULong -> SRWristTemperatureCondition) $ sendMsg srWristTemperature (mkSelector "condition") retCULong []

-- | errorEstimate
--
-- Estimated temperature error per sample.        Error could be in either positive or negative direction.
--
-- ObjC selector: @- errorEstimate@
errorEstimate :: IsSRWristTemperature srWristTemperature => srWristTemperature -> IO (Id NSMeasurement)
errorEstimate srWristTemperature  =
    sendMsg srWristTemperature (mkSelector "errorEstimate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @condition@
conditionSelector :: Selector
conditionSelector = mkSelector "condition"

-- | @Selector@ for @errorEstimate@
errorEstimateSelector :: Selector
errorEstimateSelector = mkSelector "errorEstimate"

