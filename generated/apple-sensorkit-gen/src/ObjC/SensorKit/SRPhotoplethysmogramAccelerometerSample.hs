{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRPhotoplethysmogramAccelerometerSample@.
module ObjC.SensorKit.SRPhotoplethysmogramAccelerometerSample
  ( SRPhotoplethysmogramAccelerometerSample
  , IsSRPhotoplethysmogramAccelerometerSample(..)
  , init_
  , new
  , nanosecondsSinceStart
  , samplingFrequency
  , x
  , y
  , z
  , initSelector
  , newSelector
  , nanosecondsSinceStartSelector
  , samplingFrequencySelector
  , xSelector
  , ySelector
  , zSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id SRPhotoplethysmogramAccelerometerSample)
init_ srPhotoplethysmogramAccelerometerSample  =
    sendMsg srPhotoplethysmogramAccelerometerSample (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRPhotoplethysmogramAccelerometerSample)
new  =
  do
    cls' <- getRequiredClass "SRPhotoplethysmogramAccelerometerSample"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | nanosecondsSinceStart
--
-- nanoseconds since the @SRPhotoplethysmogramSample@ start date of the specific  accelerometer sample
--
-- ObjC selector: @- nanosecondsSinceStart@
nanosecondsSinceStart :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO CLong
nanosecondsSinceStart srPhotoplethysmogramAccelerometerSample  =
    sendMsg srPhotoplethysmogramAccelerometerSample (mkSelector "nanosecondsSinceStart") retCLong []

-- | samplingFrequency
--
-- Sampling frequency of accelerometer data in Hz
--
-- ObjC selector: @- samplingFrequency@
samplingFrequency :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
samplingFrequency srPhotoplethysmogramAccelerometerSample  =
    sendMsg srPhotoplethysmogramAccelerometerSample (mkSelector "samplingFrequency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | x
--
-- X-axis acceleration in G's
--
-- ObjC selector: @- x@
x :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
x srPhotoplethysmogramAccelerometerSample  =
    sendMsg srPhotoplethysmogramAccelerometerSample (mkSelector "x") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | y
--
-- Y-axis acceleration in G's
--
-- ObjC selector: @- y@
y :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
y srPhotoplethysmogramAccelerometerSample  =
    sendMsg srPhotoplethysmogramAccelerometerSample (mkSelector "y") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | z
--
-- Z-axis acceleration in G's
--
-- ObjC selector: @- z@
z :: IsSRPhotoplethysmogramAccelerometerSample srPhotoplethysmogramAccelerometerSample => srPhotoplethysmogramAccelerometerSample -> IO (Id NSMeasurement)
z srPhotoplethysmogramAccelerometerSample  =
    sendMsg srPhotoplethysmogramAccelerometerSample (mkSelector "z") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @nanosecondsSinceStart@
nanosecondsSinceStartSelector :: Selector
nanosecondsSinceStartSelector = mkSelector "nanosecondsSinceStart"

-- | @Selector@ for @samplingFrequency@
samplingFrequencySelector :: Selector
samplingFrequencySelector = mkSelector "samplingFrequency"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @z@
zSelector :: Selector
zSelector = mkSelector "z"

