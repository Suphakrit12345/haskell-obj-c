{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRPhotoplethysmogramSample@.
module ObjC.SensorKit.SRPhotoplethysmogramSample
  ( SRPhotoplethysmogramSample
  , IsSRPhotoplethysmogramSample(..)
  , init_
  , new
  , startDate
  , nanosecondsSinceStart
  , usage
  , opticalSamples
  , accelerometerSamples
  , temperature
  , initSelector
  , newSelector
  , startDateSelector
  , nanosecondsSinceStartSelector
  , usageSelector
  , opticalSamplesSelector
  , accelerometerSamplesSelector
  , temperatureSelector


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
init_ :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id SRPhotoplethysmogramSample)
init_ srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRPhotoplethysmogramSample)
new  =
  do
    cls' <- getRequiredClass "SRPhotoplethysmogramSample"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | startDate
--
-- the start date of a data collection session
--
-- ObjC selector: @- startDate@
startDate :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSDate)
startDate srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nanosecondsSinceStart
--
-- nanoseconds since the start date of this specific sample
--
-- ObjC selector: @- nanosecondsSinceStart@
nanosecondsSinceStart :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO CLong
nanosecondsSinceStart srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "nanosecondsSinceStart") retCLong []

-- | usage
--
-- How the sensor was being used during the sample reading
--
-- It is possible for these to occur in combination
--
-- ObjC selector: @- usage@
usage :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSArray)
usage srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "usage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- opticalSamples@
opticalSamples :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSArray)
opticalSamples srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "opticalSamples") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accelerometerSamples@
accelerometerSamples :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSArray)
accelerometerSamples srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "accelerometerSamples") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | temperature
--
-- temperature of the PPG sensors in the watch, measured in celsius
--
-- This may be @nil@ when the sensor data reading is invalid or if is not supported by the hardware
--
-- ObjC selector: @- temperature@
temperature :: IsSRPhotoplethysmogramSample srPhotoplethysmogramSample => srPhotoplethysmogramSample -> IO (Id NSMeasurement)
temperature srPhotoplethysmogramSample  =
    sendMsg srPhotoplethysmogramSample (mkSelector "temperature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @nanosecondsSinceStart@
nanosecondsSinceStartSelector :: Selector
nanosecondsSinceStartSelector = mkSelector "nanosecondsSinceStart"

-- | @Selector@ for @usage@
usageSelector :: Selector
usageSelector = mkSelector "usage"

-- | @Selector@ for @opticalSamples@
opticalSamplesSelector :: Selector
opticalSamplesSelector = mkSelector "opticalSamples"

-- | @Selector@ for @accelerometerSamples@
accelerometerSamplesSelector :: Selector
accelerometerSamplesSelector = mkSelector "accelerometerSamples"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector
temperatureSelector = mkSelector "temperature"

