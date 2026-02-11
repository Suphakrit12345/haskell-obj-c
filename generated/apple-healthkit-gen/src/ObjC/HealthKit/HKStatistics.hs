{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKStatistics
--
-- Represents statistics for quantity samples over a period of time.
--
-- Generated bindings for @HKStatistics@.
module ObjC.HealthKit.HKStatistics
  ( HKStatistics
  , IsHKStatistics(..)
  , init_
  , averageQuantityForSource
  , averageQuantity
  , minimumQuantityForSource
  , minimumQuantity
  , maximumQuantityForSource
  , maximumQuantity
  , mostRecentQuantityForSource
  , mostRecentQuantity
  , mostRecentQuantityDateIntervalForSource
  , mostRecentQuantityDateInterval
  , sumQuantityForSource
  , sumQuantity
  , duration
  , durationForSource
  , quantityType
  , startDate
  , endDate
  , sources
  , initSelector
  , averageQuantityForSourceSelector
  , averageQuantitySelector
  , minimumQuantityForSourceSelector
  , minimumQuantitySelector
  , maximumQuantityForSourceSelector
  , maximumQuantitySelector
  , mostRecentQuantityForSourceSelector
  , mostRecentQuantitySelector
  , mostRecentQuantityDateIntervalForSourceSelector
  , mostRecentQuantityDateIntervalSelector
  , sumQuantityForSourceSelector
  , sumQuantitySelector
  , durationSelector
  , durationForSourceSelector
  , quantityTypeSelector
  , startDateSelector
  , endDateSelector
  , sourcesSelector


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

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKStatistics)
init_ hkStatistics  =
    sendMsg hkStatistics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | averageQuantityForSource:
--
-- Returns the average quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- averageQuantityForSource:@
averageQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
averageQuantityForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "averageQuantityForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | averageQuantity
--
-- Returns the average quantity in the time period represented by the receiver.
--
-- ObjC selector: @- averageQuantity@
averageQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
averageQuantity hkStatistics  =
    sendMsg hkStatistics (mkSelector "averageQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | minimumQuantityForSource:
--
-- Returns the minimum quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- minimumQuantityForSource:@
minimumQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
minimumQuantityForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "minimumQuantityForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | minimumQuantity
--
-- Returns the minimum quantity in the time period represented by the receiver.
--
-- ObjC selector: @- minimumQuantity@
minimumQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
minimumQuantity hkStatistics  =
    sendMsg hkStatistics (mkSelector "minimumQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maximumQuantityForSource:
--
-- Returns the maximum quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- maximumQuantityForSource:@
maximumQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
maximumQuantityForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "maximumQuantityForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | maximumQuantity
--
-- Returns the maximum quantity in the time period represented by the receiver.
--
-- ObjC selector: @- maximumQuantity@
maximumQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
maximumQuantity hkStatistics  =
    sendMsg hkStatistics (mkSelector "maximumQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mostRecentQuantityForSource:
--
-- Returns the most recent quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- mostRecentQuantityForSource:@
mostRecentQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
mostRecentQuantityForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "mostRecentQuantityForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | mostRecentQuantity
--
-- Returns the most recent quantity in the time period represented by the receiver.
--
-- ObjC selector: @- mostRecentQuantity@
mostRecentQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
mostRecentQuantity hkStatistics  =
    sendMsg hkStatistics (mkSelector "mostRecentQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mostRecentQuantityDateIntervalForSource:
--
-- Returns the date interval of the most recent quantity for the given source in the time period                represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- mostRecentQuantityDateIntervalForSource:@
mostRecentQuantityDateIntervalForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id NSDateInterval)
mostRecentQuantityDateIntervalForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "mostRecentQuantityDateIntervalForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | mostRecentQuantityDateInterval
--
-- Returns the date interval of the most recent quantity in the time period represented by the receiver.
--
-- ObjC selector: @- mostRecentQuantityDateInterval@
mostRecentQuantityDateInterval :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSDateInterval)
mostRecentQuantityDateInterval hkStatistics  =
    sendMsg hkStatistics (mkSelector "mostRecentQuantityDateInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sumQuantityForSource:
--
-- Returns the sum quantity for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- sumQuantityForSource:@
sumQuantityForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
sumQuantityForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "sumQuantityForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | sumQuantity
--
-- Returns the sum of quantities in the time period represented by the receiver.
--
-- ObjC selector: @- sumQuantity@
sumQuantity :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
sumQuantity hkStatistics  =
    sendMsg hkStatistics (mkSelector "sumQuantity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Total duration (in seconds) covered by the samples represented by these statistics. Only present if HKStatisticsOptionDuration is is specified.
--
-- duration
--
-- Total duration, as a time-unit compatible quantity, covered by the samples represented by these statistics.
--
-- Only present if HKStatisticsOptionDuration is is specified.
--
-- ObjC selector: @- duration@
duration :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantity)
duration hkStatistics  =
    sendMsg hkStatistics (mkSelector "duration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | durationForSource:
--
-- Returns the duration, as a time-unit compatible quantity, for the given source in the time period represented by the receiver.
--
-- If HKStatisticsOptionSeparateBySource is not specified, then this will always be nil.
--
-- ObjC selector: @- durationForSource:@
durationForSource :: (IsHKStatistics hkStatistics, IsHKSource source) => hkStatistics -> source -> IO (Id HKQuantity)
durationForSource hkStatistics  source =
  withObjCPtr source $ \raw_source ->
      sendMsg hkStatistics (mkSelector "durationForSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | @- quantityType@
quantityType :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id HKQuantityType)
quantityType hkStatistics  =
    sendMsg hkStatistics (mkSelector "quantityType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startDate@
startDate :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSDate)
startDate hkStatistics  =
    sendMsg hkStatistics (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSDate)
endDate hkStatistics  =
    sendMsg hkStatistics (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sources@
sources :: IsHKStatistics hkStatistics => hkStatistics -> IO (Id NSArray)
sources hkStatistics  =
    sendMsg hkStatistics (mkSelector "sources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @averageQuantityForSource:@
averageQuantityForSourceSelector :: Selector
averageQuantityForSourceSelector = mkSelector "averageQuantityForSource:"

-- | @Selector@ for @averageQuantity@
averageQuantitySelector :: Selector
averageQuantitySelector = mkSelector "averageQuantity"

-- | @Selector@ for @minimumQuantityForSource:@
minimumQuantityForSourceSelector :: Selector
minimumQuantityForSourceSelector = mkSelector "minimumQuantityForSource:"

-- | @Selector@ for @minimumQuantity@
minimumQuantitySelector :: Selector
minimumQuantitySelector = mkSelector "minimumQuantity"

-- | @Selector@ for @maximumQuantityForSource:@
maximumQuantityForSourceSelector :: Selector
maximumQuantityForSourceSelector = mkSelector "maximumQuantityForSource:"

-- | @Selector@ for @maximumQuantity@
maximumQuantitySelector :: Selector
maximumQuantitySelector = mkSelector "maximumQuantity"

-- | @Selector@ for @mostRecentQuantityForSource:@
mostRecentQuantityForSourceSelector :: Selector
mostRecentQuantityForSourceSelector = mkSelector "mostRecentQuantityForSource:"

-- | @Selector@ for @mostRecentQuantity@
mostRecentQuantitySelector :: Selector
mostRecentQuantitySelector = mkSelector "mostRecentQuantity"

-- | @Selector@ for @mostRecentQuantityDateIntervalForSource:@
mostRecentQuantityDateIntervalForSourceSelector :: Selector
mostRecentQuantityDateIntervalForSourceSelector = mkSelector "mostRecentQuantityDateIntervalForSource:"

-- | @Selector@ for @mostRecentQuantityDateInterval@
mostRecentQuantityDateIntervalSelector :: Selector
mostRecentQuantityDateIntervalSelector = mkSelector "mostRecentQuantityDateInterval"

-- | @Selector@ for @sumQuantityForSource:@
sumQuantityForSourceSelector :: Selector
sumQuantityForSourceSelector = mkSelector "sumQuantityForSource:"

-- | @Selector@ for @sumQuantity@
sumQuantitySelector :: Selector
sumQuantitySelector = mkSelector "sumQuantity"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @durationForSource:@
durationForSourceSelector :: Selector
durationForSourceSelector = mkSelector "durationForSource:"

-- | @Selector@ for @quantityType@
quantityTypeSelector :: Selector
quantityTypeSelector = mkSelector "quantityType"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @sources@
sourcesSelector :: Selector
sourcesSelector = mkSelector "sources"

