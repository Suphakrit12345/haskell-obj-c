{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKStatisticsCollection@.
module ObjC.HealthKit.HKStatisticsCollection
  ( HKStatisticsCollection
  , IsHKStatisticsCollection(..)
  , init_
  , statisticsForDate
  , enumerateStatisticsFromDate_toDate_withBlock
  , statistics
  , sources
  , initSelector
  , statisticsForDateSelector
  , enumerateStatisticsFromDate_toDate_withBlockSelector
  , statisticsSelector
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
init_ :: IsHKStatisticsCollection hkStatisticsCollection => hkStatisticsCollection -> IO (Id HKStatisticsCollection)
init_ hkStatisticsCollection  =
    sendMsg hkStatisticsCollection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | statisticsForDate:
--
-- Returns the statistics object that this date is inside of
--
-- If there are no samples for the given date, an HKStatistics instance with nil quantities will be returned.
--
-- ObjC selector: @- statisticsForDate:@
statisticsForDate :: (IsHKStatisticsCollection hkStatisticsCollection, IsNSDate date) => hkStatisticsCollection -> date -> IO (Id HKStatistics)
statisticsForDate hkStatisticsCollection  date =
  withObjCPtr date $ \raw_date ->
      sendMsg hkStatisticsCollection (mkSelector "statisticsForDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | enumerateStatisticsFromDate:toDate:withBlock:
--
-- Enumerates all statistics objects from startDate to endDate.
--
-- Statistics objects will be enumerated in chronological order. If there are no samples for an interval                between the start and end date, then the HKStatistics object for that interval will have nil quantities.
--
-- ObjC selector: @- enumerateStatisticsFromDate:toDate:withBlock:@
enumerateStatisticsFromDate_toDate_withBlock :: (IsHKStatisticsCollection hkStatisticsCollection, IsNSDate startDate, IsNSDate endDate) => hkStatisticsCollection -> startDate -> endDate -> Ptr () -> IO ()
enumerateStatisticsFromDate_toDate_withBlock hkStatisticsCollection  startDate endDate block =
  withObjCPtr startDate $ \raw_startDate ->
    withObjCPtr endDate $ \raw_endDate ->
        sendMsg hkStatisticsCollection (mkSelector "enumerateStatisticsFromDate:toDate:withBlock:") retVoid [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | statistics
--
-- Returns a copy of the populated statistics objects.
--
-- The statistics objects are ordered chronologically.
--
-- ObjC selector: @- statistics@
statistics :: IsHKStatisticsCollection hkStatisticsCollection => hkStatisticsCollection -> IO (Id NSArray)
statistics hkStatisticsCollection  =
    sendMsg hkStatisticsCollection (mkSelector "statistics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sources
--
-- Returns all HKSources found in the contained HKStatistics objects.
--
-- Sources will be empty unless HKStatisticsOptionSeparateBySource is specified in the                HKStatisticsCollectionQuery options.
--
-- ObjC selector: @- sources@
sources :: IsHKStatisticsCollection hkStatisticsCollection => hkStatisticsCollection -> IO (Id NSSet)
sources hkStatisticsCollection  =
    sendMsg hkStatisticsCollection (mkSelector "sources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @statisticsForDate:@
statisticsForDateSelector :: Selector
statisticsForDateSelector = mkSelector "statisticsForDate:"

-- | @Selector@ for @enumerateStatisticsFromDate:toDate:withBlock:@
enumerateStatisticsFromDate_toDate_withBlockSelector :: Selector
enumerateStatisticsFromDate_toDate_withBlockSelector = mkSelector "enumerateStatisticsFromDate:toDate:withBlock:"

-- | @Selector@ for @statistics@
statisticsSelector :: Selector
statisticsSelector = mkSelector "statistics"

-- | @Selector@ for @sources@
sourcesSelector :: Selector
sourcesSelector = mkSelector "sources"

