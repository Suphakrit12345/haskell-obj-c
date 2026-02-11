{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a calendar date or date range that the data detection system matches.
--
-- The DataDetection framework returns a calendar event match in a @DDMatchCalendarEvent@ object, which has only a beginning date, only an end date, or both a beginning date and an end date.
--
-- Generated bindings for @DDMatchCalendarEvent@.
module ObjC.DataDetection.DDMatchCalendarEvent
  ( DDMatchCalendarEvent
  , IsDDMatchCalendarEvent(..)
  , allDay
  , startDate
  , startTimeZone
  , endDate
  , endTimeZone
  , allDaySelector
  , startDateSelector
  , startTimeZoneSelector
  , endDateSelector
  , endTimeZoneSelector


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

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value that indicates whether the event is an all-day event.
--
-- ObjC selector: @- allDay@
allDay :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO Bool
allDay ddMatchCalendarEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ddMatchCalendarEvent (mkSelector "allDay") retCULong []

-- | A date that represents the start of the event.
--
-- ObjC selector: @- startDate@
startDate :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSDate)
startDate ddMatchCalendarEvent  =
    sendMsg ddMatchCalendarEvent (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The time zone for the event’s start date.
--
-- ObjC selector: @- startTimeZone@
startTimeZone :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSTimeZone)
startTimeZone ddMatchCalendarEvent  =
    sendMsg ddMatchCalendarEvent (mkSelector "startTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A date that represents the end of the event.
--
-- ObjC selector: @- endDate@
endDate :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSDate)
endDate ddMatchCalendarEvent  =
    sendMsg ddMatchCalendarEvent (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The time zone for the event’s end date.
--
-- ObjC selector: @- endTimeZone@
endTimeZone :: IsDDMatchCalendarEvent ddMatchCalendarEvent => ddMatchCalendarEvent -> IO (Id NSTimeZone)
endTimeZone ddMatchCalendarEvent  =
    sendMsg ddMatchCalendarEvent (mkSelector "endTimeZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allDay@
allDaySelector :: Selector
allDaySelector = mkSelector "allDay"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @startTimeZone@
startTimeZoneSelector :: Selector
startTimeZoneSelector = mkSelector "startTimeZone"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @endTimeZone@
endTimeZoneSelector :: Selector
endTimeZoneSelector = mkSelector "endTimeZone"

