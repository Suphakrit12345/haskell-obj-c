{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutEvent
--
-- Represents a particular event that occurred during a workout.
--
-- Generated bindings for @HKWorkoutEvent@.
module ObjC.HealthKit.HKWorkoutEvent
  ( HKWorkoutEvent
  , IsHKWorkoutEvent(..)
  , workoutEventWithType_date
  , workoutEventWithType_date_metadata
  , workoutEventWithType_dateInterval_metadata
  , init_
  , type_
  , date
  , dateInterval
  , metadata
  , workoutEventWithType_dateSelector
  , workoutEventWithType_date_metadataSelector
  , workoutEventWithType_dateInterval_metadataSelector
  , initSelector
  , typeSelector
  , dateSelector
  , dateIntervalSelector
  , metadataSelector

  -- * Enum types
  , HKWorkoutEventType(HKWorkoutEventType)
  , pattern HKWorkoutEventTypePause
  , pattern HKWorkoutEventTypeResume
  , pattern HKWorkoutEventTypeLap
  , pattern HKWorkoutEventTypeMarker
  , pattern HKWorkoutEventTypeMotionPaused
  , pattern HKWorkoutEventTypeMotionResumed
  , pattern HKWorkoutEventTypeSegment
  , pattern HKWorkoutEventTypePauseOrResumeRequest

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
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ workoutEventWithType:date:@
workoutEventWithType_date :: IsNSDate date => HKWorkoutEventType -> date -> IO (Id HKWorkoutEvent)
workoutEventWithType_date type_ date =
  do
    cls' <- getRequiredClass "HKWorkoutEvent"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "workoutEventWithType:date:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ workoutEventWithType:date:metadata:@
workoutEventWithType_date_metadata :: (IsNSDate date, IsNSDictionary metadata) => HKWorkoutEventType -> date -> metadata -> IO (Id HKWorkoutEvent)
workoutEventWithType_date_metadata type_ date metadata =
  do
    cls' <- getRequiredClass "HKWorkoutEvent"
    withObjCPtr date $ \raw_date ->
      withObjCPtr metadata $ \raw_metadata ->
        sendClassMsg cls' (mkSelector "workoutEventWithType:date:metadata:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | workoutEventWithType:dateInterval:metadata:
--
-- Creates an event with a date interval with or without a duration.
--
-- @type@ — The type of event to create
--
-- @dateInterval@ — The dateInterval over which the event occurs
--
-- @metadata@ — Dictionary of metadata associated with the event, nullable
--
-- ObjC selector: @+ workoutEventWithType:dateInterval:metadata:@
workoutEventWithType_dateInterval_metadata :: (IsNSDateInterval dateInterval, IsNSDictionary metadata) => HKWorkoutEventType -> dateInterval -> metadata -> IO (Id HKWorkoutEvent)
workoutEventWithType_dateInterval_metadata type_ dateInterval metadata =
  do
    cls' <- getRequiredClass "HKWorkoutEvent"
    withObjCPtr dateInterval $ \raw_dateInterval ->
      withObjCPtr metadata $ \raw_metadata ->
        sendClassMsg cls' (mkSelector "workoutEventWithType:dateInterval:metadata:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_dateInterval :: Ptr ()), argPtr (castPtr raw_metadata :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id HKWorkoutEvent)
init_ hkWorkoutEvent  =
    sendMsg hkWorkoutEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | workoutEventType
--
-- Represents the type of event that occurred during a workout.
--
-- ObjC selector: @- type@
type_ :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO HKWorkoutEventType
type_ hkWorkoutEvent  =
    fmap (coerce :: CLong -> HKWorkoutEventType) $ sendMsg hkWorkoutEvent (mkSelector "type") retCLong []

-- | @- date@
date :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id NSDate)
date hkWorkoutEvent  =
    sendMsg hkWorkoutEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dateInterval
--
-- Date interval representing the time period for which the event is valid.
--
-- Most event types only support date intervals with zero duration. Events of type HKWorkoutEventTypeLap                and HKWorkoutEventTypeSegment are currently the only events that support a nonzero duration.
--
-- ObjC selector: @- dateInterval@
dateInterval :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id NSDateInterval)
dateInterval hkWorkoutEvent  =
    sendMsg hkWorkoutEvent (mkSelector "dateInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metadata
--
-- Extra information describing properties of the receiver.
--
-- Keys must be NSString and values must be either NSString, NSNumber, NSDate, or                HKQuantity. See HKMetadata.h for potential metadata keys and values.
--
-- ObjC selector: @- metadata@
metadata :: IsHKWorkoutEvent hkWorkoutEvent => hkWorkoutEvent -> IO (Id NSDictionary)
metadata hkWorkoutEvent  =
    sendMsg hkWorkoutEvent (mkSelector "metadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workoutEventWithType:date:@
workoutEventWithType_dateSelector :: Selector
workoutEventWithType_dateSelector = mkSelector "workoutEventWithType:date:"

-- | @Selector@ for @workoutEventWithType:date:metadata:@
workoutEventWithType_date_metadataSelector :: Selector
workoutEventWithType_date_metadataSelector = mkSelector "workoutEventWithType:date:metadata:"

-- | @Selector@ for @workoutEventWithType:dateInterval:metadata:@
workoutEventWithType_dateInterval_metadataSelector :: Selector
workoutEventWithType_dateInterval_metadataSelector = mkSelector "workoutEventWithType:dateInterval:metadata:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @dateInterval@
dateIntervalSelector :: Selector
dateIntervalSelector = mkSelector "dateInterval"

-- | @Selector@ for @metadata@
metadataSelector :: Selector
metadataSelector = mkSelector "metadata"

