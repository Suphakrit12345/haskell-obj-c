{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKActivitySummary
--
-- An object that represents a summary of a user's activity for a given day.
--
-- Generated bindings for @HKActivitySummary@.
module ObjC.HealthKit.HKActivitySummary
  ( HKActivitySummary
  , IsHKActivitySummary(..)
  , dateComponentsForCalendar
  , activityMoveMode
  , setActivityMoveMode
  , paused
  , setPaused
  , activeEnergyBurned
  , setActiveEnergyBurned
  , appleMoveTime
  , setAppleMoveTime
  , appleExerciseTime
  , setAppleExerciseTime
  , appleStandHours
  , setAppleStandHours
  , activeEnergyBurnedGoal
  , setActiveEnergyBurnedGoal
  , appleMoveTimeGoal
  , setAppleMoveTimeGoal
  , appleExerciseTimeGoal
  , setAppleExerciseTimeGoal
  , exerciseTimeGoal
  , setExerciseTimeGoal
  , appleStandHoursGoal
  , setAppleStandHoursGoal
  , standHoursGoal
  , setStandHoursGoal
  , dateComponentsForCalendarSelector
  , activityMoveModeSelector
  , setActivityMoveModeSelector
  , pausedSelector
  , setPausedSelector
  , activeEnergyBurnedSelector
  , setActiveEnergyBurnedSelector
  , appleMoveTimeSelector
  , setAppleMoveTimeSelector
  , appleExerciseTimeSelector
  , setAppleExerciseTimeSelector
  , appleStandHoursSelector
  , setAppleStandHoursSelector
  , activeEnergyBurnedGoalSelector
  , setActiveEnergyBurnedGoalSelector
  , appleMoveTimeGoalSelector
  , setAppleMoveTimeGoalSelector
  , appleExerciseTimeGoalSelector
  , setAppleExerciseTimeGoalSelector
  , exerciseTimeGoalSelector
  , setExerciseTimeGoalSelector
  , appleStandHoursGoalSelector
  , setAppleStandHoursGoalSelector
  , standHoursGoalSelector
  , setStandHoursGoalSelector

  -- * Enum types
  , HKActivityMoveMode(HKActivityMoveMode)
  , pattern HKActivityMoveModeActiveEnergy
  , pattern HKActivityMoveModeAppleMoveTime

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

-- | dateComponentsForCalendar:
--
-- The date components representing the day for this particular HKActivitySummary.
--
-- These date components will contain era, year, month, and day components in the provided calendar.
--
-- ObjC selector: @- dateComponentsForCalendar:@
dateComponentsForCalendar :: (IsHKActivitySummary hkActivitySummary, IsNSCalendar calendar) => hkActivitySummary -> calendar -> IO (Id NSDateComponents)
dateComponentsForCalendar hkActivitySummary  calendar =
  withObjCPtr calendar $ \raw_calendar ->
      sendMsg hkActivitySummary (mkSelector "dateComponentsForCalendar:") (retPtr retVoid) [argPtr (castPtr raw_calendar :: Ptr ())] >>= retainedObject . castPtr

-- | activityMoveMode
--
-- The move mode of this activity summary
--
-- The move mode of an activity summary determines if activeEnergyBurned or appleMoveTime are used for the move ring.
--
-- ObjC selector: @- activityMoveMode@
activityMoveMode :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO HKActivityMoveMode
activityMoveMode hkActivitySummary  =
    fmap (coerce :: CLong -> HKActivityMoveMode) $ sendMsg hkActivitySummary (mkSelector "activityMoveMode") retCLong []

-- | activityMoveMode
--
-- The move mode of this activity summary
--
-- The move mode of an activity summary determines if activeEnergyBurned or appleMoveTime are used for the move ring.
--
-- ObjC selector: @- setActivityMoveMode:@
setActivityMoveMode :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> HKActivityMoveMode -> IO ()
setActivityMoveMode hkActivitySummary  value =
    sendMsg hkActivitySummary (mkSelector "setActivityMoveMode:") retVoid [argCLong (coerce value)]

-- | paused
--
-- The paused state of this activity summary
--
-- The paused state of an activity summary indicates if the user is tracking their rings for the given day.
--
-- ObjC selector: @- paused@
paused :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO Bool
paused hkActivitySummary  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkActivitySummary (mkSelector "paused") retCULong []

-- | paused
--
-- The paused state of this activity summary
--
-- The paused state of an activity summary indicates if the user is tracking their rings for the given day.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> Bool -> IO ()
setPaused hkActivitySummary  value =
    sendMsg hkActivitySummary (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- | activeEnergyBurned
--
-- The amount of active energy that the user burned.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- activeEnergyBurned@
activeEnergyBurned :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
activeEnergyBurned hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "activeEnergyBurned") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activeEnergyBurned
--
-- The amount of active energy that the user burned.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- setActiveEnergyBurned:@
setActiveEnergyBurned :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setActiveEnergyBurned hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setActiveEnergyBurned:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appleMoveTime
--
-- The amount of move time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 move time time is defined by Apple.
--
-- ObjC selector: @- appleMoveTime@
appleMoveTime :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleMoveTime hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "appleMoveTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appleMoveTime
--
-- The amount of move time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 move time time is defined by Apple.
--
-- ObjC selector: @- setAppleMoveTime:@
setAppleMoveTime :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleMoveTime hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setAppleMoveTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appleExerciseTime
--
-- The amount of exercise time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 exercise time is defined by Apple.
--
-- ObjC selector: @- appleExerciseTime@
appleExerciseTime :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleExerciseTime hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "appleExerciseTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appleExerciseTime
--
-- The amount of exercise time that the user performed.
--
-- This quantity is compatible with time units. The measurement criteria of                 exercise time is defined by Apple.
--
-- ObjC selector: @- setAppleExerciseTime:@
setAppleExerciseTime :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleExerciseTime hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setAppleExerciseTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appleStandHours
--
-- The number of stand hours that the user earned.
--
-- This quantity is compatible with the count unit. The measurement criteria of                 stand hours is defined by Apple.
--
-- ObjC selector: @- appleStandHours@
appleStandHours :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleStandHours hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "appleStandHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appleStandHours
--
-- The number of stand hours that the user earned.
--
-- This quantity is compatible with the count unit. The measurement criteria of                 stand hours is defined by Apple.
--
-- ObjC selector: @- setAppleStandHours:@
setAppleStandHours :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleStandHours hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setAppleStandHours:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | activeEnergyBurnedGoal
--
-- The user's active energy goal for the day.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- activeEnergyBurnedGoal@
activeEnergyBurnedGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
activeEnergyBurnedGoal hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "activeEnergyBurnedGoal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activeEnergyBurnedGoal
--
-- The user's active energy goal for the day.
--
-- This quantity is compatible with energy units.
--
-- ObjC selector: @- setActiveEnergyBurnedGoal:@
setActiveEnergyBurnedGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setActiveEnergyBurnedGoal hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setActiveEnergyBurnedGoal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appleMoveTimeGoal
--
-- The user's move time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- appleMoveTimeGoal@
appleMoveTimeGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleMoveTimeGoal hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "appleMoveTimeGoal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appleMoveTimeGoal
--
-- The user's move time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- setAppleMoveTimeGoal:@
setAppleMoveTimeGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleMoveTimeGoal hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setAppleMoveTimeGoal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appleExerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- appleExerciseTimeGoal@
appleExerciseTimeGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleExerciseTimeGoal hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "appleExerciseTimeGoal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appleExerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- setAppleExerciseTimeGoal:@
setAppleExerciseTimeGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleExerciseTimeGoal hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setAppleExerciseTimeGoal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | exerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- exerciseTimeGoal@
exerciseTimeGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
exerciseTimeGoal hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "exerciseTimeGoal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exerciseTimeGoal
--
-- The user's exercise time goal for the day.
--
-- This quantity is compatible with time units.
--
-- ObjC selector: @- setExerciseTimeGoal:@
setExerciseTimeGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setExerciseTimeGoal hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setExerciseTimeGoal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appleStandHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- appleStandHoursGoal@
appleStandHoursGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
appleStandHoursGoal hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "appleStandHoursGoal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appleStandHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- setAppleStandHoursGoal:@
setAppleStandHoursGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setAppleStandHoursGoal hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setAppleStandHoursGoal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | standHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- standHoursGoal@
standHoursGoal :: IsHKActivitySummary hkActivitySummary => hkActivitySummary -> IO (Id HKQuantity)
standHoursGoal hkActivitySummary  =
    sendMsg hkActivitySummary (mkSelector "standHoursGoal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | standHoursGoal
--
-- The user's active stand hours goal for the day.
--
-- This quantity is compatible with the count unit.
--
-- ObjC selector: @- setStandHoursGoal:@
setStandHoursGoal :: (IsHKActivitySummary hkActivitySummary, IsHKQuantity value) => hkActivitySummary -> value -> IO ()
setStandHoursGoal hkActivitySummary  value =
  withObjCPtr value $ \raw_value ->
      sendMsg hkActivitySummary (mkSelector "setStandHoursGoal:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dateComponentsForCalendar:@
dateComponentsForCalendarSelector :: Selector
dateComponentsForCalendarSelector = mkSelector "dateComponentsForCalendar:"

-- | @Selector@ for @activityMoveMode@
activityMoveModeSelector :: Selector
activityMoveModeSelector = mkSelector "activityMoveMode"

-- | @Selector@ for @setActivityMoveMode:@
setActivityMoveModeSelector :: Selector
setActivityMoveModeSelector = mkSelector "setActivityMoveMode:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @activeEnergyBurned@
activeEnergyBurnedSelector :: Selector
activeEnergyBurnedSelector = mkSelector "activeEnergyBurned"

-- | @Selector@ for @setActiveEnergyBurned:@
setActiveEnergyBurnedSelector :: Selector
setActiveEnergyBurnedSelector = mkSelector "setActiveEnergyBurned:"

-- | @Selector@ for @appleMoveTime@
appleMoveTimeSelector :: Selector
appleMoveTimeSelector = mkSelector "appleMoveTime"

-- | @Selector@ for @setAppleMoveTime:@
setAppleMoveTimeSelector :: Selector
setAppleMoveTimeSelector = mkSelector "setAppleMoveTime:"

-- | @Selector@ for @appleExerciseTime@
appleExerciseTimeSelector :: Selector
appleExerciseTimeSelector = mkSelector "appleExerciseTime"

-- | @Selector@ for @setAppleExerciseTime:@
setAppleExerciseTimeSelector :: Selector
setAppleExerciseTimeSelector = mkSelector "setAppleExerciseTime:"

-- | @Selector@ for @appleStandHours@
appleStandHoursSelector :: Selector
appleStandHoursSelector = mkSelector "appleStandHours"

-- | @Selector@ for @setAppleStandHours:@
setAppleStandHoursSelector :: Selector
setAppleStandHoursSelector = mkSelector "setAppleStandHours:"

-- | @Selector@ for @activeEnergyBurnedGoal@
activeEnergyBurnedGoalSelector :: Selector
activeEnergyBurnedGoalSelector = mkSelector "activeEnergyBurnedGoal"

-- | @Selector@ for @setActiveEnergyBurnedGoal:@
setActiveEnergyBurnedGoalSelector :: Selector
setActiveEnergyBurnedGoalSelector = mkSelector "setActiveEnergyBurnedGoal:"

-- | @Selector@ for @appleMoveTimeGoal@
appleMoveTimeGoalSelector :: Selector
appleMoveTimeGoalSelector = mkSelector "appleMoveTimeGoal"

-- | @Selector@ for @setAppleMoveTimeGoal:@
setAppleMoveTimeGoalSelector :: Selector
setAppleMoveTimeGoalSelector = mkSelector "setAppleMoveTimeGoal:"

-- | @Selector@ for @appleExerciseTimeGoal@
appleExerciseTimeGoalSelector :: Selector
appleExerciseTimeGoalSelector = mkSelector "appleExerciseTimeGoal"

-- | @Selector@ for @setAppleExerciseTimeGoal:@
setAppleExerciseTimeGoalSelector :: Selector
setAppleExerciseTimeGoalSelector = mkSelector "setAppleExerciseTimeGoal:"

-- | @Selector@ for @exerciseTimeGoal@
exerciseTimeGoalSelector :: Selector
exerciseTimeGoalSelector = mkSelector "exerciseTimeGoal"

-- | @Selector@ for @setExerciseTimeGoal:@
setExerciseTimeGoalSelector :: Selector
setExerciseTimeGoalSelector = mkSelector "setExerciseTimeGoal:"

-- | @Selector@ for @appleStandHoursGoal@
appleStandHoursGoalSelector :: Selector
appleStandHoursGoalSelector = mkSelector "appleStandHoursGoal"

-- | @Selector@ for @setAppleStandHoursGoal:@
setAppleStandHoursGoalSelector :: Selector
setAppleStandHoursGoalSelector = mkSelector "setAppleStandHoursGoal:"

-- | @Selector@ for @standHoursGoal@
standHoursGoalSelector :: Selector
standHoursGoalSelector = mkSelector "standHoursGoal"

-- | @Selector@ for @setStandHoursGoal:@
setStandHoursGoalSelector :: Selector
setStandHoursGoalSelector = mkSelector "setStandHoursGoal:"

