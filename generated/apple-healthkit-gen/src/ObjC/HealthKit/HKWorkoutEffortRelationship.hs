{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKWorkoutEffortRelationship@.
module ObjC.HealthKit.HKWorkoutEffortRelationship
  ( HKWorkoutEffortRelationship
  , IsHKWorkoutEffortRelationship(..)
  , workout
  , activity
  , samples
  , workoutSelector
  , activitySelector
  , samplesSelector


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

-- | workout
--
-- ObjC selector: @- workout@
workout :: IsHKWorkoutEffortRelationship hkWorkoutEffortRelationship => hkWorkoutEffortRelationship -> IO (Id HKWorkout)
workout hkWorkoutEffortRelationship  =
    sendMsg hkWorkoutEffortRelationship (mkSelector "workout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activity
--
-- ObjC selector: @- activity@
activity :: IsHKWorkoutEffortRelationship hkWorkoutEffortRelationship => hkWorkoutEffortRelationship -> IO (Id HKWorkoutActivity)
activity hkWorkoutEffortRelationship  =
    sendMsg hkWorkoutEffortRelationship (mkSelector "activity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | samples
--
-- The samples related to the workout but not any sub-activities
--
-- ObjC selector: @- samples@
samples :: IsHKWorkoutEffortRelationship hkWorkoutEffortRelationship => hkWorkoutEffortRelationship -> IO (Id NSArray)
samples hkWorkoutEffortRelationship  =
    sendMsg hkWorkoutEffortRelationship (mkSelector "samples") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workout@
workoutSelector :: Selector
workoutSelector = mkSelector "workout"

-- | @Selector@ for @activity@
activitySelector :: Selector
activitySelector = mkSelector "activity"

-- | @Selector@ for @samples@
samplesSelector :: Selector
samplesSelector = mkSelector "samples"

