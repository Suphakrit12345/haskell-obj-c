{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSampleType
--
-- Represents a type of HKSample.
--
-- Generated bindings for @HKSampleType@.
module ObjC.HealthKit.HKSampleType
  ( HKSampleType
  , IsHKSampleType(..)
  , isMaximumDurationRestricted
  , maximumAllowedDuration
  , isMinimumDurationRestricted
  , minimumAllowedDuration
  , allowsRecalibrationForEstimates
  , isMaximumDurationRestrictedSelector
  , maximumAllowedDurationSelector
  , isMinimumDurationRestrictedSelector
  , minimumAllowedDurationSelector
  , allowsRecalibrationForEstimatesSelector


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

-- | isMaximumDurationRestricted
--
-- Returns YES if the start and end date for samples of this type are restricted by a maximum duration.
--
-- ObjC selector: @- isMaximumDurationRestricted@
isMaximumDurationRestricted :: IsHKSampleType hkSampleType => hkSampleType -> IO Bool
isMaximumDurationRestricted hkSampleType  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkSampleType (mkSelector "isMaximumDurationRestricted") retCULong []

-- | maximumAllowedDuration
--
-- When the duration is restricted for samples of this type, returns the maximum duration allowed,                calculated as the difference between end and start dates.
--
-- Throws an exception if there is no maximum restriction on duration for samples of this type.
--
-- ObjC selector: @- maximumAllowedDuration@
maximumAllowedDuration :: IsHKSampleType hkSampleType => hkSampleType -> IO CDouble
maximumAllowedDuration hkSampleType  =
    sendMsg hkSampleType (mkSelector "maximumAllowedDuration") retCDouble []

-- | isMinimumDurationRestricted
--
-- Returns YES if the start and end date for samples of this type are restricted by a minimum duration.
--
-- ObjC selector: @- isMinimumDurationRestricted@
isMinimumDurationRestricted :: IsHKSampleType hkSampleType => hkSampleType -> IO Bool
isMinimumDurationRestricted hkSampleType  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkSampleType (mkSelector "isMinimumDurationRestricted") retCULong []

-- | minimumAllowedDuration
--
-- When the duration is restricted for samples of this type, returns the minimum duration allowed,                calculated as the difference between end and start dates.
--
-- Throws an exception if there is no minimum restriction on duration for samples of this type.
--
-- ObjC selector: @- minimumAllowedDuration@
minimumAllowedDuration :: IsHKSampleType hkSampleType => hkSampleType -> IO CDouble
minimumAllowedDuration hkSampleType  =
    sendMsg hkSampleType (mkSelector "minimumAllowedDuration") retCDouble []

-- | allowsRecalibrationForEstimates
--
-- Returns YES if first-party samples of this type are produced using a prediction algorithm, and that algorithm supports recalibration. To recalibrate the                estimates for a sample type, see -[HKHealthStore recalibrateEstimatesForSampleType:atDate:completion:]
--
-- ObjC selector: @- allowsRecalibrationForEstimates@
allowsRecalibrationForEstimates :: IsHKSampleType hkSampleType => hkSampleType -> IO Bool
allowsRecalibrationForEstimates hkSampleType  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkSampleType (mkSelector "allowsRecalibrationForEstimates") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isMaximumDurationRestricted@
isMaximumDurationRestrictedSelector :: Selector
isMaximumDurationRestrictedSelector = mkSelector "isMaximumDurationRestricted"

-- | @Selector@ for @maximumAllowedDuration@
maximumAllowedDurationSelector :: Selector
maximumAllowedDurationSelector = mkSelector "maximumAllowedDuration"

-- | @Selector@ for @isMinimumDurationRestricted@
isMinimumDurationRestrictedSelector :: Selector
isMinimumDurationRestrictedSelector = mkSelector "isMinimumDurationRestricted"

-- | @Selector@ for @minimumAllowedDuration@
minimumAllowedDurationSelector :: Selector
minimumAllowedDurationSelector = mkSelector "minimumAllowedDuration"

-- | @Selector@ for @allowsRecalibrationForEstimates@
allowsRecalibrationForEstimatesSelector :: Selector
allowsRecalibrationForEstimatesSelector = mkSelector "allowsRecalibrationForEstimates"

