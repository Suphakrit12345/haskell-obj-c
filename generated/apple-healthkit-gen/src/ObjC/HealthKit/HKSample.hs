{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSample
--
-- An abstract class representing measurements taken over a period of time.
--
-- Generated bindings for @HKSample@.
module ObjC.HealthKit.HKSample
  ( HKSample
  , IsHKSample(..)
  , sampleType
  , startDate
  , endDate
  , hasUndeterminedDuration
  , sampleTypeSelector
  , startDateSelector
  , endDateSelector
  , hasUndeterminedDurationSelector


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

-- | @- sampleType@
sampleType :: IsHKSample hkSample => hkSample -> IO (Id HKSampleType)
sampleType hkSample  =
    sendMsg hkSample (mkSelector "sampleType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startDate@
startDate :: IsHKSample hkSample => hkSample -> IO (Id NSDate)
startDate hkSample  =
    sendMsg hkSample (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsHKSample hkSample => hkSample -> IO (Id NSDate)
endDate hkSample  =
    sendMsg hkSample (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hasUndeterminedDuration
--
-- Indicates whether a sample has an undetermined duration.
--
-- Computed based on the endDate of a sample.
--
-- ObjC selector: @- hasUndeterminedDuration@
hasUndeterminedDuration :: IsHKSample hkSample => hkSample -> IO Bool
hasUndeterminedDuration hkSample  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkSample (mkSelector "hasUndeterminedDuration") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sampleType@
sampleTypeSelector :: Selector
sampleTypeSelector = mkSelector "sampleType"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @hasUndeterminedDuration@
hasUndeterminedDurationSelector :: Selector
hasUndeterminedDurationSelector = mkSelector "hasUndeterminedDuration"

