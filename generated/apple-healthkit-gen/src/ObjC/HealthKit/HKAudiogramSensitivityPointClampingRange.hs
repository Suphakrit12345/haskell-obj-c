{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines the range within which an ear's sensitivity point may have been clamped, if any.
--
-- At times, it may be required to indicate that a sensitivity point has been clamped to a range. These reasons include but are not limited to user safety, hardware limitations, or algorithm features.
--
-- Generated bindings for @HKAudiogramSensitivityPointClampingRange@.
module ObjC.HealthKit.HKAudiogramSensitivityPointClampingRange
  ( HKAudiogramSensitivityPointClampingRange
  , IsHKAudiogramSensitivityPointClampingRange(..)
  , init_
  , clampingRangeWithLowerBound_upperBound_error
  , lowerBound
  , upperBound
  , initSelector
  , clampingRangeWithLowerBound_upperBound_errorSelector
  , lowerBoundSelector
  , upperBoundSelector


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
init_ :: IsHKAudiogramSensitivityPointClampingRange hkAudiogramSensitivityPointClampingRange => hkAudiogramSensitivityPointClampingRange -> IO (Id HKAudiogramSensitivityPointClampingRange)
init_ hkAudiogramSensitivityPointClampingRange  =
    sendMsg hkAudiogramSensitivityPointClampingRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | clampingRangeWithLowerBound:upperBound:error:
--
-- Creates a clamping range from a given lower and upper bound. At least one bound must be specified. If both bounds are provided, the lower bound must be less than the upper bound.
--
-- @lowerBound@ — The lower bound of the clamping range (if any)
--
-- @upperBound@ — The upper bound of the clamping range (if any)
--
-- @errorOut@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a clamping range or nil if there were problems                            creating the instance.  Errors may include not having any bound or lower bound is greater than the upper bound
--
-- ObjC selector: @+ clampingRangeWithLowerBound:upperBound:error:@
clampingRangeWithLowerBound_upperBound_error :: (IsNSNumber lowerBound, IsNSNumber upperBound, IsNSError errorOut) => lowerBound -> upperBound -> errorOut -> IO (Id HKAudiogramSensitivityPointClampingRange)
clampingRangeWithLowerBound_upperBound_error lowerBound upperBound errorOut =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityPointClampingRange"
    withObjCPtr lowerBound $ \raw_lowerBound ->
      withObjCPtr upperBound $ \raw_upperBound ->
        withObjCPtr errorOut $ \raw_errorOut ->
          sendClassMsg cls' (mkSelector "clampingRangeWithLowerBound:upperBound:error:") (retPtr retVoid) [argPtr (castPtr raw_lowerBound :: Ptr ()), argPtr (castPtr raw_upperBound :: Ptr ()), argPtr (castPtr raw_errorOut :: Ptr ())] >>= retainedObject . castPtr

-- | lowerBound
--
-- The lower bound of the clamping range, if any, in dBHL.
--
-- ObjC selector: @- lowerBound@
lowerBound :: IsHKAudiogramSensitivityPointClampingRange hkAudiogramSensitivityPointClampingRange => hkAudiogramSensitivityPointClampingRange -> IO (Id HKQuantity)
lowerBound hkAudiogramSensitivityPointClampingRange  =
    sendMsg hkAudiogramSensitivityPointClampingRange (mkSelector "lowerBound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | upperBound
--
-- The upper bound of the clamping range, if any, in dBHL.
--
-- ObjC selector: @- upperBound@
upperBound :: IsHKAudiogramSensitivityPointClampingRange hkAudiogramSensitivityPointClampingRange => hkAudiogramSensitivityPointClampingRange -> IO (Id HKQuantity)
upperBound hkAudiogramSensitivityPointClampingRange  =
    sendMsg hkAudiogramSensitivityPointClampingRange (mkSelector "upperBound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @clampingRangeWithLowerBound:upperBound:error:@
clampingRangeWithLowerBound_upperBound_errorSelector :: Selector
clampingRangeWithLowerBound_upperBound_errorSelector = mkSelector "clampingRangeWithLowerBound:upperBound:error:"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector
upperBoundSelector = mkSelector "upperBound"

