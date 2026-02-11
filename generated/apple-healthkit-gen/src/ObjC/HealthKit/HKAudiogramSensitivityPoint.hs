{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKAudiogramSensitivityPoint@.
module ObjC.HealthKit.HKAudiogramSensitivityPoint
  ( HKAudiogramSensitivityPoint
  , IsHKAudiogramSensitivityPoint(..)
  , sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_error
  , sensitivityPointWithFrequency_tests_error
  , init_
  , frequency
  , leftEarSensitivity
  , rightEarSensitivity
  , tests
  , sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector
  , sensitivityPointWithFrequency_tests_errorSelector
  , initSelector
  , frequencySelector
  , leftEarSensitivitySelector
  , rightEarSensitivitySelector
  , testsSelector


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

-- | sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:
--
-- Creates a point that can be included in a audiogram.
--
-- @frequency@ — Frequency where sensitivity was measured.
--
-- @leftEarSensitivity@ — Left ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- @rightEarSensitivity@ — Right ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- @error@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a sensitivity point or nil if there were problems                            creating the instance.  Errors may include incorrect quantity units                            or data that is out of an expected range.
--
-- ObjC selector: @+ sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:@
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_error :: (IsHKQuantity frequency, IsHKQuantity leftEarSensitivity, IsHKQuantity rightEarSensitivity, IsNSError error_) => frequency -> leftEarSensitivity -> rightEarSensitivity -> error_ -> IO (Id HKAudiogramSensitivityPoint)
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_error frequency leftEarSensitivity rightEarSensitivity error_ =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityPoint"
    withObjCPtr frequency $ \raw_frequency ->
      withObjCPtr leftEarSensitivity $ \raw_leftEarSensitivity ->
        withObjCPtr rightEarSensitivity $ \raw_rightEarSensitivity ->
          withObjCPtr error_ $ \raw_error_ ->
            sendClassMsg cls' (mkSelector "sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:") (retPtr retVoid) [argPtr (castPtr raw_frequency :: Ptr ()), argPtr (castPtr raw_leftEarSensitivity :: Ptr ()), argPtr (castPtr raw_rightEarSensitivity :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | sensitivityPointWithFrequency:tests:error:
--
-- Creates a point that can be included in a audiogram.
--
-- @frequency@ — Frequency at which sensitivity was measured.
--
-- @tests@ — The tests conducted at the frequency
--
-- @errorOut@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a sensitivity point or nil if there were problems                            creating the instance.  Errors may include incorrect quantity units                            or data that is out of an expected range.
--
-- ObjC selector: @+ sensitivityPointWithFrequency:tests:error:@
sensitivityPointWithFrequency_tests_error :: (IsHKQuantity frequency, IsNSArray tests, IsNSError errorOut) => frequency -> tests -> errorOut -> IO (Id HKAudiogramSensitivityPoint)
sensitivityPointWithFrequency_tests_error frequency tests errorOut =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityPoint"
    withObjCPtr frequency $ \raw_frequency ->
      withObjCPtr tests $ \raw_tests ->
        withObjCPtr errorOut $ \raw_errorOut ->
          sendClassMsg cls' (mkSelector "sensitivityPointWithFrequency:tests:error:") (retPtr retVoid) [argPtr (castPtr raw_frequency :: Ptr ()), argPtr (castPtr raw_tests :: Ptr ()), argPtr (castPtr raw_errorOut :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKAudiogramSensitivityPoint)
init_ hkAudiogramSensitivityPoint  =
    sendMsg hkAudiogramSensitivityPoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | frequency  Frequency where sensitivity was measured.  The unit of measurement
--
-- is [HKUnit hertzUnit] or "Hz".
--
-- ObjC selector: @- frequency@
frequency :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKQuantity)
frequency hkAudiogramSensitivityPoint  =
    sendMsg hkAudiogramSensitivityPoint (mkSelector "frequency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sensitivity Left ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- The unit of measurement is @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- ObjC selector: @- leftEarSensitivity@
leftEarSensitivity :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKQuantity)
leftEarSensitivity hkAudiogramSensitivityPoint  =
    sendMsg hkAudiogramSensitivityPoint (mkSelector "leftEarSensitivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sensitivity Right ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- The unit of measurement is @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- ObjC selector: @- rightEarSensitivity@
rightEarSensitivity :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKQuantity)
rightEarSensitivity hkAudiogramSensitivityPoint  =
    sendMsg hkAudiogramSensitivityPoint (mkSelector "rightEarSensitivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tests
--
-- The tests conducted at this frequency
--
-- ObjC selector: @- tests@
tests :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id NSArray)
tests hkAudiogramSensitivityPoint  =
    sendMsg hkAudiogramSensitivityPoint (mkSelector "tests") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:@
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector :: Selector
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector = mkSelector "sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:"

-- | @Selector@ for @sensitivityPointWithFrequency:tests:error:@
sensitivityPointWithFrequency_tests_errorSelector :: Selector
sensitivityPointWithFrequency_tests_errorSelector = mkSelector "sensitivityPointWithFrequency:tests:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @leftEarSensitivity@
leftEarSensitivitySelector :: Selector
leftEarSensitivitySelector = mkSelector "leftEarSensitivity"

-- | @Selector@ for @rightEarSensitivity@
rightEarSensitivitySelector :: Selector
rightEarSensitivitySelector = mkSelector "rightEarSensitivity"

-- | @Selector@ for @tests@
testsSelector :: Selector
testsSelector = mkSelector "tests"

