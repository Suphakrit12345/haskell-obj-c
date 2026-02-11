{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKAudiogramSensitivityTest@.
module ObjC.HealthKit.HKAudiogramSensitivityTest
  ( HKAudiogramSensitivityTest
  , IsHKAudiogramSensitivityTest(..)
  , initWithSensitivity_type_masked_side_clampingRange_error
  , init_
  , new
  , sensitivity
  , type_
  , masked
  , side
  , clampingRange
  , initWithSensitivity_type_masked_side_clampingRange_errorSelector
  , initSelector
  , newSelector
  , sensitivitySelector
  , typeSelector
  , maskedSelector
  , sideSelector
  , clampingRangeSelector

  -- * Enum types
  , HKAudiogramConductionType(HKAudiogramConductionType)
  , pattern HKAudiogramConductionTypeAir
  , HKAudiogramSensitivityTestSide(HKAudiogramSensitivityTestSide)
  , pattern HKAudiogramSensitivityTestSideLeft
  , pattern HKAudiogramSensitivityTestSideRight

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

-- | initWithSensitivity:type:masked:side:clampingRange:error:
--
-- Creates a sensitivity test which can be added to a HKAudiogramSensitivityPoint
--
-- @sensitivity@ — The ear sensitivity measured in dB from a baseline of 0 dB with unit @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- @type@ — The type of test
--
-- @masked@ — If the test was conducted with or without masking
--
-- @side@ — The test side which was tested
--
-- @clampingRange@ — The clamping range (if any)
--
-- @errorOut@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a Sensitivity Test or nil if there were problems                            creating the instance.  Errors may include incorrect quantity units or sensitivity out of range
--
-- ObjC selector: @- initWithSensitivity:type:masked:side:clampingRange:error:@
initWithSensitivity_type_masked_side_clampingRange_error :: (IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest, IsHKQuantity sensitivity, IsHKAudiogramSensitivityPointClampingRange clampingRange, IsNSError errorOut) => hkAudiogramSensitivityTest -> sensitivity -> HKAudiogramConductionType -> Bool -> HKAudiogramSensitivityTestSide -> clampingRange -> errorOut -> IO (Id HKAudiogramSensitivityTest)
initWithSensitivity_type_masked_side_clampingRange_error hkAudiogramSensitivityTest  sensitivity type_ masked side clampingRange errorOut =
  withObjCPtr sensitivity $ \raw_sensitivity ->
    withObjCPtr clampingRange $ \raw_clampingRange ->
      withObjCPtr errorOut $ \raw_errorOut ->
          sendMsg hkAudiogramSensitivityTest (mkSelector "initWithSensitivity:type:masked:side:clampingRange:error:") (retPtr retVoid) [argPtr (castPtr raw_sensitivity :: Ptr ()), argCLong (coerce type_), argCULong (if masked then 1 else 0), argCLong (coerce side), argPtr (castPtr raw_clampingRange :: Ptr ()), argPtr (castPtr raw_errorOut :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO (Id HKAudiogramSensitivityTest)
init_ hkAudiogramSensitivityTest  =
    sendMsg hkAudiogramSensitivityTest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id HKAudiogramSensitivityTest)
new  =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityTest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | sensitivity
--
-- Ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB. The unit of measurement is @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- ObjC selector: @- sensitivity@
sensitivity :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO (Id HKQuantity)
sensitivity hkAudiogramSensitivityTest  =
    sendMsg hkAudiogramSensitivityTest (mkSelector "sensitivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | type
--
-- The conduction type
--
-- ObjC selector: @- type@
type_ :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO HKAudiogramConductionType
type_ hkAudiogramSensitivityTest  =
    fmap (coerce :: CLong -> HKAudiogramConductionType) $ sendMsg hkAudiogramSensitivityTest (mkSelector "type") retCLong []

-- | masked
--
-- Indicates if the test was conducted with or without masking
--
-- ObjC selector: @- masked@
masked :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO Bool
masked hkAudiogramSensitivityTest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkAudiogramSensitivityTest (mkSelector "masked") retCULong []

-- | side
--
-- The test side
--
-- ObjC selector: @- side@
side :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO HKAudiogramSensitivityTestSide
side hkAudiogramSensitivityTest  =
    fmap (coerce :: CLong -> HKAudiogramSensitivityTestSide) $ sendMsg hkAudiogramSensitivityTest (mkSelector "side") retCLong []

-- | clampingRange
--
-- If present, indicates that the range within which the sensitivity point should be clamped.
--
-- ObjC selector: @- clampingRange@
clampingRange :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO (Id HKAudiogramSensitivityPointClampingRange)
clampingRange hkAudiogramSensitivityTest  =
    sendMsg hkAudiogramSensitivityTest (mkSelector "clampingRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSensitivity:type:masked:side:clampingRange:error:@
initWithSensitivity_type_masked_side_clampingRange_errorSelector :: Selector
initWithSensitivity_type_masked_side_clampingRange_errorSelector = mkSelector "initWithSensitivity:type:masked:side:clampingRange:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sensitivity@
sensitivitySelector :: Selector
sensitivitySelector = mkSelector "sensitivity"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @masked@
maskedSelector :: Selector
maskedSelector = mkSelector "masked"

-- | @Selector@ for @side@
sideSelector :: Selector
sideSelector = mkSelector "side"

-- | @Selector@ for @clampingRange@
clampingRangeSelector :: Selector
clampingRangeSelector = mkSelector "clampingRange"

