{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsAccessibilityHeadphoneAccommodations@.
module ObjC.SensorKit.SRAcousticSettingsAccessibilityHeadphoneAccommodations
  ( SRAcousticSettingsAccessibilityHeadphoneAccommodations
  , IsSRAcousticSettingsAccessibilityHeadphoneAccommodations(..)
  , enabled
  , mediaEnhanceTuning
  , mediaEnhanceBoosting
  , mediaEnhanceApplication
  , enabledSelector
  , mediaEnhanceTuningSelector
  , mediaEnhanceBoostingSelector
  , mediaEnhanceApplicationSelector

  -- * Enum types
  , SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication(SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication)
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationNone
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationPhone
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationMedia
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationPhoneAndMedia
  , SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting(SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting)
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoostingSlight
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoostingModerate
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoostingStrong
  , SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning(SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning)
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuningBalancedTone
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuningVocalRange
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuningBrightness

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

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | enabled
--
-- Headphone Accommodations is turned on/off
--
-- ObjC selector: @- enabled@
enabled :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO Bool
enabled srAcousticSettingsAccessibilityHeadphoneAccommodations  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsAccessibilityHeadphoneAccommodations (mkSelector "enabled") retCULong []

-- | mediaEnhanceTuning
--
-- Tune for different range of frequencies
--
-- Optimize for providing audio tuning for different ranges of frequencies.
--
-- ObjC selector: @- mediaEnhanceTuning@
mediaEnhanceTuning :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning
mediaEnhanceTuning srAcousticSettingsAccessibilityHeadphoneAccommodations  =
    fmap (coerce :: CLong -> SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning) $ sendMsg srAcousticSettingsAccessibilityHeadphoneAccommodations (mkSelector "mediaEnhanceTuning") retCLong []

-- | mediaEnhanceBoosting
--
-- Soft Sounds boost level
--
-- Soft sounds will be boosted slightly, moderately, or strongly.
--
-- ObjC selector: @- mediaEnhanceBoosting@
mediaEnhanceBoosting :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting
mediaEnhanceBoosting srAcousticSettingsAccessibilityHeadphoneAccommodations  =
    fmap (coerce :: CLong -> SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting) $ sendMsg srAcousticSettingsAccessibilityHeadphoneAccommodations (mkSelector "mediaEnhanceBoosting") retCLong []

-- | mediaEnhanceApplication
--
-- Headphone Accommodations Application
--
-- Headphone Accommodations Apply to phone, media, or both.
--
-- ObjC selector: @- mediaEnhanceApplication@
mediaEnhanceApplication :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication
mediaEnhanceApplication srAcousticSettingsAccessibilityHeadphoneAccommodations  =
    fmap (coerce :: CLong -> SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication) $ sendMsg srAcousticSettingsAccessibilityHeadphoneAccommodations (mkSelector "mediaEnhanceApplication") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @mediaEnhanceTuning@
mediaEnhanceTuningSelector :: Selector
mediaEnhanceTuningSelector = mkSelector "mediaEnhanceTuning"

-- | @Selector@ for @mediaEnhanceBoosting@
mediaEnhanceBoostingSelector :: Selector
mediaEnhanceBoostingSelector = mkSelector "mediaEnhanceBoosting"

-- | @Selector@ for @mediaEnhanceApplication@
mediaEnhanceApplicationSelector :: Selector
mediaEnhanceApplicationSelector = mkSelector "mediaEnhanceApplication"

