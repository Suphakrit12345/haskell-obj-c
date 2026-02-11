{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettings@.
module ObjC.SensorKit.SRAcousticSettings
  ( SRAcousticSettings
  , IsSRAcousticSettings(..)
  , init_
  , new
  , environmentalSoundMeasurementsEnabled
  , audioExposureSampleLifetime
  , headphoneSafetyAudioLevel
  , musicEQSettings
  , accessibilitySettings
  , initSelector
  , newSelector
  , environmentalSoundMeasurementsEnabledSelector
  , audioExposureSampleLifetimeSelector
  , headphoneSafetyAudioLevelSelector
  , musicEQSettingsSelector
  , accessibilitySettingsSelector

  -- * Enum types
  , SRAcousticSettingsSampleLifetime(SRAcousticSettingsSampleLifetime)
  , pattern SRAcousticSettingsSampleLifetimeEightDays
  , pattern SRAcousticSettingsSampleLifetimeUntilUserDeletes

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

-- | @- init@
init_ :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id SRAcousticSettings)
init_ srAcousticSettings  =
    sendMsg srAcousticSettings (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SRAcousticSettings)
new  =
  do
    cls' <- getRequiredClass "SRAcousticSettings"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | environmentalSoundMeasurementsEnabled
--
-- Environmental Sound Measurements
--
-- Setting for Apple Watch Environmental Sound Measurements.
--
-- ObjC selector: @- environmentalSoundMeasurementsEnabled@
environmentalSoundMeasurementsEnabled :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO Bool
environmentalSoundMeasurementsEnabled srAcousticSettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettings (mkSelector "environmentalSoundMeasurementsEnabled") retCULong []

-- | audioExposureSampleLifetime
--
-- Expected lifetime of headphone audio exposure samples in HealthKit
--
-- ObjC selector: @- audioExposureSampleLifetime@
audioExposureSampleLifetime :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO SRAcousticSettingsSampleLifetime
audioExposureSampleLifetime srAcousticSettings  =
    fmap (coerce :: CLong -> SRAcousticSettingsSampleLifetime) $ sendMsg srAcousticSettings (mkSelector "audioExposureSampleLifetime") retCLong []

-- | headphoneSafetyAudioLevel
--
-- Reduce Loud Audio
--
-- iPhone can analyze headphone audio and reduce any sound that is over a set decibel level. A nil value  means the setting is disabled. If the setting is enabled, the property will hold the decibel value  that headphone audio sound volume is not to exceed.
--
-- ObjC selector: @- headphoneSafetyAudioLevel@
headphoneSafetyAudioLevel :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id NSNumber)
headphoneSafetyAudioLevel srAcousticSettings  =
    sendMsg srAcousticSettings (mkSelector "headphoneSafetyAudioLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | musicEQSettings
--
-- Music EQ Settings
--
-- ObjC selector: @- musicEQSettings@
musicEQSettings :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id SRAcousticSettingsMusicEQ)
musicEQSettings srAcousticSettings  =
    sendMsg srAcousticSettings (mkSelector "musicEQSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | accessibilitySettings
--
-- Accessibility Settings
--
-- ObjC selector: @- accessibilitySettings@
accessibilitySettings :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id SRAcousticSettingsAccessibility)
accessibilitySettings srAcousticSettings  =
    sendMsg srAcousticSettings (mkSelector "accessibilitySettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @environmentalSoundMeasurementsEnabled@
environmentalSoundMeasurementsEnabledSelector :: Selector
environmentalSoundMeasurementsEnabledSelector = mkSelector "environmentalSoundMeasurementsEnabled"

-- | @Selector@ for @audioExposureSampleLifetime@
audioExposureSampleLifetimeSelector :: Selector
audioExposureSampleLifetimeSelector = mkSelector "audioExposureSampleLifetime"

-- | @Selector@ for @headphoneSafetyAudioLevel@
headphoneSafetyAudioLevelSelector :: Selector
headphoneSafetyAudioLevelSelector = mkSelector "headphoneSafetyAudioLevel"

-- | @Selector@ for @musicEQSettings@
musicEQSettingsSelector :: Selector
musicEQSettingsSelector = mkSelector "musicEQSettings"

-- | @Selector@ for @accessibilitySettings@
accessibilitySettingsSelector :: Selector
accessibilitySettingsSelector = mkSelector "accessibilitySettings"

