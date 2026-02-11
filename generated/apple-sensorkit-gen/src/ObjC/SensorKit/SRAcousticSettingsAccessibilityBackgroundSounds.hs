{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsAccessibilityBackgroundSounds@.
module ObjC.SensorKit.SRAcousticSettingsAccessibilityBackgroundSounds
  ( SRAcousticSettingsAccessibilityBackgroundSounds
  , IsSRAcousticSettingsAccessibilityBackgroundSounds(..)
  , enabled
  , soundName
  , relativeVolume
  , playWithMediaEnabled
  , relativeVolumeWithMedia
  , stopOnLockEnabled
  , enabledSelector
  , soundNameSelector
  , relativeVolumeSelector
  , playWithMediaEnabledSelector
  , relativeVolumeWithMediaSelector
  , stopOnLockEnabledSelector

  -- * Enum types
  , SRAcousticSettingsAccessibilityBackgroundSoundsName(SRAcousticSettingsAccessibilityBackgroundSoundsName)
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBalancedNoise
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBrightNoise
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameDarkNoise
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameOcean
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameRain
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameStream
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameNight
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameFire
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBabble
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameSteam
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameAirplane
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBoat
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBus
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameTrain
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameRainOnRoof
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameQuietNight

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
-- Background Sounds is turned on/off
--
-- Plays background sounds to mask unwanted environmental noise.
--
-- ObjC selector: @- enabled@
enabled :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO Bool
enabled srAcousticSettingsAccessibilityBackgroundSounds  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsAccessibilityBackgroundSounds (mkSelector "enabled") retCULong []

-- | soundName
--
-- Accessibility Background sounds name
--
-- ObjC selector: @- soundName@
soundName :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO SRAcousticSettingsAccessibilityBackgroundSoundsName
soundName srAcousticSettingsAccessibilityBackgroundSounds  =
    fmap (coerce :: CLong -> SRAcousticSettingsAccessibilityBackgroundSoundsName) $ sendMsg srAcousticSettingsAccessibilityBackgroundSounds (mkSelector "soundName") retCLong []

-- | relativeVolume
--
-- Accessibility Background sounds volume
--
-- Background sounds volume relative to system volume. Units is a percentage.
--
-- ObjC selector: @- relativeVolume@
relativeVolume :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO CDouble
relativeVolume srAcousticSettingsAccessibilityBackgroundSounds  =
    sendMsg srAcousticSettingsAccessibilityBackgroundSounds (mkSelector "relativeVolume") retCDouble []

-- | playWithMediaEnabled
--
-- Background sounds is to be played while media is also playing
--
-- ObjC selector: @- playWithMediaEnabled@
playWithMediaEnabled :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO Bool
playWithMediaEnabled srAcousticSettingsAccessibilityBackgroundSounds  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsAccessibilityBackgroundSounds (mkSelector "playWithMediaEnabled") retCULong []

-- | relativeVolumeWithMedia
--
-- Accessibility Background sounds volume with media
--
-- Background sounds volume while media is playing. Units is a percentage.
--
-- ObjC selector: @- relativeVolumeWithMedia@
relativeVolumeWithMedia :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO CDouble
relativeVolumeWithMedia srAcousticSettingsAccessibilityBackgroundSounds  =
    sendMsg srAcousticSettingsAccessibilityBackgroundSounds (mkSelector "relativeVolumeWithMedia") retCDouble []

-- | stopOnLockEnabled
--
-- Stop background sounds when iPhone is locked
--
-- ObjC selector: @- stopOnLockEnabled@
stopOnLockEnabled :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO Bool
stopOnLockEnabled srAcousticSettingsAccessibilityBackgroundSounds  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsAccessibilityBackgroundSounds (mkSelector "stopOnLockEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @soundName@
soundNameSelector :: Selector
soundNameSelector = mkSelector "soundName"

-- | @Selector@ for @relativeVolume@
relativeVolumeSelector :: Selector
relativeVolumeSelector = mkSelector "relativeVolume"

-- | @Selector@ for @playWithMediaEnabled@
playWithMediaEnabledSelector :: Selector
playWithMediaEnabledSelector = mkSelector "playWithMediaEnabled"

-- | @Selector@ for @relativeVolumeWithMedia@
relativeVolumeWithMediaSelector :: Selector
relativeVolumeWithMediaSelector = mkSelector "relativeVolumeWithMedia"

-- | @Selector@ for @stopOnLockEnabled@
stopOnLockEnabledSelector :: Selector
stopOnLockEnabledSelector = mkSelector "stopOnLockEnabled"

