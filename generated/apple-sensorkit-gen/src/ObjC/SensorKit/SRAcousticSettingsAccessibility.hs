{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsAccessibility@.
module ObjC.SensorKit.SRAcousticSettingsAccessibility
  ( SRAcousticSettingsAccessibility
  , IsSRAcousticSettingsAccessibility(..)
  , leftRightBalance
  , monoAudioEnabled
  , backgroundSounds
  , headphoneAccommodations
  , leftRightBalanceSelector
  , monoAudioEnabledSelector
  , backgroundSoundsSelector
  , headphoneAccommodationsSelector


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
import ObjC.Foundation.Internal.Classes

-- | leftRightBalance
--
-- Audio volume between left and right channels
--
-- ObjC selector: @- leftRightBalance@
leftRightBalance :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO CDouble
leftRightBalance srAcousticSettingsAccessibility  =
    sendMsg srAcousticSettingsAccessibility (mkSelector "leftRightBalance") retCDouble []

-- | monoAudioEnabled
--
-- When in mono mode, audio output is the same audio from both the left and right channels
--
-- ObjC selector: @- monoAudioEnabled@
monoAudioEnabled :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO Bool
monoAudioEnabled srAcousticSettingsAccessibility  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsAccessibility (mkSelector "monoAudioEnabled") retCULong []

-- | backgroundSounds
--
-- Background Sounds Settings
--
-- ObjC selector: @- backgroundSounds@
backgroundSounds :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO (Id SRAcousticSettingsAccessibilityBackgroundSounds)
backgroundSounds srAcousticSettingsAccessibility  =
    sendMsg srAcousticSettingsAccessibility (mkSelector "backgroundSounds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | headphoneAccommodations
--
-- Headphone Accommodations Settings
--
-- ObjC selector: @- headphoneAccommodations@
headphoneAccommodations :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO (Id SRAcousticSettingsAccessibilityHeadphoneAccommodations)
headphoneAccommodations srAcousticSettingsAccessibility  =
    sendMsg srAcousticSettingsAccessibility (mkSelector "headphoneAccommodations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @leftRightBalance@
leftRightBalanceSelector :: Selector
leftRightBalanceSelector = mkSelector "leftRightBalance"

-- | @Selector@ for @monoAudioEnabled@
monoAudioEnabledSelector :: Selector
monoAudioEnabledSelector = mkSelector "monoAudioEnabled"

-- | @Selector@ for @backgroundSounds@
backgroundSoundsSelector :: Selector
backgroundSoundsSelector = mkSelector "backgroundSounds"

-- | @Selector@ for @headphoneAccommodations@
headphoneAccommodationsSelector :: Selector
headphoneAccommodationsSelector = mkSelector "headphoneAccommodations"

