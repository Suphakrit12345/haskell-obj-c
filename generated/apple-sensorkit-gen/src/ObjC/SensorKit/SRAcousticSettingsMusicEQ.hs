{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsMusicEQ@.
module ObjC.SensorKit.SRAcousticSettingsMusicEQ
  ( SRAcousticSettingsMusicEQ
  , IsSRAcousticSettingsMusicEQ(..)
  , soundCheckEnabled
  , lateNightModeEnabled
  , soundCheckEnabledSelector
  , lateNightModeEnabledSelector


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

-- | soundCheckEnabled
--
-- Sound Check is turned on/off
--
-- ObjC selector: @- soundCheckEnabled@
soundCheckEnabled :: IsSRAcousticSettingsMusicEQ srAcousticSettingsMusicEQ => srAcousticSettingsMusicEQ -> IO Bool
soundCheckEnabled srAcousticSettingsMusicEQ  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsMusicEQ (mkSelector "soundCheckEnabled") retCULong []

-- | lateNightModeEnabled
--
-- Late Night Mode is turned on/off
--
-- Music EQ Setting to dynamically compress system level audio
--
-- ObjC selector: @- lateNightModeEnabled@
lateNightModeEnabled :: IsSRAcousticSettingsMusicEQ srAcousticSettingsMusicEQ => srAcousticSettingsMusicEQ -> IO Bool
lateNightModeEnabled srAcousticSettingsMusicEQ  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg srAcousticSettingsMusicEQ (mkSelector "lateNightModeEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @soundCheckEnabled@
soundCheckEnabledSelector :: Selector
soundCheckEnabledSelector = mkSelector "soundCheckEnabled"

-- | @Selector@ for @lateNightModeEnabled@
lateNightModeEnabledSelector :: Selector
lateNightModeEnabledSelector = mkSelector "lateNightModeEnabled"

