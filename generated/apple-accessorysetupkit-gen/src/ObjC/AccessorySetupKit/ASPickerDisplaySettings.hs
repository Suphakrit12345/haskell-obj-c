{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that contains settings to customize the display of the accessory picker
--
-- Generated bindings for @ASPickerDisplaySettings@.
module ObjC.AccessorySetupKit.ASPickerDisplaySettings
  ( ASPickerDisplaySettings
  , IsASPickerDisplaySettings(..)
  , defaultSettings
  , discoveryTimeout
  , setDiscoveryTimeout
  , options
  , setOptions
  , defaultSettingsSelector
  , discoveryTimeoutSelector
  , setDiscoveryTimeoutSelector
  , optionsSelector
  , setOptionsSelector

  -- * Enum types
  , ASPickerDisplaySettingsOptions(ASPickerDisplaySettingsOptions)
  , pattern ASPickerDisplaySettingsOptionFilterDiscoveryResults

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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | An empty settings object.
--
-- ObjC selector: @+ defaultSettings@
defaultSettings :: IO (Id ASPickerDisplaySettings)
defaultSettings  =
  do
    cls' <- getRequiredClass "ASPickerDisplaySettings"
    sendClassMsg cls' (mkSelector "defaultSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Custom timeout for picker. Default is 30 seconds.
--
-- ObjC selector: @- discoveryTimeout@
discoveryTimeout :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> IO CDouble
discoveryTimeout asPickerDisplaySettings  =
    sendMsg asPickerDisplaySettings (mkSelector "discoveryTimeout") retCDouble []

-- | Custom timeout for picker. Default is 30 seconds.
--
-- ObjC selector: @- setDiscoveryTimeout:@
setDiscoveryTimeout :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> CDouble -> IO ()
setDiscoveryTimeout asPickerDisplaySettings  value =
    sendMsg asPickerDisplaySettings (mkSelector "setDiscoveryTimeout:") retVoid [argCDouble value]

-- | Custom options for the picker.
--
-- ObjC selector: @- options@
options :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> IO ASPickerDisplaySettingsOptions
options asPickerDisplaySettings  =
    fmap (coerce :: CULong -> ASPickerDisplaySettingsOptions) $ sendMsg asPickerDisplaySettings (mkSelector "options") retCULong []

-- | Custom options for the picker.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> ASPickerDisplaySettingsOptions -> IO ()
setOptions asPickerDisplaySettings  value =
    sendMsg asPickerDisplaySettings (mkSelector "setOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSettings@
defaultSettingsSelector :: Selector
defaultSettingsSelector = mkSelector "defaultSettings"

-- | @Selector@ for @discoveryTimeout@
discoveryTimeoutSelector :: Selector
discoveryTimeoutSelector = mkSelector "discoveryTimeout"

-- | @Selector@ for @setDiscoveryTimeout:@
setDiscoveryTimeoutSelector :: Selector
setDiscoveryTimeoutSelector = mkSelector "setDiscoveryTimeout:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

