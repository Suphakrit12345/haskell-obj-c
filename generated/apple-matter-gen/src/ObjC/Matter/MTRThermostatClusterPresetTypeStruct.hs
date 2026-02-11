{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterPresetTypeStruct@.
module ObjC.Matter.MTRThermostatClusterPresetTypeStruct
  ( MTRThermostatClusterPresetTypeStruct
  , IsMTRThermostatClusterPresetTypeStruct(..)
  , presetScenario
  , setPresetScenario
  , numberOfPresets
  , setNumberOfPresets
  , presetTypeFeatures
  , setPresetTypeFeatures
  , presetScenarioSelector
  , setPresetScenarioSelector
  , numberOfPresetsSelector
  , setNumberOfPresetsSelector
  , presetTypeFeaturesSelector
  , setPresetTypeFeaturesSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- presetScenario@
presetScenario :: IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct => mtrThermostatClusterPresetTypeStruct -> IO (Id NSNumber)
presetScenario mtrThermostatClusterPresetTypeStruct  =
    sendMsg mtrThermostatClusterPresetTypeStruct (mkSelector "presetScenario") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetScenario:@
setPresetScenario :: (IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct, IsNSNumber value) => mtrThermostatClusterPresetTypeStruct -> value -> IO ()
setPresetScenario mtrThermostatClusterPresetTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetTypeStruct (mkSelector "setPresetScenario:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfPresets@
numberOfPresets :: IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct => mtrThermostatClusterPresetTypeStruct -> IO (Id NSNumber)
numberOfPresets mtrThermostatClusterPresetTypeStruct  =
    sendMsg mtrThermostatClusterPresetTypeStruct (mkSelector "numberOfPresets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNumberOfPresets:@
setNumberOfPresets :: (IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct, IsNSNumber value) => mtrThermostatClusterPresetTypeStruct -> value -> IO ()
setNumberOfPresets mtrThermostatClusterPresetTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetTypeStruct (mkSelector "setNumberOfPresets:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- presetTypeFeatures@
presetTypeFeatures :: IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct => mtrThermostatClusterPresetTypeStruct -> IO (Id NSNumber)
presetTypeFeatures mtrThermostatClusterPresetTypeStruct  =
    sendMsg mtrThermostatClusterPresetTypeStruct (mkSelector "presetTypeFeatures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetTypeFeatures:@
setPresetTypeFeatures :: (IsMTRThermostatClusterPresetTypeStruct mtrThermostatClusterPresetTypeStruct, IsNSNumber value) => mtrThermostatClusterPresetTypeStruct -> value -> IO ()
setPresetTypeFeatures mtrThermostatClusterPresetTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterPresetTypeStruct (mkSelector "setPresetTypeFeatures:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetScenario@
presetScenarioSelector :: Selector
presetScenarioSelector = mkSelector "presetScenario"

-- | @Selector@ for @setPresetScenario:@
setPresetScenarioSelector :: Selector
setPresetScenarioSelector = mkSelector "setPresetScenario:"

-- | @Selector@ for @numberOfPresets@
numberOfPresetsSelector :: Selector
numberOfPresetsSelector = mkSelector "numberOfPresets"

-- | @Selector@ for @setNumberOfPresets:@
setNumberOfPresetsSelector :: Selector
setNumberOfPresetsSelector = mkSelector "setNumberOfPresets:"

-- | @Selector@ for @presetTypeFeatures@
presetTypeFeaturesSelector :: Selector
presetTypeFeaturesSelector = mkSelector "presetTypeFeatures"

-- | @Selector@ for @setPresetTypeFeatures:@
setPresetTypeFeaturesSelector :: Selector
setPresetTypeFeaturesSelector = mkSelector "setPresetTypeFeatures:"

