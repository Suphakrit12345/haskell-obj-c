{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct(..)
  , presetID
  , setPresetID
  , name
  , setName
  , settings
  , setSettings
  , presetIDSelector
  , setPresetIDSelector
  , nameSelector
  , setNameSelector
  , settingsSelector
  , setSettingsSelector


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

-- | @- presetID@
presetID :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> IO (Id NSNumber)
presetID mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct (mkSelector "presetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPresetID:@
setPresetID :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> value -> IO ()
setPresetID mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct (mkSelector "setPresetID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> IO (Id NSString)
name mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct, IsNSString value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> value -> IO ()
setName mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- settings@
settings :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> IO (Id MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct)
settings mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct (mkSelector "settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSettings:@
setSettings :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct -> value -> IO ()
setSettings mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZPresetStruct (mkSelector "setSettings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetID@
presetIDSelector :: Selector
presetIDSelector = mkSelector "presetID"

-- | @Selector@ for @setPresetID:@
setPresetIDSelector :: Selector
setPresetIDSelector = mkSelector "setPresetID:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @settings@
settingsSelector :: Selector
settingsSelector = mkSelector "settings"

-- | @Selector@ for @setSettings:@
setSettingsSelector :: Selector
setSettingsSelector = mkSelector "setSettings:"

