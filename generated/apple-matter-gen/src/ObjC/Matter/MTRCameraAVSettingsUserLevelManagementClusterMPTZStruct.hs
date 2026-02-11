{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct(..)
  , pan
  , setPan
  , tilt
  , setTilt
  , zoom
  , setZoom
  , panSelector
  , setPanSelector
  , tiltSelector
  , setTiltSelector
  , zoomSelector
  , setZoomSelector


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

-- | @- pan@
pan :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> IO (Id NSNumber)
pan mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct (mkSelector "pan") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPan:@
setPan :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> value -> IO ()
setPan mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct (mkSelector "setPan:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tilt@
tilt :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> IO (Id NSNumber)
tilt mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct (mkSelector "tilt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTilt:@
setTilt :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> value -> IO ()
setTilt mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct (mkSelector "setTilt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zoom@
zoom :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> IO (Id NSNumber)
zoom mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct (mkSelector "zoom") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoom:@
setZoom :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> value -> IO ()
setZoom mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct (mkSelector "setZoom:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pan@
panSelector :: Selector
panSelector = mkSelector "pan"

-- | @Selector@ for @setPan:@
setPanSelector :: Selector
setPanSelector = mkSelector "setPan:"

-- | @Selector@ for @tilt@
tiltSelector :: Selector
tiltSelector = mkSelector "tilt"

-- | @Selector@ for @setTilt:@
setTiltSelector :: Selector
setTiltSelector = mkSelector "setTilt:"

-- | @Selector@ for @zoom@
zoomSelector :: Selector
zoomSelector = mkSelector "zoom"

-- | @Selector@ for @setZoom:@
setZoomSelector :: Selector
setZoomSelector = mkSelector "setZoom:"

