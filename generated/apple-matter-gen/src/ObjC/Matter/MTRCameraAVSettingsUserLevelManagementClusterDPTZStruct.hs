{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterDPTZStruct@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterDPTZStruct
  ( MTRCameraAVSettingsUserLevelManagementClusterDPTZStruct
  , IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct(..)
  , videoStreamID
  , setVideoStreamID
  , viewport
  , setViewport
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , viewportSelector
  , setViewportSelector


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

-- | @- videoStreamID@
videoStreamID :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> IO (Id NSNumber)
videoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> value -> IO ()
setVideoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- viewport@
viewport :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> IO (Id MTRDataTypeViewportStruct)
viewport mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct  =
    sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct (mkSelector "viewport") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setViewport:@
setViewport :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct, IsMTRDataTypeViewportStruct value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> value -> IO ()
setViewport mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct (mkSelector "setViewport:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @viewport@
viewportSelector :: Selector
viewportSelector = mkSelector "viewport"

-- | @Selector@ for @setViewport:@
setViewportSelector :: Selector
setViewportSelector = mkSelector "setViewport:"

