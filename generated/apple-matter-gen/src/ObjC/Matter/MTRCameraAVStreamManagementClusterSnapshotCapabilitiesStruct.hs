{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct
  ( MTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct
  , IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct(..)
  , resolution
  , setResolution
  , maxFrameRate
  , setMaxFrameRate
  , imageCodec
  , setImageCodec
  , requiresEncodedPixels
  , setRequiresEncodedPixels
  , requiresHardwareEncoder
  , setRequiresHardwareEncoder
  , resolutionSelector
  , setResolutionSelector
  , maxFrameRateSelector
  , setMaxFrameRateSelector
  , imageCodecSelector
  , setImageCodecSelector
  , requiresEncodedPixelsSelector
  , setRequiresEncodedPixelsSelector
  , requiresHardwareEncoderSelector
  , setRequiresHardwareEncoderSelector


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

-- | @- resolution@
resolution :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolution mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "resolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setResolution:@
setResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setResolution mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "setResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "maxFrameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "setMaxFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "imageCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "setImageCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiresEncodedPixels@
requiresEncodedPixels :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
requiresEncodedPixels mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "requiresEncodedPixels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiresEncodedPixels:@
setRequiresEncodedPixels :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setRequiresEncodedPixels mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "setRequiresEncodedPixels:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requiresHardwareEncoder@
requiresHardwareEncoder :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
requiresHardwareEncoder mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "requiresHardwareEncoder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiresHardwareEncoder:@
setRequiresHardwareEncoder :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setRequiresHardwareEncoder mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct (mkSelector "setRequiresHardwareEncoder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resolution@
resolutionSelector :: Selector
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector
setResolutionSelector = mkSelector "setResolution:"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector
maxFrameRateSelector = mkSelector "maxFrameRate"

-- | @Selector@ for @setMaxFrameRate:@
setMaxFrameRateSelector :: Selector
setMaxFrameRateSelector = mkSelector "setMaxFrameRate:"

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @requiresEncodedPixels@
requiresEncodedPixelsSelector :: Selector
requiresEncodedPixelsSelector = mkSelector "requiresEncodedPixels"

-- | @Selector@ for @setRequiresEncodedPixels:@
setRequiresEncodedPixelsSelector :: Selector
setRequiresEncodedPixelsSelector = mkSelector "setRequiresEncodedPixels:"

-- | @Selector@ for @requiresHardwareEncoder@
requiresHardwareEncoderSelector :: Selector
requiresHardwareEncoderSelector = mkSelector "requiresHardwareEncoder"

-- | @Selector@ for @setRequiresHardwareEncoder:@
setRequiresHardwareEncoderSelector :: Selector
setRequiresHardwareEncoderSelector = mkSelector "setRequiresHardwareEncoder:"

