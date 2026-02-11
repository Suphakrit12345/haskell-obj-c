{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamStruct
  ( MTRCameraAVStreamManagementClusterSnapshotStreamStruct
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct(..)
  , snapshotStreamID
  , setSnapshotStreamID
  , imageCodec
  , setImageCodec
  , frameRate
  , setFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , quality
  , setQuality
  , referenceCount
  , setReferenceCount
  , encodedPixels
  , setEncodedPixels
  , hardwareEncoder
  , setHardwareEncoder
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , snapshotStreamIDSelector
  , setSnapshotStreamIDSelector
  , imageCodecSelector
  , setImageCodecSelector
  , frameRateSelector
  , setFrameRateSelector
  , minResolutionSelector
  , setMinResolutionSelector
  , maxResolutionSelector
  , setMaxResolutionSelector
  , qualitySelector
  , setQualitySelector
  , referenceCountSelector
  , setReferenceCountSelector
  , encodedPixelsSelector
  , setEncodedPixelsSelector
  , hardwareEncoderSelector
  , setHardwareEncoderSelector
  , watermarkEnabledSelector
  , setWatermarkEnabledSelector
  , osdEnabledSelector
  , setOsdEnabledSelector


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

-- | @- snapshotStreamID@
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "snapshotStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setSnapshotStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "imageCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setImageCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- frameRate@
frameRate :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
frameRate mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "frameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrameRate:@
setFrameRate :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setFrameRate mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "minResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setMinResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "maxResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setMaxResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- quality@
quality :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
quality mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "quality") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuality:@
setQuality :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setQuality mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setQuality:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referenceCount@
referenceCount :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
referenceCount mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "referenceCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setReferenceCount mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setReferenceCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- encodedPixels@
encodedPixels :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
encodedPixels mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "encodedPixels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncodedPixels:@
setEncodedPixels :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setEncodedPixels mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setEncodedPixels:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hardwareEncoder@
hardwareEncoder :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
hardwareEncoder mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "hardwareEncoder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHardwareEncoder:@
setHardwareEncoder :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setHardwareEncoder mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setHardwareEncoder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "watermarkEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setWatermarkEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "osdEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterSnapshotStreamStruct (mkSelector "setOsdEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @frameRate@
frameRateSelector :: Selector
frameRateSelector = mkSelector "frameRate"

-- | @Selector@ for @setFrameRate:@
setFrameRateSelector :: Selector
setFrameRateSelector = mkSelector "setFrameRate:"

-- | @Selector@ for @minResolution@
minResolutionSelector :: Selector
minResolutionSelector = mkSelector "minResolution"

-- | @Selector@ for @setMinResolution:@
setMinResolutionSelector :: Selector
setMinResolutionSelector = mkSelector "setMinResolution:"

-- | @Selector@ for @maxResolution@
maxResolutionSelector :: Selector
maxResolutionSelector = mkSelector "maxResolution"

-- | @Selector@ for @setMaxResolution:@
setMaxResolutionSelector :: Selector
setMaxResolutionSelector = mkSelector "setMaxResolution:"

-- | @Selector@ for @quality@
qualitySelector :: Selector
qualitySelector = mkSelector "quality"

-- | @Selector@ for @setQuality:@
setQualitySelector :: Selector
setQualitySelector = mkSelector "setQuality:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector
setReferenceCountSelector = mkSelector "setReferenceCount:"

-- | @Selector@ for @encodedPixels@
encodedPixelsSelector :: Selector
encodedPixelsSelector = mkSelector "encodedPixels"

-- | @Selector@ for @setEncodedPixels:@
setEncodedPixelsSelector :: Selector
setEncodedPixelsSelector = mkSelector "setEncodedPixels:"

-- | @Selector@ for @hardwareEncoder@
hardwareEncoderSelector :: Selector
hardwareEncoderSelector = mkSelector "hardwareEncoder"

-- | @Selector@ for @setHardwareEncoder:@
setHardwareEncoderSelector :: Selector
setHardwareEncoderSelector = mkSelector "setHardwareEncoder:"

-- | @Selector@ for @watermarkEnabled@
watermarkEnabledSelector :: Selector
watermarkEnabledSelector = mkSelector "watermarkEnabled"

-- | @Selector@ for @setWatermarkEnabled:@
setWatermarkEnabledSelector :: Selector
setWatermarkEnabledSelector = mkSelector "setWatermarkEnabled:"

-- | @Selector@ for @osdEnabled@
osdEnabledSelector :: Selector
osdEnabledSelector = mkSelector "osdEnabled"

-- | @Selector@ for @setOsdEnabled:@
setOsdEnabledSelector :: Selector
setOsdEnabledSelector = mkSelector "setOsdEnabled:"

