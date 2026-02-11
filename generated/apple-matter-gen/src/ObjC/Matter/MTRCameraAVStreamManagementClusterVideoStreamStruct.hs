{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamStruct
  ( MTRCameraAVStreamManagementClusterVideoStreamStruct
  , IsMTRCameraAVStreamManagementClusterVideoStreamStruct(..)
  , videoStreamID
  , setVideoStreamID
  , streamUsage
  , setStreamUsage
  , videoCodec
  , setVideoCodec
  , minFrameRate
  , setMinFrameRate
  , maxFrameRate
  , setMaxFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , minBitRate
  , setMinBitRate
  , maxBitRate
  , setMaxBitRate
  , keyFrameInterval
  , setKeyFrameInterval
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , referenceCount
  , setReferenceCount
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , streamUsageSelector
  , setStreamUsageSelector
  , videoCodecSelector
  , setVideoCodecSelector
  , minFrameRateSelector
  , setMinFrameRateSelector
  , maxFrameRateSelector
  , setMaxFrameRateSelector
  , minResolutionSelector
  , setMinResolutionSelector
  , maxResolutionSelector
  , setMaxResolutionSelector
  , minBitRateSelector
  , setMinBitRateSelector
  , maxBitRateSelector
  , setMaxBitRateSelector
  , keyFrameIntervalSelector
  , setKeyFrameIntervalSelector
  , watermarkEnabledSelector
  , setWatermarkEnabledSelector
  , osdEnabledSelector
  , setOsdEnabledSelector
  , referenceCountSelector
  , setReferenceCountSelector


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
videoStreamID :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
videoStreamID mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setVideoStreamID mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoCodec@
videoCodec :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
videoCodec mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "videoCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoCodec:@
setVideoCodec :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setVideoCodec mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setVideoCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minFrameRate@
minFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
minFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "minFrameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinFrameRate:@
setMinFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMinFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setMinFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "maxFrameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setMaxFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "minResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setMinResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "maxResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setMaxResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minBitRate@
minBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
minBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "minBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinBitRate:@
setMinBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMinBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setMinBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxBitRate@
maxBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
maxBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "maxBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxBitRate:@
setMaxBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMaxBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setMaxBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyFrameInterval@
keyFrameInterval :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
keyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "keyFrameInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyFrameInterval:@
setKeyFrameInterval :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setKeyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setKeyFrameInterval:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "watermarkEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setWatermarkEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "osdEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setOsdEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referenceCount@
referenceCount :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
referenceCount mtrCameraAVStreamManagementClusterVideoStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "referenceCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setReferenceCount mtrCameraAVStreamManagementClusterVideoStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamStruct (mkSelector "setReferenceCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @videoCodec@
videoCodecSelector :: Selector
videoCodecSelector = mkSelector "videoCodec"

-- | @Selector@ for @setVideoCodec:@
setVideoCodecSelector :: Selector
setVideoCodecSelector = mkSelector "setVideoCodec:"

-- | @Selector@ for @minFrameRate@
minFrameRateSelector :: Selector
minFrameRateSelector = mkSelector "minFrameRate"

-- | @Selector@ for @setMinFrameRate:@
setMinFrameRateSelector :: Selector
setMinFrameRateSelector = mkSelector "setMinFrameRate:"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector
maxFrameRateSelector = mkSelector "maxFrameRate"

-- | @Selector@ for @setMaxFrameRate:@
setMaxFrameRateSelector :: Selector
setMaxFrameRateSelector = mkSelector "setMaxFrameRate:"

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

-- | @Selector@ for @minBitRate@
minBitRateSelector :: Selector
minBitRateSelector = mkSelector "minBitRate"

-- | @Selector@ for @setMinBitRate:@
setMinBitRateSelector :: Selector
setMinBitRateSelector = mkSelector "setMinBitRate:"

-- | @Selector@ for @maxBitRate@
maxBitRateSelector :: Selector
maxBitRateSelector = mkSelector "maxBitRate"

-- | @Selector@ for @setMaxBitRate:@
setMaxBitRateSelector :: Selector
setMaxBitRateSelector = mkSelector "setMaxBitRate:"

-- | @Selector@ for @keyFrameInterval@
keyFrameIntervalSelector :: Selector
keyFrameIntervalSelector = mkSelector "keyFrameInterval"

-- | @Selector@ for @setKeyFrameInterval:@
setKeyFrameIntervalSelector :: Selector
setKeyFrameIntervalSelector = mkSelector "setKeyFrameInterval:"

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

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector
setReferenceCountSelector = mkSelector "setReferenceCount:"

