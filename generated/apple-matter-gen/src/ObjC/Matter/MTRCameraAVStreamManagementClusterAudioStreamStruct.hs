{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioStreamStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioStreamStruct
  ( MTRCameraAVStreamManagementClusterAudioStreamStruct
  , IsMTRCameraAVStreamManagementClusterAudioStreamStruct(..)
  , audioStreamID
  , setAudioStreamID
  , streamUsage
  , setStreamUsage
  , audioCodec
  , setAudioCodec
  , channelCount
  , setChannelCount
  , sampleRate
  , setSampleRate
  , bitRate
  , setBitRate
  , bitDepth
  , setBitDepth
  , referenceCount
  , setReferenceCount
  , audioStreamIDSelector
  , setAudioStreamIDSelector
  , streamUsageSelector
  , setStreamUsageSelector
  , audioCodecSelector
  , setAudioCodecSelector
  , channelCountSelector
  , setChannelCountSelector
  , sampleRateSelector
  , setSampleRateSelector
  , bitRateSelector
  , setBitRateSelector
  , bitDepthSelector
  , setBitDepthSelector
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

-- | @- audioStreamID@
audioStreamID :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
audioStreamID mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setAudioStreamID mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioCodec@
audioCodec :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
audioCodec mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "audioCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioCodec:@
setAudioCodec :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setAudioCodec mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setAudioCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channelCount@
channelCount :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
channelCount mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "channelCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannelCount:@
setChannelCount :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setChannelCount mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setChannelCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sampleRate@
sampleRate :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
sampleRate mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "sampleRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSampleRate:@
setSampleRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setSampleRate mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setSampleRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bitRate@
bitRate :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
bitRate mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "bitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBitRate:@
setBitRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setBitRate mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bitDepth@
bitDepth :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
bitDepth mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "bitDepth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBitDepth:@
setBitDepth :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setBitDepth mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setBitDepth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referenceCount@
referenceCount :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
referenceCount mtrCameraAVStreamManagementClusterAudioStreamStruct  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "referenceCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setReferenceCount mtrCameraAVStreamManagementClusterAudioStreamStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamStruct (mkSelector "setReferenceCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @audioCodec@
audioCodecSelector :: Selector
audioCodecSelector = mkSelector "audioCodec"

-- | @Selector@ for @setAudioCodec:@
setAudioCodecSelector :: Selector
setAudioCodecSelector = mkSelector "setAudioCodec:"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @setChannelCount:@
setChannelCountSelector :: Selector
setChannelCountSelector = mkSelector "setChannelCount:"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @setSampleRate:@
setSampleRateSelector :: Selector
setSampleRateSelector = mkSelector "setSampleRate:"

-- | @Selector@ for @bitRate@
bitRateSelector :: Selector
bitRateSelector = mkSelector "bitRate"

-- | @Selector@ for @setBitRate:@
setBitRateSelector :: Selector
setBitRateSelector = mkSelector "setBitRate:"

-- | @Selector@ for @bitDepth@
bitDepthSelector :: Selector
bitDepthSelector = mkSelector "bitDepth"

-- | @Selector@ for @setBitDepth:@
setBitDepthSelector :: Selector
setBitDepthSelector = mkSelector "setBitDepth:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector
setReferenceCountSelector = mkSelector "setReferenceCount:"

