{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioStreamAllocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioStreamAllocateParams
  ( MTRCameraAVStreamManagementClusterAudioStreamAllocateParams
  , IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams(..)
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
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioCodec@
audioCodec :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
audioCodec mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "audioCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioCodec:@
setAudioCodec :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setAudioCodec mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setAudioCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channelCount@
channelCount :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
channelCount mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "channelCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannelCount:@
setChannelCount :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setChannelCount mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setChannelCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sampleRate@
sampleRate :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
sampleRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "sampleRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSampleRate:@
setSampleRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setSampleRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setSampleRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bitRate@
bitRate :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
bitRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "bitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBitRate:@
setBitRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setBitRate mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bitDepth@
bitDepth :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
bitDepth mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "bitDepth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBitDepth:@
setBitDepth :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setBitDepth mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setBitDepth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams mtrCameraAVStreamManagementClusterAudioStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterAudioStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterAudioStreamAllocateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

