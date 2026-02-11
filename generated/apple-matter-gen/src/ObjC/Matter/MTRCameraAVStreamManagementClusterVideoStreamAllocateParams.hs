{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamAllocateParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamAllocateParams
  ( MTRCameraAVStreamManagementClusterVideoStreamAllocateParams
  , IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams(..)
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
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
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
streamUsage :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoCodec@
videoCodec :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
videoCodec mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "videoCodec") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoCodec:@
setVideoCodec :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setVideoCodec mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setVideoCodec:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minFrameRate@
minFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
minFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "minFrameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinFrameRate:@
setMinFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMinFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setMinFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "maxFrameRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setMaxFrameRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "minResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setMinResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "maxResolution") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setMaxResolution:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minBitRate@
minBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
minBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "minBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinBitRate:@
setMinBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMinBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setMinBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxBitRate@
maxBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
maxBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "maxBitRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxBitRate:@
setMaxBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setMaxBitRate mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setMaxBitRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyFrameInterval@
keyFrameInterval :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
keyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "keyFrameInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyFrameInterval:@
setKeyFrameInterval :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setKeyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setKeyFrameInterval:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "watermarkEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setWatermarkEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "osdEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setOsdEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  =
    sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams mtrCameraAVStreamManagementClusterVideoStreamAllocateParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVStreamManagementClusterVideoStreamAllocateParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoStreamAllocateParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

