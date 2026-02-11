{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideOfferParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideOfferParams
  ( MTRWebRTCTransportProviderClusterProvideOfferParams
  , IsMTRWebRTCTransportProviderClusterProvideOfferParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , sdp
  , setSdp
  , streamUsage
  , setStreamUsage
  , originatingEndpointID
  , setOriginatingEndpointID
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , iceServers
  , setIceServers
  , iceTransportPolicy
  , setIceTransportPolicy
  , metadataEnabled
  , setMetadataEnabled
  , sFrameConfig
  , setSFrameConfig
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , sdpSelector
  , setSdpSelector
  , streamUsageSelector
  , setStreamUsageSelector
  , originatingEndpointIDSelector
  , setOriginatingEndpointIDSelector
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , audioStreamIDSelector
  , setAudioStreamIDSelector
  , iceServersSelector
  , setIceServersSelector
  , iceTransportPolicySelector
  , setIceTransportPolicySelector
  , metadataEnabledSelector
  , setMetadataEnabledSelector
  , sFrameConfigSelector
  , setSFrameConfigSelector
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

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sdp@
sdp :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSString)
sdp mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "sdp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSString value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setSdp mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setSdp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- streamUsage@
streamUsage :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
streamUsage mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setStreamUsage mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originatingEndpointID@
originatingEndpointID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
originatingEndpointID mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "originatingEndpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginatingEndpointID:@
setOriginatingEndpointID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setOriginatingEndpointID mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setOriginatingEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceServers@
iceServers :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSArray)
iceServers mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "iceServers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceServers:@
setIceServers :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSArray value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setIceServers mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setIceServers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceTransportPolicy@
iceTransportPolicy :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSString)
iceTransportPolicy mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "iceTransportPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceTransportPolicy:@
setIceTransportPolicy :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSString value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setIceTransportPolicy mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setIceTransportPolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataEnabled@
metadataEnabled :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
metadataEnabled mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "metadataEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setMetadataEnabled mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setMetadataEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sFrameConfig@
sFrameConfig :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id MTRWebRTCTransportProviderClusterSFrameStruct)
sFrameConfig mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "sFrameConfig") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSFrameConfig:@
setSFrameConfig :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsMTRWebRTCTransportProviderClusterSFrameStruct value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setSFrameConfig mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setSFrameConfig:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @sdp@
sdpSelector :: Selector
sdpSelector = mkSelector "sdp"

-- | @Selector@ for @setSdp:@
setSdpSelector :: Selector
setSdpSelector = mkSelector "setSdp:"

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @originatingEndpointID@
originatingEndpointIDSelector :: Selector
originatingEndpointIDSelector = mkSelector "originatingEndpointID"

-- | @Selector@ for @setOriginatingEndpointID:@
setOriginatingEndpointIDSelector :: Selector
setOriginatingEndpointIDSelector = mkSelector "setOriginatingEndpointID:"

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

-- | @Selector@ for @iceServers@
iceServersSelector :: Selector
iceServersSelector = mkSelector "iceServers"

-- | @Selector@ for @setIceServers:@
setIceServersSelector :: Selector
setIceServersSelector = mkSelector "setIceServers:"

-- | @Selector@ for @iceTransportPolicy@
iceTransportPolicySelector :: Selector
iceTransportPolicySelector = mkSelector "iceTransportPolicy"

-- | @Selector@ for @setIceTransportPolicy:@
setIceTransportPolicySelector :: Selector
setIceTransportPolicySelector = mkSelector "setIceTransportPolicy:"

-- | @Selector@ for @metadataEnabled@
metadataEnabledSelector :: Selector
metadataEnabledSelector = mkSelector "metadataEnabled"

-- | @Selector@ for @setMetadataEnabled:@
setMetadataEnabledSelector :: Selector
setMetadataEnabledSelector = mkSelector "setMetadataEnabled:"

-- | @Selector@ for @sFrameConfig@
sFrameConfigSelector :: Selector
sFrameConfigSelector = mkSelector "sFrameConfig"

-- | @Selector@ for @setSFrameConfig:@
setSFrameConfigSelector :: Selector
setSFrameConfigSelector = mkSelector "setSFrameConfig:"

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

