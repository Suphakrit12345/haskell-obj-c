{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterSolicitOfferParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterSolicitOfferParams
  ( MTRWebRTCTransportProviderClusterSolicitOfferParams
  , IsMTRWebRTCTransportProviderClusterSolicitOfferParams(..)
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

-- | @- streamUsage@
streamUsage :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
streamUsage mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setStreamUsage mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originatingEndpointID@
originatingEndpointID :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
originatingEndpointID mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "originatingEndpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginatingEndpointID:@
setOriginatingEndpointID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setOriginatingEndpointID mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setOriginatingEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceServers@
iceServers :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSArray)
iceServers mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "iceServers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceServers:@
setIceServers :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSArray value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setIceServers mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setIceServers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceTransportPolicy@
iceTransportPolicy :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSString)
iceTransportPolicy mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "iceTransportPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceTransportPolicy:@
setIceTransportPolicy :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSString value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setIceTransportPolicy mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setIceTransportPolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- metadataEnabled@
metadataEnabled :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
metadataEnabled mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "metadataEnabled") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setMetadataEnabled mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setMetadataEnabled:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sFrameConfig@
sFrameConfig :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id MTRWebRTCTransportProviderClusterSFrameStruct)
sFrameConfig mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "sFrameConfig") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSFrameConfig:@
setSFrameConfig :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsMTRWebRTCTransportProviderClusterSFrameStruct value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setSFrameConfig mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setSFrameConfig:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterSolicitOfferParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterSolicitOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

