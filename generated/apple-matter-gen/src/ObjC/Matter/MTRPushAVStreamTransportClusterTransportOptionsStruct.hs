{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportOptionsStruct
  ( MTRPushAVStreamTransportClusterTransportOptionsStruct
  , IsMTRPushAVStreamTransportClusterTransportOptionsStruct(..)
  , streamUsage
  , setStreamUsage
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , tlsEndpointID
  , setTlsEndpointID
  , url
  , setUrl
  , triggerOptions
  , setTriggerOptions
  , ingestMethod
  , setIngestMethod
  , containerOptions
  , setContainerOptions
  , expiryTime
  , setExpiryTime
  , streamUsageSelector
  , setStreamUsageSelector
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , audioStreamIDSelector
  , setAudioStreamIDSelector
  , tlsEndpointIDSelector
  , setTlsEndpointIDSelector
  , urlSelector
  , setUrlSelector
  , triggerOptionsSelector
  , setTriggerOptionsSelector
  , ingestMethodSelector
  , setIngestMethodSelector
  , containerOptionsSelector
  , setContainerOptionsSelector
  , expiryTimeSelector
  , setExpiryTimeSelector


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
streamUsage :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
streamUsage mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "streamUsage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setStreamUsage mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setStreamUsage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoStreamID@
videoStreamID :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
videoStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setVideoStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioStreamID@
audioStreamID :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
audioStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setAudioStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tlsEndpointID@
tlsEndpointID :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
tlsEndpointID mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "tlsEndpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTlsEndpointID:@
setTlsEndpointID :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setTlsEndpointID mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setTlsEndpointID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- url@
url :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSString)
url mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUrl:@
setUrl :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSString value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setUrl mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- triggerOptions@
triggerOptions :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct)
triggerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "triggerOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTriggerOptions:@
setTriggerOptions :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setTriggerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setTriggerOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ingestMethod@
ingestMethod :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
ingestMethod mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "ingestMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIngestMethod:@
setIngestMethod :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setIngestMethod mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setIngestMethod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerOptions@
containerOptions :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterContainerOptionsStruct)
containerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "containerOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerOptions:@
setContainerOptions :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsMTRPushAVStreamTransportClusterContainerOptionsStruct value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setContainerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setContainerOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expiryTime@
expiryTime :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
expiryTime mtrPushAVStreamTransportClusterTransportOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "expiryTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpiryTime:@
setExpiryTime :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setExpiryTime mtrPushAVStreamTransportClusterTransportOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterTransportOptionsStruct (mkSelector "setExpiryTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector
setStreamUsageSelector = mkSelector "setStreamUsage:"

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

-- | @Selector@ for @tlsEndpointID@
tlsEndpointIDSelector :: Selector
tlsEndpointIDSelector = mkSelector "tlsEndpointID"

-- | @Selector@ for @setTlsEndpointID:@
setTlsEndpointIDSelector :: Selector
setTlsEndpointIDSelector = mkSelector "setTlsEndpointID:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @triggerOptions@
triggerOptionsSelector :: Selector
triggerOptionsSelector = mkSelector "triggerOptions"

-- | @Selector@ for @setTriggerOptions:@
setTriggerOptionsSelector :: Selector
setTriggerOptionsSelector = mkSelector "setTriggerOptions:"

-- | @Selector@ for @ingestMethod@
ingestMethodSelector :: Selector
ingestMethodSelector = mkSelector "ingestMethod"

-- | @Selector@ for @setIngestMethod:@
setIngestMethodSelector :: Selector
setIngestMethodSelector = mkSelector "setIngestMethod:"

-- | @Selector@ for @containerOptions@
containerOptionsSelector :: Selector
containerOptionsSelector = mkSelector "containerOptions"

-- | @Selector@ for @setContainerOptions:@
setContainerOptionsSelector :: Selector
setContainerOptionsSelector = mkSelector "setContainerOptions:"

-- | @Selector@ for @expiryTime@
expiryTimeSelector :: Selector
expiryTimeSelector = mkSelector "expiryTime"

-- | @Selector@ for @setExpiryTime:@
setExpiryTimeSelector :: Selector
setExpiryTimeSelector = mkSelector "setExpiryTime:"

