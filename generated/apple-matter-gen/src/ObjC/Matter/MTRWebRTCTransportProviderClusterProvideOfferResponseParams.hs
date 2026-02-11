{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideOfferResponseParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideOfferResponseParams
  ( MTRWebRTCTransportProviderClusterProvideOfferResponseParams
  , IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams(..)
  , initWithResponseValue_error
  , webRTCSessionID
  , setWebRTCSessionID
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , initWithResponseValue_errorSelector
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , videoStreamIDSelector
  , setVideoStreamIDSelector
  , audioStreamIDSelector
  , setAudioStreamIDSelector


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

-- | Initialize an MTRWebRTCTransportProviderClusterProvideOfferResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> responseValue -> error_ -> IO (Id MTRWebRTCTransportProviderClusterProvideOfferResponseParams)
initWithResponseValue_error mtrWebRTCTransportProviderClusterProvideOfferResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferResponseParams mtrWebRTCTransportProviderClusterProvideOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferResponseParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterProvideOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideOfferResponseParams (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

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

