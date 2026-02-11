{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterSolicitOfferResponseParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterSolicitOfferResponseParams
  ( MTRWebRTCTransportProviderClusterSolicitOfferResponseParams
  , IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams(..)
  , initWithResponseValue_error
  , webRTCSessionID
  , setWebRTCSessionID
  , deferredOffer
  , setDeferredOffer
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , initWithResponseValue_errorSelector
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , deferredOfferSelector
  , setDeferredOfferSelector
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

-- | Initialize an MTRWebRTCTransportProviderClusterSolicitOfferResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> responseValue -> error_ -> IO (Id MTRWebRTCTransportProviderClusterSolicitOfferResponseParams)
initWithResponseValue_error mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- deferredOffer@
deferredOffer :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
deferredOffer mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "deferredOffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeferredOffer:@
setDeferredOffer :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setDeferredOffer mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "setDeferredOffer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "videoStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "setVideoStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  =
    sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "audioStreamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferResponseParams mtrWebRTCTransportProviderClusterSolicitOfferResponseParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferResponseParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterSolicitOfferResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSolicitOfferResponseParams (mkSelector "setAudioStreamID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @deferredOffer@
deferredOfferSelector :: Selector
deferredOfferSelector = mkSelector "deferredOffer"

-- | @Selector@ for @setDeferredOffer:@
setDeferredOfferSelector :: Selector
setDeferredOfferSelector = mkSelector "setDeferredOffer:"

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

