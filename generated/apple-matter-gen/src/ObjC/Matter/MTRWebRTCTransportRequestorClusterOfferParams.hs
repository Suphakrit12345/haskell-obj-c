{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportRequestorClusterOfferParams@.
module ObjC.Matter.MTRWebRTCTransportRequestorClusterOfferParams
  ( MTRWebRTCTransportRequestorClusterOfferParams
  , IsMTRWebRTCTransportRequestorClusterOfferParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , sdp
  , setSdp
  , iceServers
  , setIceServers
  , iceTransportPolicy
  , setIceTransportPolicy
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , sdpSelector
  , setSdpSelector
  , iceServersSelector
  , setIceServersSelector
  , iceTransportPolicySelector
  , setIceTransportPolicySelector
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
webRTCSessionID :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportRequestorClusterOfferParams  =
    sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportRequestorClusterOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sdp@
sdp :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSString)
sdp mtrWebRTCTransportRequestorClusterOfferParams  =
    sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "sdp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSString value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setSdp mtrWebRTCTransportRequestorClusterOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "setSdp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceServers@
iceServers :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSArray)
iceServers mtrWebRTCTransportRequestorClusterOfferParams  =
    sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "iceServers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceServers:@
setIceServers :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSArray value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setIceServers mtrWebRTCTransportRequestorClusterOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "setIceServers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceTransportPolicy@
iceTransportPolicy :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSString)
iceTransportPolicy mtrWebRTCTransportRequestorClusterOfferParams  =
    sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "iceTransportPolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceTransportPolicy:@
setIceTransportPolicy :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSString value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setIceTransportPolicy mtrWebRTCTransportRequestorClusterOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "setIceTransportPolicy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterOfferParams  =
    sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportRequestorClusterOfferParams  =
    sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportRequestorClusterOfferParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportRequestorClusterOfferParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

