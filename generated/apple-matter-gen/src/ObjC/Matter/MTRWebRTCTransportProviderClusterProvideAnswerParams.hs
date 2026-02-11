{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideAnswerParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideAnswerParams
  ( MTRWebRTCTransportProviderClusterProvideAnswerParams
  , IsMTRWebRTCTransportProviderClusterProvideAnswerParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , sdp
  , setSdp
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , sdpSelector
  , setSdpSelector
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
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideAnswerParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideAnswerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sdp@
sdp :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSString)
sdp mtrWebRTCTransportProviderClusterProvideAnswerParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "sdp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSString value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setSdp mtrWebRTCTransportProviderClusterProvideAnswerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "setSdp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideAnswerParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideAnswerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideAnswerParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideAnswerParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideAnswerParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

