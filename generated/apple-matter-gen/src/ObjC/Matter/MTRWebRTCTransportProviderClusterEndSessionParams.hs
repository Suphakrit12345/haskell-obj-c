{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterEndSessionParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterEndSessionParams
  ( MTRWebRTCTransportProviderClusterEndSessionParams
  , IsMTRWebRTCTransportProviderClusterEndSessionParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , reason
  , setReason
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , reasonSelector
  , setReasonSelector
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
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterEndSessionParams  =
    sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterEndSessionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reason@
reason :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
reason mtrWebRTCTransportProviderClusterEndSessionParams  =
    sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "reason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setReason:@
setReason :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setReason mtrWebRTCTransportProviderClusterEndSessionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "setReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterEndSessionParams  =
    sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterEndSessionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterEndSessionParams  =
    sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterEndSessionParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterEndSessionParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector
setReasonSelector = mkSelector "setReason:"

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

