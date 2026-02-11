{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideICECandidatesParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideICECandidatesParams
  ( MTRWebRTCTransportProviderClusterProvideICECandidatesParams
  , IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , iceCandidates
  , setIceCandidates
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , webRTCSessionIDSelector
  , setWebRTCSessionIDSelector
  , iceCandidatesSelector
  , setIceCandidatesSelector
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
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideICECandidatesParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "webRTCSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideICECandidatesParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "setWebRTCSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iceCandidates@
iceCandidates :: IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> IO (Id NSArray)
iceCandidates mtrWebRTCTransportProviderClusterProvideICECandidatesParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "iceCandidates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIceCandidates:@
setIceCandidates :: (IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams, IsNSArray value) => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> value -> IO ()
setIceCandidates mtrWebRTCTransportProviderClusterProvideICECandidatesParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "setIceCandidates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideICECandidatesParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideICECandidatesParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideICECandidatesParams  =
    sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterProvideICECandidatesParams mtrWebRTCTransportProviderClusterProvideICECandidatesParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideICECandidatesParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideICECandidatesParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterProvideICECandidatesParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @iceCandidates@
iceCandidatesSelector :: Selector
iceCandidatesSelector = mkSelector "iceCandidates"

-- | @Selector@ for @setIceCandidates:@
setIceCandidatesSelector :: Selector
setIceCandidatesSelector = mkSelector "setIceCandidates:"

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

