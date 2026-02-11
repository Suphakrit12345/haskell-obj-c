{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterStringEchoRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterStringEchoRequestParams
  ( MTRUnitTestingClusterStringEchoRequestParams
  , IsMTRUnitTestingClusterStringEchoRequestParams(..)
  , payload
  , setPayload
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , payloadSelector
  , setPayloadSelector
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

-- | @- payload@
payload :: IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams => mtrUnitTestingClusterStringEchoRequestParams -> IO (Id NSData)
payload mtrUnitTestingClusterStringEchoRequestParams  =
    sendMsg mtrUnitTestingClusterStringEchoRequestParams (mkSelector "payload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPayload:@
setPayload :: (IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams, IsNSData value) => mtrUnitTestingClusterStringEchoRequestParams -> value -> IO ()
setPayload mtrUnitTestingClusterStringEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterStringEchoRequestParams (mkSelector "setPayload:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams => mtrUnitTestingClusterStringEchoRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterStringEchoRequestParams  =
    sendMsg mtrUnitTestingClusterStringEchoRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterStringEchoRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterStringEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterStringEchoRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams => mtrUnitTestingClusterStringEchoRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterStringEchoRequestParams  =
    sendMsg mtrUnitTestingClusterStringEchoRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterStringEchoRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterStringEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterStringEchoRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @payload@
payloadSelector :: Selector
payloadSelector = mkSelector "payload"

-- | @Selector@ for @setPayload:@
setPayloadSelector :: Selector
setPayloadSelector = mkSelector "setPayload:"

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

