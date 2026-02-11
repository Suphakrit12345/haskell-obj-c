{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAtomicRequestParams@.
module ObjC.Matter.MTRThermostatClusterAtomicRequestParams
  ( MTRThermostatClusterAtomicRequestParams
  , IsMTRThermostatClusterAtomicRequestParams(..)
  , requestType
  , setRequestType
  , attributeRequests
  , setAttributeRequests
  , timeout
  , setTimeout
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , requestTypeSelector
  , setRequestTypeSelector
  , attributeRequestsSelector
  , setAttributeRequestsSelector
  , timeoutSelector
  , setTimeoutSelector
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

-- | @- requestType@
requestType :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
requestType mtrThermostatClusterAtomicRequestParams  =
    sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "requestType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequestType:@
setRequestType :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setRequestType mtrThermostatClusterAtomicRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "setRequestType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributeRequests@
attributeRequests :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSArray)
attributeRequests mtrThermostatClusterAtomicRequestParams  =
    sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "attributeRequests") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeRequests:@
setAttributeRequests :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSArray value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setAttributeRequests mtrThermostatClusterAtomicRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "setAttributeRequests:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeout@
timeout :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
timeout mtrThermostatClusterAtomicRequestParams  =
    sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "timeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeout:@
setTimeout :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setTimeout mtrThermostatClusterAtomicRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "setTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterAtomicRequestParams  =
    sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterAtomicRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterAtomicRequestParams  =
    sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterAtomicRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestType@
requestTypeSelector :: Selector
requestTypeSelector = mkSelector "requestType"

-- | @Selector@ for @setRequestType:@
setRequestTypeSelector :: Selector
setRequestTypeSelector = mkSelector "setRequestType:"

-- | @Selector@ for @attributeRequests@
attributeRequestsSelector :: Selector
attributeRequestsSelector = mkSelector "attributeRequests"

-- | @Selector@ for @setAttributeRequests:@
setAttributeRequestsSelector :: Selector
setAttributeRequestsSelector = mkSelector "setAttributeRequests:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector
setTimeoutSelector = mkSelector "setTimeout:"

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

