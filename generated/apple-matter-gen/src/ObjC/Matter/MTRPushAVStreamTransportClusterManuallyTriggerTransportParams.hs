{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterManuallyTriggerTransportParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterManuallyTriggerTransportParams
  ( MTRPushAVStreamTransportClusterManuallyTriggerTransportParams
  , IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams(..)
  , connectionID
  , setConnectionID
  , activationReason
  , setActivationReason
  , timeControl
  , setTimeControl
  , userDefined
  , setUserDefined
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , connectionIDSelector
  , setConnectionIDSelector
  , activationReasonSelector
  , setActivationReasonSelector
  , timeControlSelector
  , setTimeControlSelector
  , userDefinedSelector
  , setUserDefinedSelector
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

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "connectionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "setConnectionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activationReason@
activationReason :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
activationReason mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "activationReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActivationReason:@
setActivationReason :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setActivationReason mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "setActivationReason:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeControl@
timeControl :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct)
timeControl mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "timeControl") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeControl:@
setTimeControl :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setTimeControl mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "setTimeControl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userDefined@
userDefined :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSData)
userDefined mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "userDefined") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserDefined:@
setUserDefined :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSData value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setUserDefined mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "setUserDefined:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams mtrPushAVStreamTransportClusterManuallyTriggerTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterManuallyTriggerTransportParams -> value -> IO ()
setServerSideProcessingTimeout mtrPushAVStreamTransportClusterManuallyTriggerTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterManuallyTriggerTransportParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @activationReason@
activationReasonSelector :: Selector
activationReasonSelector = mkSelector "activationReason"

-- | @Selector@ for @setActivationReason:@
setActivationReasonSelector :: Selector
setActivationReasonSelector = mkSelector "setActivationReason:"

-- | @Selector@ for @timeControl@
timeControlSelector :: Selector
timeControlSelector = mkSelector "timeControl"

-- | @Selector@ for @setTimeControl:@
setTimeControlSelector :: Selector
setTimeControlSelector = mkSelector "setTimeControl:"

-- | @Selector@ for @userDefined@
userDefinedSelector :: Selector
userDefinedSelector = mkSelector "userDefined"

-- | @Selector@ for @setUserDefined:@
setUserDefinedSelector :: Selector
setUserDefinedSelector = mkSelector "setUserDefined:"

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

