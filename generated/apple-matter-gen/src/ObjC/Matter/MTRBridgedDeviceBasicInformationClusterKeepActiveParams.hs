{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterKeepActiveParams@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterKeepActiveParams
  ( MTRBridgedDeviceBasicInformationClusterKeepActiveParams
  , IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams(..)
  , stayActiveDuration
  , setStayActiveDuration
  , timeoutMs
  , setTimeoutMs
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , stayActiveDurationSelector
  , setStayActiveDurationSelector
  , timeoutMsSelector
  , setTimeoutMsSelector
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

-- | @- stayActiveDuration@
stayActiveDuration :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
stayActiveDuration mtrBridgedDeviceBasicInformationClusterKeepActiveParams  =
    sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "stayActiveDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStayActiveDuration:@
setStayActiveDuration :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setStayActiveDuration mtrBridgedDeviceBasicInformationClusterKeepActiveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "setStayActiveDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeoutMs@
timeoutMs :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
timeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams  =
    sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "timeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeoutMs:@
setTimeoutMs :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setTimeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "setTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams  =
    sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrBridgedDeviceBasicInformationClusterKeepActiveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrBridgedDeviceBasicInformationClusterKeepActiveParams  =
    sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams mtrBridgedDeviceBasicInformationClusterKeepActiveParams, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterKeepActiveParams -> value -> IO ()
setServerSideProcessingTimeout mtrBridgedDeviceBasicInformationClusterKeepActiveParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBridgedDeviceBasicInformationClusterKeepActiveParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stayActiveDuration@
stayActiveDurationSelector :: Selector
stayActiveDurationSelector = mkSelector "stayActiveDuration"

-- | @Selector@ for @setStayActiveDuration:@
setStayActiveDurationSelector :: Selector
setStayActiveDurationSelector = mkSelector "setStayActiveDuration:"

-- | @Selector@ for @timeoutMs@
timeoutMsSelector :: Selector
timeoutMsSelector = mkSelector "timeoutMs"

-- | @Selector@ for @setTimeoutMs:@
setTimeoutMsSelector :: Selector
setTimeoutMsSelector = mkSelector "setTimeoutMs:"

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

