{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterModifyPushTransportParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterModifyPushTransportParams
  ( MTRPushAVStreamTransportClusterModifyPushTransportParams
  , IsMTRPushAVStreamTransportClusterModifyPushTransportParams(..)
  , connectionID
  , setConnectionID
  , transportOptions
  , setTransportOptions
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , connectionIDSelector
  , setConnectionIDSelector
  , transportOptionsSelector
  , setTransportOptionsSelector
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
connectionID :: IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams => mtrPushAVStreamTransportClusterModifyPushTransportParams -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterModifyPushTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "connectionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterModifyPushTransportParams -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterModifyPushTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "setConnectionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- transportOptions@
transportOptions :: IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams => mtrPushAVStreamTransportClusterModifyPushTransportParams -> IO (Id MTRPushAVStreamTransportClusterTransportOptionsStruct)
transportOptions mtrPushAVStreamTransportClusterModifyPushTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "transportOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransportOptions:@
setTransportOptions :: (IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams, IsMTRPushAVStreamTransportClusterTransportOptionsStruct value) => mtrPushAVStreamTransportClusterModifyPushTransportParams -> value -> IO ()
setTransportOptions mtrPushAVStreamTransportClusterModifyPushTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "setTransportOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams => mtrPushAVStreamTransportClusterModifyPushTransportParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrPushAVStreamTransportClusterModifyPushTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterModifyPushTransportParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrPushAVStreamTransportClusterModifyPushTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams => mtrPushAVStreamTransportClusterModifyPushTransportParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrPushAVStreamTransportClusterModifyPushTransportParams  =
    sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRPushAVStreamTransportClusterModifyPushTransportParams mtrPushAVStreamTransportClusterModifyPushTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterModifyPushTransportParams -> value -> IO ()
setServerSideProcessingTimeout mtrPushAVStreamTransportClusterModifyPushTransportParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterModifyPushTransportParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @transportOptions@
transportOptionsSelector :: Selector
transportOptionsSelector = mkSelector "transportOptions"

-- | @Selector@ for @setTransportOptions:@
setTransportOptionsSelector :: Selector
setTransportOptionsSelector = mkSelector "setTransportOptions:"

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

