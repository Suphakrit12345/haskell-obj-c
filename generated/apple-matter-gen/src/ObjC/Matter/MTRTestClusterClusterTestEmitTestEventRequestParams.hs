{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEmitTestEventRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestEmitTestEventRequestParams
  ( MTRTestClusterClusterTestEmitTestEventRequestParams
  , IsMTRTestClusterClusterTestEmitTestEventRequestParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , setArg1Selector
  , arg2Selector
  , setArg2Selector
  , arg3Selector
  , setArg3Selector
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

-- | @- arg1@
arg1 :: IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams => mtrTestClusterClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestEmitTestEventRequestParams  =
    sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestEventRequestParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestEmitTestEventRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams => mtrTestClusterClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
arg2 mtrTestClusterClusterTestEmitTestEventRequestParams  =
    sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestEventRequestParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestEmitTestEventRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg3@
arg3 :: IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams => mtrTestClusterClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
arg3 mtrTestClusterClusterTestEmitTestEventRequestParams  =
    sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "arg3") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg3:@
setArg3 :: (IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestEventRequestParams -> value -> IO ()
setArg3 mtrTestClusterClusterTestEmitTestEventRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "setArg3:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams => mtrTestClusterClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestEmitTestEventRequestParams  =
    sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestEventRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestEmitTestEventRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams => mtrTestClusterClusterTestEmitTestEventRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestEmitTestEventRequestParams  =
    sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestEmitTestEventRequestParams mtrTestClusterClusterTestEmitTestEventRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestEventRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestEmitTestEventRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEmitTestEventRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector
setArg2Selector = mkSelector "setArg2:"

-- | @Selector@ for @arg3@
arg3Selector :: Selector
arg3Selector = mkSelector "arg3"

-- | @Selector@ for @setArg3:@
setArg3Selector :: Selector
setArg3Selector = mkSelector "setArg3:"

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

