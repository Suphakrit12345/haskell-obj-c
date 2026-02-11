{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEnumsRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestEnumsRequestParams
  ( MTRTestClusterClusterTestEnumsRequestParams
  , IsMTRTestClusterClusterTestEnumsRequestParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , setArg1Selector
  , arg2Selector
  , setArg2Selector
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
arg1 :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestEnumsRequestParams  =
    sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestEnumsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
arg2 mtrTestClusterClusterTestEnumsRequestParams  =
    sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestEnumsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestEnumsRequestParams  =
    sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestEnumsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams => mtrTestClusterClusterTestEnumsRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestEnumsRequestParams  =
    sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestEnumsRequestParams mtrTestClusterClusterTestEnumsRequestParams, IsNSNumber value) => mtrTestClusterClusterTestEnumsRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestEnumsRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEnumsRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

