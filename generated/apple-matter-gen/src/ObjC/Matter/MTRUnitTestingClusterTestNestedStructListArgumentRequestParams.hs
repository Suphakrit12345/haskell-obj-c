{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestNestedStructListArgumentRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestNestedStructListArgumentRequestParams
  ( MTRUnitTestingClusterTestNestedStructListArgumentRequestParams
  , IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams(..)
  , arg1
  , setArg1
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , arg1Selector
  , setArg1Selector
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
arg1 :: IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams mtrUnitTestingClusterTestNestedStructListArgumentRequestParams => mtrUnitTestingClusterTestNestedStructListArgumentRequestParams -> IO (Id MTRUnitTestingClusterNestedStructList)
arg1 mtrUnitTestingClusterTestNestedStructListArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestNestedStructListArgumentRequestParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams mtrUnitTestingClusterTestNestedStructListArgumentRequestParams, IsMTRUnitTestingClusterNestedStructList value) => mtrUnitTestingClusterTestNestedStructListArgumentRequestParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestNestedStructListArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNestedStructListArgumentRequestParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams mtrUnitTestingClusterTestNestedStructListArgumentRequestParams => mtrUnitTestingClusterTestNestedStructListArgumentRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestNestedStructListArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestNestedStructListArgumentRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams mtrUnitTestingClusterTestNestedStructListArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestNestedStructListArgumentRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestNestedStructListArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNestedStructListArgumentRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams mtrUnitTestingClusterTestNestedStructListArgumentRequestParams => mtrUnitTestingClusterTestNestedStructListArgumentRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestNestedStructListArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestNestedStructListArgumentRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestNestedStructListArgumentRequestParams mtrUnitTestingClusterTestNestedStructListArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestNestedStructListArgumentRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestNestedStructListArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNestedStructListArgumentRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector
setArg1Selector = mkSelector "setArg1:"

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

