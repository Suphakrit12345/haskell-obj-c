{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterGlobalEchoRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterGlobalEchoRequestParams
  ( MTRUnitTestingClusterGlobalEchoRequestParams
  , IsMTRUnitTestingClusterGlobalEchoRequestParams(..)
  , field1
  , setField1
  , field2
  , setField2
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , field1Selector
  , setField1Selector
  , field2Selector
  , setField2Selector
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

-- | @- field1@
field1 :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id MTRDataTypeTestGlobalStruct)
field1 mtrUnitTestingClusterGlobalEchoRequestParams  =
    sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "field1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setField1:@
setField1 :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsMTRDataTypeTestGlobalStruct value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setField1 mtrUnitTestingClusterGlobalEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "setField1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- field2@
field2 :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id NSNumber)
field2 mtrUnitTestingClusterGlobalEchoRequestParams  =
    sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "field2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setField2:@
setField2 :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setField2 mtrUnitTestingClusterGlobalEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "setField2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterGlobalEchoRequestParams  =
    sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterGlobalEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterGlobalEchoRequestParams  =
    sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterGlobalEchoRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterGlobalEchoRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @field1@
field1Selector :: Selector
field1Selector = mkSelector "field1"

-- | @Selector@ for @setField1:@
setField1Selector :: Selector
setField1Selector = mkSelector "setField1:"

-- | @Selector@ for @field2@
field2Selector :: Selector
field2Selector = mkSelector "field2"

-- | @Selector@ for @setField2:@
setField2Selector :: Selector
setField2Selector = mkSelector "setField2:"

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

