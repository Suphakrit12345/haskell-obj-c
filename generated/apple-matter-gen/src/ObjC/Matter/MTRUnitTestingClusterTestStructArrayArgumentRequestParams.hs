{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestStructArrayArgumentRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestStructArrayArgumentRequestParams
  ( MTRUnitTestingClusterTestStructArrayArgumentRequestParams
  , IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , arg4
  , setArg4
  , arg5
  , setArg5
  , arg6
  , setArg6
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
  , arg4Selector
  , setArg4Selector
  , arg5Selector
  , setArg5Selector
  , arg6Selector
  , setArg6Selector
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
arg1 :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg1 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg2 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg2 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg3@
arg3 :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg3 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "arg3") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg3:@
setArg3 :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg3 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setArg3:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg4@
arg4 :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg4 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "arg4") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg4:@
setArg4 :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg4 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setArg4:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg5@
arg5 :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
arg5 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "arg5") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg5:@
setArg5 :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg5 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setArg5:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg6@
arg6 :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
arg6 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "arg6") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg6:@
setArg6 :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg6 mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setArg6:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestStructArrayArgumentRequestParams mtrUnitTestingClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @arg4@
arg4Selector :: Selector
arg4Selector = mkSelector "arg4"

-- | @Selector@ for @setArg4:@
setArg4Selector :: Selector
setArg4Selector = mkSelector "setArg4:"

-- | @Selector@ for @arg5@
arg5Selector :: Selector
arg5Selector = mkSelector "arg5"

-- | @Selector@ for @setArg5:@
setArg5Selector :: Selector
setArg5Selector = mkSelector "setArg5:"

-- | @Selector@ for @arg6@
arg6Selector :: Selector
arg6Selector = mkSelector "arg6"

-- | @Selector@ for @setArg6:@
setArg6Selector :: Selector
setArg6Selector = mkSelector "setArg6:"

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

