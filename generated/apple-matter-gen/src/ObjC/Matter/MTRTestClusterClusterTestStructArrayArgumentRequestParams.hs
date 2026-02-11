{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestStructArrayArgumentRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestStructArrayArgumentRequestParams
  ( MTRTestClusterClusterTestStructArrayArgumentRequestParams
  , IsMTRTestClusterClusterTestStructArrayArgumentRequestParams(..)
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
arg1 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg1 mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg2 mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg3@
arg3 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg3 mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "arg3") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg3:@
setArg3 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg3 mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setArg3:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg4@
arg4 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSArray)
arg4 mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "arg4") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg4:@
setArg4 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg4 mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setArg4:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg5@
arg5 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
arg5 mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "arg5") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg5:@
setArg5 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg5 mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setArg5:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg6@
arg6 :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
arg6 mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "arg6") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg6:@
setArg6 :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setArg6 mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setArg6:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestStructArrayArgumentRequestParams  =
    sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestStructArrayArgumentRequestParams mtrTestClusterClusterTestStructArrayArgumentRequestParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestStructArrayArgumentRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestStructArrayArgumentRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

