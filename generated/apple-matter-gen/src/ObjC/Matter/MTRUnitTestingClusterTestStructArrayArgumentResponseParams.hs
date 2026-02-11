{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestStructArrayArgumentResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestStructArrayArgumentResponseParams
  ( MTRUnitTestingClusterTestStructArrayArgumentResponseParams
  , IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams(..)
  , initWithResponseValue_error
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
  , initWithResponseValue_errorSelector
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

-- | Initialize an MTRUnitTestingClusterTestStructArrayArgumentResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestStructArrayArgumentResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestStructArrayArgumentResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg1 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg2 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg2 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg3@
arg3 :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg3 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "arg3") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg3:@
setArg3 :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg3 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setArg3:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg4@
arg4 :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg4 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "arg4") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg4:@
setArg4 :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg4 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setArg4:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg5@
arg5 :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSNumber)
arg5 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "arg5") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg5:@
setArg5 :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg5 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setArg5:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg6@
arg6 :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSNumber)
arg6 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "arg6") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg6:@
setArg6 :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg6 mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setArg6:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestStructArrayArgumentResponseParams  =
    sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestStructArrayArgumentResponseParams mtrUnitTestingClusterTestStructArrayArgumentResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestStructArrayArgumentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestStructArrayArgumentResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

