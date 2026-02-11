{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestEnumsResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestEnumsResponseParams
  ( MTRUnitTestingClusterTestEnumsResponseParams
  , IsMTRUnitTestingClusterTestEnumsResponseParams(..)
  , initWithResponseValue_error
  , arg1
  , setArg1
  , arg2
  , setArg2
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , arg1Selector
  , setArg1Selector
  , arg2Selector
  , setArg2Selector
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

-- | Initialize an MTRUnitTestingClusterTestEnumsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestEnumsResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestEnumsResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestEnumsResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams => mtrUnitTestingClusterTestEnumsResponseParams -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestEnumsResponseParams  =
    sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestEnumsResponseParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestEnumsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams => mtrUnitTestingClusterTestEnumsResponseParams -> IO (Id NSNumber)
arg2 mtrUnitTestingClusterTestEnumsResponseParams  =
    sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestEnumsResponseParams -> value -> IO ()
setArg2 mtrUnitTestingClusterTestEnumsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams => mtrUnitTestingClusterTestEnumsResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestEnumsResponseParams  =
    sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestEnumsResponseParams mtrUnitTestingClusterTestEnumsResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestEnumsResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestEnumsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestEnumsResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

