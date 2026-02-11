{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestListInt8UReverseResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestListInt8UReverseResponseParams
  ( MTRUnitTestingClusterTestListInt8UReverseResponseParams
  , IsMTRUnitTestingClusterTestListInt8UReverseResponseParams(..)
  , initWithResponseValue_error
  , arg1
  , setArg1
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , arg1Selector
  , setArg1Selector
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

-- | Initialize an MTRUnitTestingClusterTestListInt8UReverseResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestListInt8UReverseResponseParams mtrUnitTestingClusterTestListInt8UReverseResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestListInt8UReverseResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestListInt8UReverseResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestListInt8UReverseResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestListInt8UReverseResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestListInt8UReverseResponseParams mtrUnitTestingClusterTestListInt8UReverseResponseParams => mtrUnitTestingClusterTestListInt8UReverseResponseParams -> IO (Id NSArray)
arg1 mtrUnitTestingClusterTestListInt8UReverseResponseParams  =
    sendMsg mtrUnitTestingClusterTestListInt8UReverseResponseParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestListInt8UReverseResponseParams mtrUnitTestingClusterTestListInt8UReverseResponseParams, IsNSArray value) => mtrUnitTestingClusterTestListInt8UReverseResponseParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestListInt8UReverseResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestListInt8UReverseResponseParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestListInt8UReverseResponseParams mtrUnitTestingClusterTestListInt8UReverseResponseParams => mtrUnitTestingClusterTestListInt8UReverseResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestListInt8UReverseResponseParams  =
    sendMsg mtrUnitTestingClusterTestListInt8UReverseResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestListInt8UReverseResponseParams mtrUnitTestingClusterTestListInt8UReverseResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestListInt8UReverseResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestListInt8UReverseResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestListInt8UReverseResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

