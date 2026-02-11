{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccountLoginClusterGetSetupPINResponseParams@.
module ObjC.Matter.MTRAccountLoginClusterGetSetupPINResponseParams
  ( MTRAccountLoginClusterGetSetupPINResponseParams
  , IsMTRAccountLoginClusterGetSetupPINResponseParams(..)
  , initWithResponseValue_error
  , setupPIN
  , setSetupPIN
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , setupPINSelector
  , setSetupPINSelector
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

-- | Initialize an MTRAccountLoginClusterGetSetupPINResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRAccountLoginClusterGetSetupPINResponseParams mtrAccountLoginClusterGetSetupPINResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrAccountLoginClusterGetSetupPINResponseParams -> responseValue -> error_ -> IO (Id MTRAccountLoginClusterGetSetupPINResponseParams)
initWithResponseValue_error mtrAccountLoginClusterGetSetupPINResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrAccountLoginClusterGetSetupPINResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- setupPIN@
setupPIN :: IsMTRAccountLoginClusterGetSetupPINResponseParams mtrAccountLoginClusterGetSetupPINResponseParams => mtrAccountLoginClusterGetSetupPINResponseParams -> IO (Id NSString)
setupPIN mtrAccountLoginClusterGetSetupPINResponseParams  =
    sendMsg mtrAccountLoginClusterGetSetupPINResponseParams (mkSelector "setupPIN") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSetupPIN:@
setSetupPIN :: (IsMTRAccountLoginClusterGetSetupPINResponseParams mtrAccountLoginClusterGetSetupPINResponseParams, IsNSString value) => mtrAccountLoginClusterGetSetupPINResponseParams -> value -> IO ()
setSetupPIN mtrAccountLoginClusterGetSetupPINResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterGetSetupPINResponseParams (mkSelector "setSetupPIN:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRAccountLoginClusterGetSetupPINResponseParams mtrAccountLoginClusterGetSetupPINResponseParams => mtrAccountLoginClusterGetSetupPINResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrAccountLoginClusterGetSetupPINResponseParams  =
    sendMsg mtrAccountLoginClusterGetSetupPINResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRAccountLoginClusterGetSetupPINResponseParams mtrAccountLoginClusterGetSetupPINResponseParams, IsNSNumber value) => mtrAccountLoginClusterGetSetupPINResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrAccountLoginClusterGetSetupPINResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterGetSetupPINResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @setupPIN@
setupPINSelector :: Selector
setupPINSelector = mkSelector "setupPIN"

-- | @Selector@ for @setSetupPIN:@
setSetupPINSelector :: Selector
setSetupPINSelector = mkSelector "setSetupPIN:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

