{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterSetCredentialResponseParams@.
module ObjC.Matter.MTRDoorLockClusterSetCredentialResponseParams
  ( MTRDoorLockClusterSetCredentialResponseParams
  , IsMTRDoorLockClusterSetCredentialResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , userIndex
  , setUserIndex
  , nextCredentialIndex
  , setNextCredentialIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , userIndexSelector
  , setUserIndexSelector
  , nextCredentialIndexSelector
  , setNextCredentialIndexSelector
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

-- | Initialize an MTRDoorLockClusterSetCredentialResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDoorLockClusterSetCredentialResponseParams -> responseValue -> error_ -> IO (Id MTRDoorLockClusterSetCredentialResponseParams)
initWithResponseValue_error mtrDoorLockClusterSetCredentialResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
status mtrDoorLockClusterSetCredentialResponseParams  =
    sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setStatus mtrDoorLockClusterSetCredentialResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
userIndex mtrDoorLockClusterSetCredentialResponseParams  =
    sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "userIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setUserIndex mtrDoorLockClusterSetCredentialResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "setUserIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nextCredentialIndex@
nextCredentialIndex :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
nextCredentialIndex mtrDoorLockClusterSetCredentialResponseParams  =
    sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "nextCredentialIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextCredentialIndex:@
setNextCredentialIndex :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setNextCredentialIndex mtrDoorLockClusterSetCredentialResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "setNextCredentialIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams => mtrDoorLockClusterSetCredentialResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterSetCredentialResponseParams  =
    sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterSetCredentialResponseParams mtrDoorLockClusterSetCredentialResponseParams, IsNSNumber value) => mtrDoorLockClusterSetCredentialResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterSetCredentialResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterSetCredentialResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @nextCredentialIndex@
nextCredentialIndexSelector :: Selector
nextCredentialIndexSelector = mkSelector "nextCredentialIndex"

-- | @Selector@ for @setNextCredentialIndex:@
setNextCredentialIndexSelector :: Selector
setNextCredentialIndexSelector = mkSelector "setNextCredentialIndex:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

