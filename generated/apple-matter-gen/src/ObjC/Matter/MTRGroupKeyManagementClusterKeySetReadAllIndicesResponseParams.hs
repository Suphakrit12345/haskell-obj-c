{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams@.
module ObjC.Matter.MTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams
  ( MTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams
  , IsMTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams(..)
  , initWithResponseValue_error
  , groupKeySetIDs
  , setGroupKeySetIDs
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , groupKeySetIDsSelector
  , setGroupKeySetIDsSelector
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

-- | Initialize an MTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams -> responseValue -> error_ -> IO (Id MTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams)
initWithResponseValue_error mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- groupKeySetIDs@
groupKeySetIDs :: IsMTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams => mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams -> IO (Id NSArray)
groupKeySetIDs mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams  =
    sendMsg mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams (mkSelector "groupKeySetIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupKeySetIDs:@
setGroupKeySetIDs :: (IsMTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams, IsNSArray value) => mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams -> value -> IO ()
setGroupKeySetIDs mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams (mkSelector "setGroupKeySetIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams => mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams  =
    sendMsg mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupKeyManagementClusterKeySetReadAllIndicesResponseParams mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGroupKeyManagementClusterKeySetReadAllIndicesResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @groupKeySetIDs@
groupKeySetIDsSelector :: Selector
groupKeySetIDsSelector = mkSelector "groupKeySetIDs"

-- | @Selector@ for @setGroupKeySetIDs:@
setGroupKeySetIDsSelector :: Selector
setGroupKeySetIDsSelector = mkSelector "setGroupKeySetIDs:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

