{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLauncherResponseParams@.
module ObjC.Matter.MTRContentLauncherClusterLauncherResponseParams
  ( MTRContentLauncherClusterLauncherResponseParams
  , IsMTRContentLauncherClusterLauncherResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , dataSelector
  , setDataSelector
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

-- | Initialize an MTRContentLauncherClusterLauncherResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrContentLauncherClusterLauncherResponseParams -> responseValue -> error_ -> IO (Id MTRContentLauncherClusterLauncherResponseParams)
initWithResponseValue_error mtrContentLauncherClusterLauncherResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams => mtrContentLauncherClusterLauncherResponseParams -> IO (Id NSNumber)
status mtrContentLauncherClusterLauncherResponseParams  =
    sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams, IsNSNumber value) => mtrContentLauncherClusterLauncherResponseParams -> value -> IO ()
setStatus mtrContentLauncherClusterLauncherResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams => mtrContentLauncherClusterLauncherResponseParams -> IO (Id NSString)
data_ mtrContentLauncherClusterLauncherResponseParams  =
    sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams, IsNSString value) => mtrContentLauncherClusterLauncherResponseParams -> value -> IO ()
setData mtrContentLauncherClusterLauncherResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams => mtrContentLauncherClusterLauncherResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLauncherResponseParams  =
    sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLauncherResponseParams mtrContentLauncherClusterLauncherResponseParams, IsNSNumber value) => mtrContentLauncherClusterLauncherResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLauncherResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterLauncherResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

