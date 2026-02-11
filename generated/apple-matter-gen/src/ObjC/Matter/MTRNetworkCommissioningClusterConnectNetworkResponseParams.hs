{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterConnectNetworkResponseParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterConnectNetworkResponseParams
  ( MTRNetworkCommissioningClusterConnectNetworkResponseParams
  , IsMTRNetworkCommissioningClusterConnectNetworkResponseParams(..)
  , initWithResponseValue_error
  , networkingStatus
  , setNetworkingStatus
  , debugText
  , setDebugText
  , errorValue
  , setErrorValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , networkingStatusSelector
  , setNetworkingStatusSelector
  , debugTextSelector
  , setDebugTextSelector
  , errorValueSelector
  , setErrorValueSelector
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

-- | Initialize an MTRNetworkCommissioningClusterConnectNetworkResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> responseValue -> error_ -> IO (Id MTRNetworkCommissioningClusterConnectNetworkResponseParams)
initWithResponseValue_error mtrNetworkCommissioningClusterConnectNetworkResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- networkingStatus@
networkingStatus :: IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> IO (Id NSNumber)
networkingStatus mtrNetworkCommissioningClusterConnectNetworkResponseParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "networkingStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkingStatus:@
setNetworkingStatus :: (IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> value -> IO ()
setNetworkingStatus mtrNetworkCommissioningClusterConnectNetworkResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "setNetworkingStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- debugText@
debugText :: IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> IO (Id NSString)
debugText mtrNetworkCommissioningClusterConnectNetworkResponseParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "debugText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDebugText:@
setDebugText :: (IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams, IsNSString value) => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> value -> IO ()
setDebugText mtrNetworkCommissioningClusterConnectNetworkResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "setDebugText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorValue@
errorValue :: IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> IO (Id NSNumber)
errorValue mtrNetworkCommissioningClusterConnectNetworkResponseParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "errorValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorValue:@
setErrorValue :: (IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> value -> IO ()
setErrorValue mtrNetworkCommissioningClusterConnectNetworkResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "setErrorValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterConnectNetworkResponseParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterConnectNetworkResponseParams mtrNetworkCommissioningClusterConnectNetworkResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterConnectNetworkResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @networkingStatus@
networkingStatusSelector :: Selector
networkingStatusSelector = mkSelector "networkingStatus"

-- | @Selector@ for @setNetworkingStatus:@
setNetworkingStatusSelector :: Selector
setNetworkingStatusSelector = mkSelector "setNetworkingStatus:"

-- | @Selector@ for @debugText@
debugTextSelector :: Selector
debugTextSelector = mkSelector "debugText"

-- | @Selector@ for @setDebugText:@
setDebugTextSelector :: Selector
setDebugTextSelector = mkSelector "setDebugText:"

-- | @Selector@ for @errorValue@
errorValueSelector :: Selector
errorValueSelector = mkSelector "errorValue"

-- | @Selector@ for @setErrorValue:@
setErrorValueSelector :: Selector
setErrorValueSelector = mkSelector "setErrorValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

