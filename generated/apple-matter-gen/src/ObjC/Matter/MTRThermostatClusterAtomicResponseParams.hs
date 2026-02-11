{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAtomicResponseParams@.
module ObjC.Matter.MTRThermostatClusterAtomicResponseParams
  ( MTRThermostatClusterAtomicResponseParams
  , IsMTRThermostatClusterAtomicResponseParams(..)
  , initWithResponseValue_error
  , statusCode
  , setStatusCode
  , attributeStatus
  , setAttributeStatus
  , timeout
  , setTimeout
  , initWithResponseValue_errorSelector
  , statusCodeSelector
  , setStatusCodeSelector
  , attributeStatusSelector
  , setAttributeStatusSelector
  , timeoutSelector
  , setTimeoutSelector


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

-- | Initialize an MTRThermostatClusterAtomicResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThermostatClusterAtomicResponseParams -> responseValue -> error_ -> IO (Id MTRThermostatClusterAtomicResponseParams)
initWithResponseValue_error mtrThermostatClusterAtomicResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- statusCode@
statusCode :: IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams => mtrThermostatClusterAtomicResponseParams -> IO (Id NSNumber)
statusCode mtrThermostatClusterAtomicResponseParams  =
    sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "statusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusCode:@
setStatusCode :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSNumber value) => mtrThermostatClusterAtomicResponseParams -> value -> IO ()
setStatusCode mtrThermostatClusterAtomicResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "setStatusCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributeStatus@
attributeStatus :: IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams => mtrThermostatClusterAtomicResponseParams -> IO (Id NSArray)
attributeStatus mtrThermostatClusterAtomicResponseParams  =
    sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "attributeStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeStatus:@
setAttributeStatus :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSArray value) => mtrThermostatClusterAtomicResponseParams -> value -> IO ()
setAttributeStatus mtrThermostatClusterAtomicResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "setAttributeStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeout@
timeout :: IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams => mtrThermostatClusterAtomicResponseParams -> IO (Id NSNumber)
timeout mtrThermostatClusterAtomicResponseParams  =
    sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "timeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimeout:@
setTimeout :: (IsMTRThermostatClusterAtomicResponseParams mtrThermostatClusterAtomicResponseParams, IsNSNumber value) => mtrThermostatClusterAtomicResponseParams -> value -> IO ()
setTimeout mtrThermostatClusterAtomicResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThermostatClusterAtomicResponseParams (mkSelector "setTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector
setStatusCodeSelector = mkSelector "setStatusCode:"

-- | @Selector@ for @attributeStatus@
attributeStatusSelector :: Selector
attributeStatusSelector = mkSelector "attributeStatus"

-- | @Selector@ for @setAttributeStatus:@
setAttributeStatusSelector :: Selector
setAttributeStatusSelector = mkSelector "setAttributeStatus:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector
setTimeoutSelector = mkSelector "setTimeout:"

