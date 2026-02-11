{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestNullableOptionalResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestNullableOptionalResponseParams
  ( MTRUnitTestingClusterTestNullableOptionalResponseParams
  , IsMTRUnitTestingClusterTestNullableOptionalResponseParams(..)
  , initWithResponseValue_error
  , wasPresent
  , setWasPresent
  , wasNull
  , setWasNull
  , value
  , setValue
  , originalValue
  , setOriginalValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , wasPresentSelector
  , setWasPresentSelector
  , wasNullSelector
  , setWasNullSelector
  , valueSelector
  , setValueSelector
  , originalValueSelector
  , setOriginalValueSelector
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

-- | Initialize an MTRUnitTestingClusterTestNullableOptionalResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestNullableOptionalResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestNullableOptionalResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- wasPresent@
wasPresent :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasPresent mtrUnitTestingClusterTestNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "wasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWasPresent:@
setWasPresent :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasPresent mtrUnitTestingClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "setWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wasNull@
wasNull :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasNull mtrUnitTestingClusterTestNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "wasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWasNull:@
setWasNull :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasNull mtrUnitTestingClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "setWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
value mtrUnitTestingClusterTestNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setValue mtrUnitTestingClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originalValue@
originalValue :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
originalValue mtrUnitTestingClusterTestNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "originalValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginalValue:@
setOriginalValue :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setOriginalValue mtrUnitTestingClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "setOriginalValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestNullableOptionalResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @wasPresent@
wasPresentSelector :: Selector
wasPresentSelector = mkSelector "wasPresent"

-- | @Selector@ for @setWasPresent:@
setWasPresentSelector :: Selector
setWasPresentSelector = mkSelector "setWasPresent:"

-- | @Selector@ for @wasNull@
wasNullSelector :: Selector
wasNullSelector = mkSelector "wasNull"

-- | @Selector@ for @setWasNull:@
setWasNullSelector :: Selector
setWasNullSelector = mkSelector "setWasNull:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @originalValue@
originalValueSelector :: Selector
originalValueSelector = mkSelector "originalValue"

-- | @Selector@ for @setOriginalValue:@
setOriginalValueSelector :: Selector
setOriginalValueSelector = mkSelector "setOriginalValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

