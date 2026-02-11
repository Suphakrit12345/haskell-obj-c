{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestNullableOptionalResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestNullableOptionalResponseParams
  ( MTRTestClusterClusterTestNullableOptionalResponseParams
  , IsMTRTestClusterClusterTestNullableOptionalResponseParams(..)
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

-- | @- wasPresent@
wasPresent :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasPresent mtrTestClusterClusterTestNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "wasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWasPresent:@
setWasPresent :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasPresent mtrTestClusterClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "setWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- wasNull@
wasNull :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasNull mtrTestClusterClusterTestNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "wasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWasNull:@
setWasNull :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasNull mtrTestClusterClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "setWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
value mtrTestClusterClusterTestNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setValue mtrTestClusterClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originalValue@
originalValue :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
originalValue mtrTestClusterClusterTestNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "originalValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOriginalValue:@
setOriginalValue :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setOriginalValue mtrTestClusterClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "setOriginalValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestNullableOptionalResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

