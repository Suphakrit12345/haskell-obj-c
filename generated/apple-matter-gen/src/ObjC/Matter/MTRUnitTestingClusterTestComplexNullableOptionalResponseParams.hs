{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestComplexNullableOptionalResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestComplexNullableOptionalResponseParams
  ( MTRUnitTestingClusterTestComplexNullableOptionalResponseParams
  , IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams(..)
  , initWithResponseValue_error
  , nullableIntWasNull
  , setNullableIntWasNull
  , nullableIntValue
  , setNullableIntValue
  , optionalIntWasPresent
  , setOptionalIntWasPresent
  , optionalIntValue
  , setOptionalIntValue
  , nullableOptionalIntWasPresent
  , setNullableOptionalIntWasPresent
  , nullableOptionalIntWasNull
  , setNullableOptionalIntWasNull
  , nullableOptionalIntValue
  , setNullableOptionalIntValue
  , nullableStringWasNull
  , setNullableStringWasNull
  , nullableStringValue
  , setNullableStringValue
  , optionalStringWasPresent
  , setOptionalStringWasPresent
  , optionalStringValue
  , setOptionalStringValue
  , nullableOptionalStringWasPresent
  , setNullableOptionalStringWasPresent
  , nullableOptionalStringWasNull
  , setNullableOptionalStringWasNull
  , nullableOptionalStringValue
  , setNullableOptionalStringValue
  , nullableStructWasNull
  , setNullableStructWasNull
  , nullableStructValue
  , setNullableStructValue
  , optionalStructWasPresent
  , setOptionalStructWasPresent
  , optionalStructValue
  , setOptionalStructValue
  , nullableOptionalStructWasPresent
  , setNullableOptionalStructWasPresent
  , nullableOptionalStructWasNull
  , setNullableOptionalStructWasNull
  , nullableOptionalStructValue
  , setNullableOptionalStructValue
  , nullableListWasNull
  , setNullableListWasNull
  , nullableListValue
  , setNullableListValue
  , optionalListWasPresent
  , setOptionalListWasPresent
  , optionalListValue
  , setOptionalListValue
  , nullableOptionalListWasPresent
  , setNullableOptionalListWasPresent
  , nullableOptionalListWasNull
  , setNullableOptionalListWasNull
  , nullableOptionalListValue
  , setNullableOptionalListValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , nullableIntWasNullSelector
  , setNullableIntWasNullSelector
  , nullableIntValueSelector
  , setNullableIntValueSelector
  , optionalIntWasPresentSelector
  , setOptionalIntWasPresentSelector
  , optionalIntValueSelector
  , setOptionalIntValueSelector
  , nullableOptionalIntWasPresentSelector
  , setNullableOptionalIntWasPresentSelector
  , nullableOptionalIntWasNullSelector
  , setNullableOptionalIntWasNullSelector
  , nullableOptionalIntValueSelector
  , setNullableOptionalIntValueSelector
  , nullableStringWasNullSelector
  , setNullableStringWasNullSelector
  , nullableStringValueSelector
  , setNullableStringValueSelector
  , optionalStringWasPresentSelector
  , setOptionalStringWasPresentSelector
  , optionalStringValueSelector
  , setOptionalStringValueSelector
  , nullableOptionalStringWasPresentSelector
  , setNullableOptionalStringWasPresentSelector
  , nullableOptionalStringWasNullSelector
  , setNullableOptionalStringWasNullSelector
  , nullableOptionalStringValueSelector
  , setNullableOptionalStringValueSelector
  , nullableStructWasNullSelector
  , setNullableStructWasNullSelector
  , nullableStructValueSelector
  , setNullableStructValueSelector
  , optionalStructWasPresentSelector
  , setOptionalStructWasPresentSelector
  , optionalStructValueSelector
  , setOptionalStructValueSelector
  , nullableOptionalStructWasPresentSelector
  , setNullableOptionalStructWasPresentSelector
  , nullableOptionalStructWasNullSelector
  , setNullableOptionalStructWasNullSelector
  , nullableOptionalStructValueSelector
  , setNullableOptionalStructValueSelector
  , nullableListWasNullSelector
  , setNullableListWasNullSelector
  , nullableListValueSelector
  , setNullableListValueSelector
  , optionalListWasPresentSelector
  , setOptionalListWasPresentSelector
  , optionalListValueSelector
  , setOptionalListValueSelector
  , nullableOptionalListWasPresentSelector
  , setNullableOptionalListWasPresentSelector
  , nullableOptionalListWasNullSelector
  , setNullableOptionalListWasNullSelector
  , nullableOptionalListValueSelector
  , setNullableOptionalListValueSelector
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

-- | Initialize an MTRUnitTestingClusterTestComplexNullableOptionalResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestComplexNullableOptionalResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- nullableIntWasNull@
nullableIntWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableIntWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableIntWasNull:@
setNullableIntWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableIntWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableIntValue@
nullableIntValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableIntValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableIntValue:@
setNullableIntValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableIntValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalIntWasPresent@
optionalIntWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalIntWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalIntWasPresent:@
setOptionalIntWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalIntWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalIntValue@
optionalIntValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalIntValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalIntValue:@
setOptionalIntValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalIntValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalIntWasPresent@
nullableOptionalIntWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalIntWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalIntWasPresent:@
setNullableOptionalIntWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalIntWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalIntWasNull@
nullableOptionalIntWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalIntWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalIntWasNull:@
setNullableOptionalIntWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalIntWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalIntValue@
nullableOptionalIntValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalIntValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalIntValue:@
setNullableOptionalIntValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalIntValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStringWasNull@
nullableStringWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStringWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStringWasNull:@
setNullableStringWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStringWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStringValue@
nullableStringValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStringValue:@
setNullableStringValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStringWasPresent@
optionalStringWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStringWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStringWasPresent:@
setOptionalStringWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStringWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStringValue@
optionalStringValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
optionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStringValue:@
setOptionalStringValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStringWasPresent@
nullableOptionalStringWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStringWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStringWasPresent:@
setNullableOptionalStringWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStringWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStringWasNull@
nullableOptionalStringWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStringWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStringWasNull:@
setNullableOptionalStringWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStringWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStringValue@
nullableOptionalStringValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableOptionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStringValue:@
setNullableOptionalStringValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStructWasNull@
nullableStructWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStructWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStructWasNull:@
setNullableStructWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStructWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStructValue@
nullableStructValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStructValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStructValue:@
setNullableStructValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStructValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStructWasPresent@
optionalStructWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStructWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStructWasPresent:@
setOptionalStructWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStructWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStructValue@
optionalStructValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStructValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStructValue:@
setOptionalStructValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStructValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStructWasPresent@
nullableOptionalStructWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStructWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStructWasPresent:@
setNullableOptionalStructWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStructWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStructWasNull@
nullableOptionalStructWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStructWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStructWasNull:@
setNullableOptionalStructWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStructWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStructValue@
nullableOptionalStructValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStructValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStructValue:@
setNullableOptionalStructValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStructValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableListWasNull@
nullableListWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableListWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableListWasNull:@
setNullableListWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableListWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableListValue@
nullableListValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableListValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableListValue:@
setNullableListValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableListValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalListWasPresent@
optionalListWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalListWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalListWasPresent:@
setOptionalListWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalListWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalListValue@
optionalListValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
optionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalListValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalListValue:@
setOptionalListValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalListValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalListWasPresent@
nullableOptionalListWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalListWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalListWasPresent:@
setNullableOptionalListWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalListWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalListWasNull@
nullableOptionalListWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalListWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalListWasNull:@
setNullableOptionalListWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalListWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalListValue@
nullableOptionalListValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableOptionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalListValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalListValue:@
setNullableOptionalListValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalListValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @nullableIntWasNull@
nullableIntWasNullSelector :: Selector
nullableIntWasNullSelector = mkSelector "nullableIntWasNull"

-- | @Selector@ for @setNullableIntWasNull:@
setNullableIntWasNullSelector :: Selector
setNullableIntWasNullSelector = mkSelector "setNullableIntWasNull:"

-- | @Selector@ for @nullableIntValue@
nullableIntValueSelector :: Selector
nullableIntValueSelector = mkSelector "nullableIntValue"

-- | @Selector@ for @setNullableIntValue:@
setNullableIntValueSelector :: Selector
setNullableIntValueSelector = mkSelector "setNullableIntValue:"

-- | @Selector@ for @optionalIntWasPresent@
optionalIntWasPresentSelector :: Selector
optionalIntWasPresentSelector = mkSelector "optionalIntWasPresent"

-- | @Selector@ for @setOptionalIntWasPresent:@
setOptionalIntWasPresentSelector :: Selector
setOptionalIntWasPresentSelector = mkSelector "setOptionalIntWasPresent:"

-- | @Selector@ for @optionalIntValue@
optionalIntValueSelector :: Selector
optionalIntValueSelector = mkSelector "optionalIntValue"

-- | @Selector@ for @setOptionalIntValue:@
setOptionalIntValueSelector :: Selector
setOptionalIntValueSelector = mkSelector "setOptionalIntValue:"

-- | @Selector@ for @nullableOptionalIntWasPresent@
nullableOptionalIntWasPresentSelector :: Selector
nullableOptionalIntWasPresentSelector = mkSelector "nullableOptionalIntWasPresent"

-- | @Selector@ for @setNullableOptionalIntWasPresent:@
setNullableOptionalIntWasPresentSelector :: Selector
setNullableOptionalIntWasPresentSelector = mkSelector "setNullableOptionalIntWasPresent:"

-- | @Selector@ for @nullableOptionalIntWasNull@
nullableOptionalIntWasNullSelector :: Selector
nullableOptionalIntWasNullSelector = mkSelector "nullableOptionalIntWasNull"

-- | @Selector@ for @setNullableOptionalIntWasNull:@
setNullableOptionalIntWasNullSelector :: Selector
setNullableOptionalIntWasNullSelector = mkSelector "setNullableOptionalIntWasNull:"

-- | @Selector@ for @nullableOptionalIntValue@
nullableOptionalIntValueSelector :: Selector
nullableOptionalIntValueSelector = mkSelector "nullableOptionalIntValue"

-- | @Selector@ for @setNullableOptionalIntValue:@
setNullableOptionalIntValueSelector :: Selector
setNullableOptionalIntValueSelector = mkSelector "setNullableOptionalIntValue:"

-- | @Selector@ for @nullableStringWasNull@
nullableStringWasNullSelector :: Selector
nullableStringWasNullSelector = mkSelector "nullableStringWasNull"

-- | @Selector@ for @setNullableStringWasNull:@
setNullableStringWasNullSelector :: Selector
setNullableStringWasNullSelector = mkSelector "setNullableStringWasNull:"

-- | @Selector@ for @nullableStringValue@
nullableStringValueSelector :: Selector
nullableStringValueSelector = mkSelector "nullableStringValue"

-- | @Selector@ for @setNullableStringValue:@
setNullableStringValueSelector :: Selector
setNullableStringValueSelector = mkSelector "setNullableStringValue:"

-- | @Selector@ for @optionalStringWasPresent@
optionalStringWasPresentSelector :: Selector
optionalStringWasPresentSelector = mkSelector "optionalStringWasPresent"

-- | @Selector@ for @setOptionalStringWasPresent:@
setOptionalStringWasPresentSelector :: Selector
setOptionalStringWasPresentSelector = mkSelector "setOptionalStringWasPresent:"

-- | @Selector@ for @optionalStringValue@
optionalStringValueSelector :: Selector
optionalStringValueSelector = mkSelector "optionalStringValue"

-- | @Selector@ for @setOptionalStringValue:@
setOptionalStringValueSelector :: Selector
setOptionalStringValueSelector = mkSelector "setOptionalStringValue:"

-- | @Selector@ for @nullableOptionalStringWasPresent@
nullableOptionalStringWasPresentSelector :: Selector
nullableOptionalStringWasPresentSelector = mkSelector "nullableOptionalStringWasPresent"

-- | @Selector@ for @setNullableOptionalStringWasPresent:@
setNullableOptionalStringWasPresentSelector :: Selector
setNullableOptionalStringWasPresentSelector = mkSelector "setNullableOptionalStringWasPresent:"

-- | @Selector@ for @nullableOptionalStringWasNull@
nullableOptionalStringWasNullSelector :: Selector
nullableOptionalStringWasNullSelector = mkSelector "nullableOptionalStringWasNull"

-- | @Selector@ for @setNullableOptionalStringWasNull:@
setNullableOptionalStringWasNullSelector :: Selector
setNullableOptionalStringWasNullSelector = mkSelector "setNullableOptionalStringWasNull:"

-- | @Selector@ for @nullableOptionalStringValue@
nullableOptionalStringValueSelector :: Selector
nullableOptionalStringValueSelector = mkSelector "nullableOptionalStringValue"

-- | @Selector@ for @setNullableOptionalStringValue:@
setNullableOptionalStringValueSelector :: Selector
setNullableOptionalStringValueSelector = mkSelector "setNullableOptionalStringValue:"

-- | @Selector@ for @nullableStructWasNull@
nullableStructWasNullSelector :: Selector
nullableStructWasNullSelector = mkSelector "nullableStructWasNull"

-- | @Selector@ for @setNullableStructWasNull:@
setNullableStructWasNullSelector :: Selector
setNullableStructWasNullSelector = mkSelector "setNullableStructWasNull:"

-- | @Selector@ for @nullableStructValue@
nullableStructValueSelector :: Selector
nullableStructValueSelector = mkSelector "nullableStructValue"

-- | @Selector@ for @setNullableStructValue:@
setNullableStructValueSelector :: Selector
setNullableStructValueSelector = mkSelector "setNullableStructValue:"

-- | @Selector@ for @optionalStructWasPresent@
optionalStructWasPresentSelector :: Selector
optionalStructWasPresentSelector = mkSelector "optionalStructWasPresent"

-- | @Selector@ for @setOptionalStructWasPresent:@
setOptionalStructWasPresentSelector :: Selector
setOptionalStructWasPresentSelector = mkSelector "setOptionalStructWasPresent:"

-- | @Selector@ for @optionalStructValue@
optionalStructValueSelector :: Selector
optionalStructValueSelector = mkSelector "optionalStructValue"

-- | @Selector@ for @setOptionalStructValue:@
setOptionalStructValueSelector :: Selector
setOptionalStructValueSelector = mkSelector "setOptionalStructValue:"

-- | @Selector@ for @nullableOptionalStructWasPresent@
nullableOptionalStructWasPresentSelector :: Selector
nullableOptionalStructWasPresentSelector = mkSelector "nullableOptionalStructWasPresent"

-- | @Selector@ for @setNullableOptionalStructWasPresent:@
setNullableOptionalStructWasPresentSelector :: Selector
setNullableOptionalStructWasPresentSelector = mkSelector "setNullableOptionalStructWasPresent:"

-- | @Selector@ for @nullableOptionalStructWasNull@
nullableOptionalStructWasNullSelector :: Selector
nullableOptionalStructWasNullSelector = mkSelector "nullableOptionalStructWasNull"

-- | @Selector@ for @setNullableOptionalStructWasNull:@
setNullableOptionalStructWasNullSelector :: Selector
setNullableOptionalStructWasNullSelector = mkSelector "setNullableOptionalStructWasNull:"

-- | @Selector@ for @nullableOptionalStructValue@
nullableOptionalStructValueSelector :: Selector
nullableOptionalStructValueSelector = mkSelector "nullableOptionalStructValue"

-- | @Selector@ for @setNullableOptionalStructValue:@
setNullableOptionalStructValueSelector :: Selector
setNullableOptionalStructValueSelector = mkSelector "setNullableOptionalStructValue:"

-- | @Selector@ for @nullableListWasNull@
nullableListWasNullSelector :: Selector
nullableListWasNullSelector = mkSelector "nullableListWasNull"

-- | @Selector@ for @setNullableListWasNull:@
setNullableListWasNullSelector :: Selector
setNullableListWasNullSelector = mkSelector "setNullableListWasNull:"

-- | @Selector@ for @nullableListValue@
nullableListValueSelector :: Selector
nullableListValueSelector = mkSelector "nullableListValue"

-- | @Selector@ for @setNullableListValue:@
setNullableListValueSelector :: Selector
setNullableListValueSelector = mkSelector "setNullableListValue:"

-- | @Selector@ for @optionalListWasPresent@
optionalListWasPresentSelector :: Selector
optionalListWasPresentSelector = mkSelector "optionalListWasPresent"

-- | @Selector@ for @setOptionalListWasPresent:@
setOptionalListWasPresentSelector :: Selector
setOptionalListWasPresentSelector = mkSelector "setOptionalListWasPresent:"

-- | @Selector@ for @optionalListValue@
optionalListValueSelector :: Selector
optionalListValueSelector = mkSelector "optionalListValue"

-- | @Selector@ for @setOptionalListValue:@
setOptionalListValueSelector :: Selector
setOptionalListValueSelector = mkSelector "setOptionalListValue:"

-- | @Selector@ for @nullableOptionalListWasPresent@
nullableOptionalListWasPresentSelector :: Selector
nullableOptionalListWasPresentSelector = mkSelector "nullableOptionalListWasPresent"

-- | @Selector@ for @setNullableOptionalListWasPresent:@
setNullableOptionalListWasPresentSelector :: Selector
setNullableOptionalListWasPresentSelector = mkSelector "setNullableOptionalListWasPresent:"

-- | @Selector@ for @nullableOptionalListWasNull@
nullableOptionalListWasNullSelector :: Selector
nullableOptionalListWasNullSelector = mkSelector "nullableOptionalListWasNull"

-- | @Selector@ for @setNullableOptionalListWasNull:@
setNullableOptionalListWasNullSelector :: Selector
setNullableOptionalListWasNullSelector = mkSelector "setNullableOptionalListWasNull:"

-- | @Selector@ for @nullableOptionalListValue@
nullableOptionalListValueSelector :: Selector
nullableOptionalListValueSelector = mkSelector "nullableOptionalListValue"

-- | @Selector@ for @setNullableOptionalListValue:@
setNullableOptionalListValueSelector :: Selector
setNullableOptionalListValueSelector = mkSelector "setNullableOptionalListValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

