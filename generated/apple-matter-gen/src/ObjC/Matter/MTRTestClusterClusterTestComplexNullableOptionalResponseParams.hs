{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestComplexNullableOptionalResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestComplexNullableOptionalResponseParams
  ( MTRTestClusterClusterTestComplexNullableOptionalResponseParams
  , IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams(..)
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

-- | @- nullableIntWasNull@
nullableIntWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableIntWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableIntWasNull:@
setNullableIntWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableIntWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableIntValue@
nullableIntValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableIntValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableIntValue:@
setNullableIntValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableIntValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalIntWasPresent@
optionalIntWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalIntWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalIntWasPresent:@
setOptionalIntWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalIntWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalIntValue@
optionalIntValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalIntValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalIntValue:@
setOptionalIntValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalIntValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalIntWasPresent@
nullableOptionalIntWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalIntWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalIntWasPresent:@
setNullableOptionalIntWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalIntWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalIntWasNull@
nullableOptionalIntWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalIntWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalIntWasNull:@
setNullableOptionalIntWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalIntWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalIntValue@
nullableOptionalIntValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalIntValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalIntValue:@
setNullableOptionalIntValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalIntValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStringWasNull@
nullableStringWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStringWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStringWasNull:@
setNullableStringWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStringWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStringValue@
nullableStringValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStringValue:@
setNullableStringValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStringWasPresent@
optionalStringWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStringWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStringWasPresent:@
setOptionalStringWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStringWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStringValue@
optionalStringValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
optionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStringValue:@
setOptionalStringValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStringWasPresent@
nullableOptionalStringWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStringWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStringWasPresent:@
setNullableOptionalStringWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStringWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStringWasNull@
nullableOptionalStringWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStringWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStringWasNull:@
setNullableOptionalStringWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStringWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStringValue@
nullableOptionalStringValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableOptionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStringValue:@
setNullableOptionalStringValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStructWasNull@
nullableStructWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStructWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStructWasNull:@
setNullableStructWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStructWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStructValue@
nullableStructValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableStructValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStructValue:@
setNullableStructValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableStructValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStructWasPresent@
optionalStructWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStructWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStructWasPresent:@
setOptionalStructWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStructWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStructValue@
optionalStructValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalStructValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStructValue:@
setOptionalStructValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalStructValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStructWasPresent@
nullableOptionalStructWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStructWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStructWasPresent:@
setNullableOptionalStructWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStructWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStructWasNull@
nullableOptionalStructWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStructWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStructWasNull:@
setNullableOptionalStructWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStructWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStructValue@
nullableOptionalStructValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalStructValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStructValue:@
setNullableOptionalStructValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalStructValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableListWasNull@
nullableListWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableListWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableListWasNull:@
setNullableListWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableListWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableListValue@
nullableListValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableListValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableListValue:@
setNullableListValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableListValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalListWasPresent@
optionalListWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalListWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalListWasPresent:@
setOptionalListWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalListWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalListValue@
optionalListValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
optionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "optionalListValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalListValue:@
setOptionalListValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setOptionalListValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalListWasPresent@
nullableOptionalListWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalListWasPresent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalListWasPresent:@
setNullableOptionalListWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalListWasPresent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalListWasNull@
nullableOptionalListWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalListWasNull") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalListWasNull:@
setNullableOptionalListWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalListWasNull:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalListValue@
nullableOptionalListValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableOptionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "nullableOptionalListValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalListValue:@
setNullableOptionalListValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setNullableOptionalListValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalResponseParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalResponseParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

