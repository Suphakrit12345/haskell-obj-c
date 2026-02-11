{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestComplexNullableOptionalRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestComplexNullableOptionalRequestParams
  ( MTRUnitTestingClusterTestComplexNullableOptionalRequestParams
  , IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams(..)
  , nullableInt
  , setNullableInt
  , optionalInt
  , setOptionalInt
  , nullableOptionalInt
  , setNullableOptionalInt
  , nullableString
  , setNullableString
  , optionalString
  , setOptionalString
  , nullableOptionalString
  , setNullableOptionalString
  , nullableStruct
  , setNullableStruct
  , optionalStruct
  , setOptionalStruct
  , nullableOptionalStruct
  , setNullableOptionalStruct
  , nullableList
  , setNullableList
  , optionalList
  , setOptionalList
  , nullableOptionalList
  , setNullableOptionalList
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nullableIntSelector
  , setNullableIntSelector
  , optionalIntSelector
  , setOptionalIntSelector
  , nullableOptionalIntSelector
  , setNullableOptionalIntSelector
  , nullableStringSelector
  , setNullableStringSelector
  , optionalStringSelector
  , setOptionalStringSelector
  , nullableOptionalStringSelector
  , setNullableOptionalStringSelector
  , nullableStructSelector
  , setNullableStructSelector
  , optionalStructSelector
  , setOptionalStructSelector
  , nullableOptionalStructSelector
  , setNullableOptionalStructSelector
  , nullableListSelector
  , setNullableListSelector
  , optionalListSelector
  , setOptionalListSelector
  , nullableOptionalListSelector
  , setNullableOptionalListSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- nullableInt@
nullableInt :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableInt:@
setNullableInt :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalInt@
optionalInt :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
optionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableOptionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableString@
nullableString :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableString:@
setNullableString :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalString@
optionalString :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
optionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalString:@
setOptionalString :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableOptionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStruct@
nullableStruct :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStruct@
optionalStruct :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableList@
nullableList :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableList:@
setNullableList :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalList@
optionalList :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
optionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalList:@
setOptionalList :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableOptionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestComplexNullableOptionalRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nullableInt@
nullableIntSelector :: Selector
nullableIntSelector = mkSelector "nullableInt"

-- | @Selector@ for @setNullableInt:@
setNullableIntSelector :: Selector
setNullableIntSelector = mkSelector "setNullableInt:"

-- | @Selector@ for @optionalInt@
optionalIntSelector :: Selector
optionalIntSelector = mkSelector "optionalInt"

-- | @Selector@ for @setOptionalInt:@
setOptionalIntSelector :: Selector
setOptionalIntSelector = mkSelector "setOptionalInt:"

-- | @Selector@ for @nullableOptionalInt@
nullableOptionalIntSelector :: Selector
nullableOptionalIntSelector = mkSelector "nullableOptionalInt"

-- | @Selector@ for @setNullableOptionalInt:@
setNullableOptionalIntSelector :: Selector
setNullableOptionalIntSelector = mkSelector "setNullableOptionalInt:"

-- | @Selector@ for @nullableString@
nullableStringSelector :: Selector
nullableStringSelector = mkSelector "nullableString"

-- | @Selector@ for @setNullableString:@
setNullableStringSelector :: Selector
setNullableStringSelector = mkSelector "setNullableString:"

-- | @Selector@ for @optionalString@
optionalStringSelector :: Selector
optionalStringSelector = mkSelector "optionalString"

-- | @Selector@ for @setOptionalString:@
setOptionalStringSelector :: Selector
setOptionalStringSelector = mkSelector "setOptionalString:"

-- | @Selector@ for @nullableOptionalString@
nullableOptionalStringSelector :: Selector
nullableOptionalStringSelector = mkSelector "nullableOptionalString"

-- | @Selector@ for @setNullableOptionalString:@
setNullableOptionalStringSelector :: Selector
setNullableOptionalStringSelector = mkSelector "setNullableOptionalString:"

-- | @Selector@ for @nullableStruct@
nullableStructSelector :: Selector
nullableStructSelector = mkSelector "nullableStruct"

-- | @Selector@ for @setNullableStruct:@
setNullableStructSelector :: Selector
setNullableStructSelector = mkSelector "setNullableStruct:"

-- | @Selector@ for @optionalStruct@
optionalStructSelector :: Selector
optionalStructSelector = mkSelector "optionalStruct"

-- | @Selector@ for @setOptionalStruct:@
setOptionalStructSelector :: Selector
setOptionalStructSelector = mkSelector "setOptionalStruct:"

-- | @Selector@ for @nullableOptionalStruct@
nullableOptionalStructSelector :: Selector
nullableOptionalStructSelector = mkSelector "nullableOptionalStruct"

-- | @Selector@ for @setNullableOptionalStruct:@
setNullableOptionalStructSelector :: Selector
setNullableOptionalStructSelector = mkSelector "setNullableOptionalStruct:"

-- | @Selector@ for @nullableList@
nullableListSelector :: Selector
nullableListSelector = mkSelector "nullableList"

-- | @Selector@ for @setNullableList:@
setNullableListSelector :: Selector
setNullableListSelector = mkSelector "setNullableList:"

-- | @Selector@ for @optionalList@
optionalListSelector :: Selector
optionalListSelector = mkSelector "optionalList"

-- | @Selector@ for @setOptionalList:@
setOptionalListSelector :: Selector
setOptionalListSelector = mkSelector "setOptionalList:"

-- | @Selector@ for @nullableOptionalList@
nullableOptionalListSelector :: Selector
nullableOptionalListSelector = mkSelector "nullableOptionalList"

-- | @Selector@ for @setNullableOptionalList:@
setNullableOptionalListSelector :: Selector
setNullableOptionalListSelector = mkSelector "setNullableOptionalList:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

