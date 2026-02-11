{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestComplexNullableOptionalRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestComplexNullableOptionalRequestParams
  ( MTRTestClusterClusterTestComplexNullableOptionalRequestParams
  , IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams(..)
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
nullableInt :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableInt:@
setNullableInt :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalInt@
optionalInt :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
optionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableOptionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableString@
nullableString :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableString mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableString:@
setNullableString :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableString mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalString@
optionalString :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
optionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalString:@
setOptionalString :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableOptionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStruct@
nullableStruct :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStruct@
optionalStruct :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableList@
nullableList :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableList mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableList:@
setNullableList :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableList mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalList@
optionalList :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
optionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "optionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalList:@
setOptionalList :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableOptionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "nullableOptionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setNullableOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestComplexNullableOptionalRequestParams  =
    sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestComplexNullableOptionalRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestComplexNullableOptionalRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

