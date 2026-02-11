{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterNullablesAndOptionalsStruct@.
module ObjC.Matter.MTRUnitTestingClusterNullablesAndOptionalsStruct
  ( MTRUnitTestingClusterNullablesAndOptionalsStruct
  , IsMTRUnitTestingClusterNullablesAndOptionalsStruct(..)
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
nullableInt :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableInt mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableInt:@
setNullableInt :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableInt mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalInt@
optionalInt :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
optionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "optionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableOptionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableString@
nullableString :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableString mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableString:@
setNullableString :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSString value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableString mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalString@
optionalString :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSString)
optionalString mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "optionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalString:@
setOptionalString :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSString value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalString mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableOptionalString mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSString value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalString mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStruct@
nullableStruct :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStruct mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableStruct mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStruct@
optionalStruct :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "optionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableList@
nullableList :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableList mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableList:@
setNullableList :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableList mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalList@
optionalList :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
optionalList mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "optionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalList:@
setOptionalList :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalList mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableOptionalList mtrUnitTestingClusterNullablesAndOptionalsStruct  =
    sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalList mtrUnitTestingClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

