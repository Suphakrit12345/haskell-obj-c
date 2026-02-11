{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterNullablesAndOptionalsStruct@.
module ObjC.Matter.MTRTestClusterClusterNullablesAndOptionalsStruct
  ( MTRTestClusterClusterNullablesAndOptionalsStruct
  , IsMTRTestClusterClusterNullablesAndOptionalsStruct(..)
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
nullableInt :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableInt mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableInt:@
setNullableInt :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableInt mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalInt@
optionalInt :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
optionalInt mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "optionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalInt mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableOptionalInt mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalInt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalInt mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalInt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableString@
nullableString :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableString mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableString:@
setNullableString :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSString value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableString mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalString@
optionalString :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSString)
optionalString mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "optionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalString:@
setOptionalString :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSString value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalString mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableOptionalString mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSString value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalString mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableStruct@
nullableStruct :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
nullableStruct mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableStruct mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalStruct@
optionalStruct :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
optionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "optionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
nullableOptionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableList@
nullableList :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableList mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableList:@
setNullableList :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableList mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalList@
optionalList :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
optionalList mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "optionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalList:@
setOptionalList :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalList mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableOptionalList mtrTestClusterClusterNullablesAndOptionalsStruct  =
    sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "nullableOptionalList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalList mtrTestClusterClusterNullablesAndOptionalsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNullablesAndOptionalsStruct (mkSelector "setNullableOptionalList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

