{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestFabricScoped@.
module ObjC.Matter.MTRUnitTestingClusterTestFabricScoped
  ( MTRUnitTestingClusterTestFabricScoped
  , IsMTRUnitTestingClusterTestFabricScoped(..)
  , fabricSensitiveInt8u
  , setFabricSensitiveInt8u
  , optionalFabricSensitiveInt8u
  , setOptionalFabricSensitiveInt8u
  , nullableFabricSensitiveInt8u
  , setNullableFabricSensitiveInt8u
  , nullableOptionalFabricSensitiveInt8u
  , setNullableOptionalFabricSensitiveInt8u
  , fabricSensitiveCharString
  , setFabricSensitiveCharString
  , fabricSensitiveStruct
  , setFabricSensitiveStruct
  , fabricSensitiveInt8uList
  , setFabricSensitiveInt8uList
  , fabricIndex
  , setFabricIndex
  , fabricSensitiveInt8uSelector
  , setFabricSensitiveInt8uSelector
  , optionalFabricSensitiveInt8uSelector
  , setOptionalFabricSensitiveInt8uSelector
  , nullableFabricSensitiveInt8uSelector
  , setNullableFabricSensitiveInt8uSelector
  , nullableOptionalFabricSensitiveInt8uSelector
  , setNullableOptionalFabricSensitiveInt8uSelector
  , fabricSensitiveCharStringSelector
  , setFabricSensitiveCharStringSelector
  , fabricSensitiveStructSelector
  , setFabricSensitiveStructSelector
  , fabricSensitiveInt8uListSelector
  , setFabricSensitiveInt8uListSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- fabricSensitiveInt8u@
fabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
fabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "fabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveInt8u:@
setFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalFabricSensitiveInt8u@
optionalFabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
optionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "optionalFabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalFabricSensitiveInt8u:@
setOptionalFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setOptionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setOptionalFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableFabricSensitiveInt8u@
nullableFabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
nullableFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "nullableFabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableFabricSensitiveInt8u:@
setNullableFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setNullableFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setNullableFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalFabricSensitiveInt8u@
nullableOptionalFabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
nullableOptionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "nullableOptionalFabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalFabricSensitiveInt8u:@
setNullableOptionalFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setNullableOptionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setNullableOptionalFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricSensitiveCharString@
fabricSensitiveCharString :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSString)
fabricSensitiveCharString mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "fabricSensitiveCharString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveCharString:@
setFabricSensitiveCharString :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSString value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveCharString mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setFabricSensitiveCharString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricSensitiveStruct@
fabricSensitiveStruct :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id MTRUnitTestingClusterSimpleStruct)
fabricSensitiveStruct mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "fabricSensitiveStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveStruct:@
setFabricSensitiveStruct :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveStruct mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setFabricSensitiveStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricSensitiveInt8uList@
fabricSensitiveInt8uList :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSArray)
fabricSensitiveInt8uList mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "fabricSensitiveInt8uList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveInt8uList:@
setFabricSensitiveInt8uList :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSArray value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8uList mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setFabricSensitiveInt8uList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
fabricIndex mtrUnitTestingClusterTestFabricScoped  =
    sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricIndex mtrUnitTestingClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScoped (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricSensitiveInt8u@
fabricSensitiveInt8uSelector :: Selector
fabricSensitiveInt8uSelector = mkSelector "fabricSensitiveInt8u"

-- | @Selector@ for @setFabricSensitiveInt8u:@
setFabricSensitiveInt8uSelector :: Selector
setFabricSensitiveInt8uSelector = mkSelector "setFabricSensitiveInt8u:"

-- | @Selector@ for @optionalFabricSensitiveInt8u@
optionalFabricSensitiveInt8uSelector :: Selector
optionalFabricSensitiveInt8uSelector = mkSelector "optionalFabricSensitiveInt8u"

-- | @Selector@ for @setOptionalFabricSensitiveInt8u:@
setOptionalFabricSensitiveInt8uSelector :: Selector
setOptionalFabricSensitiveInt8uSelector = mkSelector "setOptionalFabricSensitiveInt8u:"

-- | @Selector@ for @nullableFabricSensitiveInt8u@
nullableFabricSensitiveInt8uSelector :: Selector
nullableFabricSensitiveInt8uSelector = mkSelector "nullableFabricSensitiveInt8u"

-- | @Selector@ for @setNullableFabricSensitiveInt8u:@
setNullableFabricSensitiveInt8uSelector :: Selector
setNullableFabricSensitiveInt8uSelector = mkSelector "setNullableFabricSensitiveInt8u:"

-- | @Selector@ for @nullableOptionalFabricSensitiveInt8u@
nullableOptionalFabricSensitiveInt8uSelector :: Selector
nullableOptionalFabricSensitiveInt8uSelector = mkSelector "nullableOptionalFabricSensitiveInt8u"

-- | @Selector@ for @setNullableOptionalFabricSensitiveInt8u:@
setNullableOptionalFabricSensitiveInt8uSelector :: Selector
setNullableOptionalFabricSensitiveInt8uSelector = mkSelector "setNullableOptionalFabricSensitiveInt8u:"

-- | @Selector@ for @fabricSensitiveCharString@
fabricSensitiveCharStringSelector :: Selector
fabricSensitiveCharStringSelector = mkSelector "fabricSensitiveCharString"

-- | @Selector@ for @setFabricSensitiveCharString:@
setFabricSensitiveCharStringSelector :: Selector
setFabricSensitiveCharStringSelector = mkSelector "setFabricSensitiveCharString:"

-- | @Selector@ for @fabricSensitiveStruct@
fabricSensitiveStructSelector :: Selector
fabricSensitiveStructSelector = mkSelector "fabricSensitiveStruct"

-- | @Selector@ for @setFabricSensitiveStruct:@
setFabricSensitiveStructSelector :: Selector
setFabricSensitiveStructSelector = mkSelector "setFabricSensitiveStruct:"

-- | @Selector@ for @fabricSensitiveInt8uList@
fabricSensitiveInt8uListSelector :: Selector
fabricSensitiveInt8uListSelector = mkSelector "fabricSensitiveInt8uList"

-- | @Selector@ for @setFabricSensitiveInt8uList:@
setFabricSensitiveInt8uListSelector :: Selector
setFabricSensitiveInt8uListSelector = mkSelector "setFabricSensitiveInt8uList:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

