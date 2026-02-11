{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestFabricScoped@.
module ObjC.Matter.MTRTestClusterClusterTestFabricScoped
  ( MTRTestClusterClusterTestFabricScoped
  , IsMTRTestClusterClusterTestFabricScoped(..)
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
fabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
fabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "fabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveInt8u:@
setFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optionalFabricSensitiveInt8u@
optionalFabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
optionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "optionalFabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionalFabricSensitiveInt8u:@
setOptionalFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setOptionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setOptionalFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableFabricSensitiveInt8u@
nullableFabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
nullableFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "nullableFabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableFabricSensitiveInt8u:@
setNullableFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setNullableFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setNullableFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nullableOptionalFabricSensitiveInt8u@
nullableOptionalFabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
nullableOptionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "nullableOptionalFabricSensitiveInt8u") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNullableOptionalFabricSensitiveInt8u:@
setNullableOptionalFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setNullableOptionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setNullableOptionalFabricSensitiveInt8u:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricSensitiveCharString@
fabricSensitiveCharString :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSString)
fabricSensitiveCharString mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "fabricSensitiveCharString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveCharString:@
setFabricSensitiveCharString :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSString value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveCharString mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setFabricSensitiveCharString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricSensitiveStruct@
fabricSensitiveStruct :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id MTRTestClusterClusterSimpleStruct)
fabricSensitiveStruct mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "fabricSensitiveStruct") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveStruct:@
setFabricSensitiveStruct :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveStruct mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setFabricSensitiveStruct:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricSensitiveInt8uList@
fabricSensitiveInt8uList :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSArray)
fabricSensitiveInt8uList mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "fabricSensitiveInt8uList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricSensitiveInt8uList:@
setFabricSensitiveInt8uList :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSArray value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8uList mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setFabricSensitiveInt8uList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
fabricIndex mtrTestClusterClusterTestFabricScoped  =
    sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricIndex mtrTestClusterClusterTestFabricScoped  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestFabricScoped (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

