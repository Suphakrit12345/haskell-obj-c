{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeTestGlobalStruct@.
module ObjC.Matter.MTRDataTypeTestGlobalStruct
  ( MTRDataTypeTestGlobalStruct
  , IsMTRDataTypeTestGlobalStruct(..)
  , name
  , setName
  , myBitmap
  , setMyBitmap
  , myEnum
  , setMyEnum
  , nameSelector
  , setNameSelector
  , myBitmapSelector
  , setMyBitmapSelector
  , myEnumSelector
  , setMyEnumSelector


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

-- | @- name@
name :: IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct => mtrDataTypeTestGlobalStruct -> IO (Id NSString)
name mtrDataTypeTestGlobalStruct  =
    sendMsg mtrDataTypeTestGlobalStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct, IsNSString value) => mtrDataTypeTestGlobalStruct -> value -> IO ()
setName mtrDataTypeTestGlobalStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeTestGlobalStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- myBitmap@
myBitmap :: IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct => mtrDataTypeTestGlobalStruct -> IO (Id NSNumber)
myBitmap mtrDataTypeTestGlobalStruct  =
    sendMsg mtrDataTypeTestGlobalStruct (mkSelector "myBitmap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMyBitmap:@
setMyBitmap :: (IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct, IsNSNumber value) => mtrDataTypeTestGlobalStruct -> value -> IO ()
setMyBitmap mtrDataTypeTestGlobalStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeTestGlobalStruct (mkSelector "setMyBitmap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- myEnum@
myEnum :: IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct => mtrDataTypeTestGlobalStruct -> IO (Id NSNumber)
myEnum mtrDataTypeTestGlobalStruct  =
    sendMsg mtrDataTypeTestGlobalStruct (mkSelector "myEnum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMyEnum:@
setMyEnum :: (IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct, IsNSNumber value) => mtrDataTypeTestGlobalStruct -> value -> IO ()
setMyEnum mtrDataTypeTestGlobalStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeTestGlobalStruct (mkSelector "setMyEnum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @myBitmap@
myBitmapSelector :: Selector
myBitmapSelector = mkSelector "myBitmap"

-- | @Selector@ for @setMyBitmap:@
setMyBitmapSelector :: Selector
setMyBitmapSelector = mkSelector "setMyBitmap:"

-- | @Selector@ for @myEnum@
myEnumSelector :: Selector
myEnumSelector = mkSelector "myEnum"

-- | @Selector@ for @setMyEnum:@
setMyEnumSelector :: Selector
setMyEnumSelector = mkSelector "setMyEnum:"

