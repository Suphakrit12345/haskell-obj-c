{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterAttributeValuePairStruct@.
module ObjC.Matter.MTRScenesManagementClusterAttributeValuePairStruct
  ( MTRScenesManagementClusterAttributeValuePairStruct
  , IsMTRScenesManagementClusterAttributeValuePairStruct(..)
  , attributeID
  , setAttributeID
  , valueUnsigned8
  , setValueUnsigned8
  , valueSigned8
  , setValueSigned8
  , valueUnsigned16
  , setValueUnsigned16
  , valueSigned16
  , setValueSigned16
  , valueUnsigned32
  , setValueUnsigned32
  , valueSigned32
  , setValueSigned32
  , valueUnsigned64
  , setValueUnsigned64
  , valueSigned64
  , setValueSigned64
  , attributeIDSelector
  , setAttributeIDSelector
  , valueUnsigned8Selector
  , setValueUnsigned8Selector
  , valueSigned8Selector
  , setValueSigned8Selector
  , valueUnsigned16Selector
  , setValueUnsigned16Selector
  , valueSigned16Selector
  , setValueSigned16Selector
  , valueUnsigned32Selector
  , setValueUnsigned32Selector
  , valueSigned32Selector
  , setValueSigned32Selector
  , valueUnsigned64Selector
  , setValueUnsigned64Selector
  , valueSigned64Selector
  , setValueSigned64Selector


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

-- | @- attributeID@
attributeID :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
attributeID mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "attributeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeID:@
setAttributeID :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setAttributeID mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setAttributeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueUnsigned8@
valueUnsigned8 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned8 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueUnsigned8") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueUnsigned8:@
setValueUnsigned8 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned8 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueUnsigned8:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueSigned8@
valueSigned8 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned8 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueSigned8") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueSigned8:@
setValueSigned8 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned8 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueSigned8:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueUnsigned16@
valueUnsigned16 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned16 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueUnsigned16") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueUnsigned16:@
setValueUnsigned16 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned16 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueUnsigned16:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueSigned16@
valueSigned16 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned16 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueSigned16") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueSigned16:@
setValueSigned16 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned16 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueSigned16:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueUnsigned32@
valueUnsigned32 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned32 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueUnsigned32") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueUnsigned32:@
setValueUnsigned32 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned32 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueUnsigned32:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueSigned32@
valueSigned32 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned32 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueSigned32") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueSigned32:@
setValueSigned32 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned32 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueSigned32:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueUnsigned64@
valueUnsigned64 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueUnsigned64 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueUnsigned64") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueUnsigned64:@
setValueUnsigned64 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueUnsigned64 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueUnsigned64:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueSigned64@
valueSigned64 :: IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct => mtrScenesManagementClusterAttributeValuePairStruct -> IO (Id NSNumber)
valueSigned64 mtrScenesManagementClusterAttributeValuePairStruct  =
    sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "valueSigned64") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueSigned64:@
setValueSigned64 :: (IsMTRScenesManagementClusterAttributeValuePairStruct mtrScenesManagementClusterAttributeValuePairStruct, IsNSNumber value) => mtrScenesManagementClusterAttributeValuePairStruct -> value -> IO ()
setValueSigned64 mtrScenesManagementClusterAttributeValuePairStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterAttributeValuePairStruct (mkSelector "setValueSigned64:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeID@
attributeIDSelector :: Selector
attributeIDSelector = mkSelector "attributeID"

-- | @Selector@ for @setAttributeID:@
setAttributeIDSelector :: Selector
setAttributeIDSelector = mkSelector "setAttributeID:"

-- | @Selector@ for @valueUnsigned8@
valueUnsigned8Selector :: Selector
valueUnsigned8Selector = mkSelector "valueUnsigned8"

-- | @Selector@ for @setValueUnsigned8:@
setValueUnsigned8Selector :: Selector
setValueUnsigned8Selector = mkSelector "setValueUnsigned8:"

-- | @Selector@ for @valueSigned8@
valueSigned8Selector :: Selector
valueSigned8Selector = mkSelector "valueSigned8"

-- | @Selector@ for @setValueSigned8:@
setValueSigned8Selector :: Selector
setValueSigned8Selector = mkSelector "setValueSigned8:"

-- | @Selector@ for @valueUnsigned16@
valueUnsigned16Selector :: Selector
valueUnsigned16Selector = mkSelector "valueUnsigned16"

-- | @Selector@ for @setValueUnsigned16:@
setValueUnsigned16Selector :: Selector
setValueUnsigned16Selector = mkSelector "setValueUnsigned16:"

-- | @Selector@ for @valueSigned16@
valueSigned16Selector :: Selector
valueSigned16Selector = mkSelector "valueSigned16"

-- | @Selector@ for @setValueSigned16:@
setValueSigned16Selector :: Selector
setValueSigned16Selector = mkSelector "setValueSigned16:"

-- | @Selector@ for @valueUnsigned32@
valueUnsigned32Selector :: Selector
valueUnsigned32Selector = mkSelector "valueUnsigned32"

-- | @Selector@ for @setValueUnsigned32:@
setValueUnsigned32Selector :: Selector
setValueUnsigned32Selector = mkSelector "setValueUnsigned32:"

-- | @Selector@ for @valueSigned32@
valueSigned32Selector :: Selector
valueSigned32Selector = mkSelector "valueSigned32"

-- | @Selector@ for @setValueSigned32:@
setValueSigned32Selector :: Selector
setValueSigned32Selector = mkSelector "setValueSigned32:"

-- | @Selector@ for @valueUnsigned64@
valueUnsigned64Selector :: Selector
valueUnsigned64Selector = mkSelector "valueUnsigned64"

-- | @Selector@ for @setValueUnsigned64:@
setValueUnsigned64Selector :: Selector
setValueUnsigned64Selector = mkSelector "setValueUnsigned64:"

-- | @Selector@ for @valueSigned64@
valueSigned64Selector :: Selector
valueSigned64Selector = mkSelector "valueSigned64"

-- | @Selector@ for @setValueSigned64:@
setValueSigned64Selector :: Selector
setValueSigned64Selector = mkSelector "setValueSigned64:"

