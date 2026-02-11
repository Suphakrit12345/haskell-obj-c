{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeAtomicAttributeStatusStruct@.
module ObjC.Matter.MTRDataTypeAtomicAttributeStatusStruct
  ( MTRDataTypeAtomicAttributeStatusStruct
  , IsMTRDataTypeAtomicAttributeStatusStruct(..)
  , attributeID
  , setAttributeID
  , statusCode
  , setStatusCode
  , attributeIDSelector
  , setAttributeIDSelector
  , statusCodeSelector
  , setStatusCodeSelector


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
attributeID :: IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct => mtrDataTypeAtomicAttributeStatusStruct -> IO (Id NSNumber)
attributeID mtrDataTypeAtomicAttributeStatusStruct  =
    sendMsg mtrDataTypeAtomicAttributeStatusStruct (mkSelector "attributeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeID:@
setAttributeID :: (IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct, IsNSNumber value) => mtrDataTypeAtomicAttributeStatusStruct -> value -> IO ()
setAttributeID mtrDataTypeAtomicAttributeStatusStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeAtomicAttributeStatusStruct (mkSelector "setAttributeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- statusCode@
statusCode :: IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct => mtrDataTypeAtomicAttributeStatusStruct -> IO (Id NSNumber)
statusCode mtrDataTypeAtomicAttributeStatusStruct  =
    sendMsg mtrDataTypeAtomicAttributeStatusStruct (mkSelector "statusCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatusCode:@
setStatusCode :: (IsMTRDataTypeAtomicAttributeStatusStruct mtrDataTypeAtomicAttributeStatusStruct, IsNSNumber value) => mtrDataTypeAtomicAttributeStatusStruct -> value -> IO ()
setStatusCode mtrDataTypeAtomicAttributeStatusStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeAtomicAttributeStatusStruct (mkSelector "setStatusCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeID@
attributeIDSelector :: Selector
attributeIDSelector = mkSelector "attributeID"

-- | @Selector@ for @setAttributeID:@
setAttributeIDSelector :: Selector
setAttributeIDSelector = mkSelector "setAttributeID:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector
setStatusCodeSelector = mkSelector "setStatusCode:"

