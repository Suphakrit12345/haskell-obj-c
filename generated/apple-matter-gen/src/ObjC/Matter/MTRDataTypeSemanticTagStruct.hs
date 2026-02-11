{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeSemanticTagStruct@.
module ObjC.Matter.MTRDataTypeSemanticTagStruct
  ( MTRDataTypeSemanticTagStruct
  , IsMTRDataTypeSemanticTagStruct(..)
  , mfgCode
  , setMfgCode
  , namespaceID
  , setNamespaceID
  , tag
  , setTag
  , label
  , setLabel
  , mfgCodeSelector
  , setMfgCodeSelector
  , namespaceIDSelector
  , setNamespaceIDSelector
  , tagSelector
  , setTagSelector
  , labelSelector
  , setLabelSelector


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

-- | @- mfgCode@
mfgCode :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSNumber)
mfgCode mtrDataTypeSemanticTagStruct  =
    sendMsg mtrDataTypeSemanticTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSNumber value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setMfgCode mtrDataTypeSemanticTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeSemanticTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- namespaceID@
namespaceID :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSNumber)
namespaceID mtrDataTypeSemanticTagStruct  =
    sendMsg mtrDataTypeSemanticTagStruct (mkSelector "namespaceID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNamespaceID:@
setNamespaceID :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSNumber value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setNamespaceID mtrDataTypeSemanticTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeSemanticTagStruct (mkSelector "setNamespaceID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tag@
tag :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSNumber)
tag mtrDataTypeSemanticTagStruct  =
    sendMsg mtrDataTypeSemanticTagStruct (mkSelector "tag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTag:@
setTag :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSNumber value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setTag mtrDataTypeSemanticTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeSemanticTagStruct (mkSelector "setTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct => mtrDataTypeSemanticTagStruct -> IO (Id NSString)
label mtrDataTypeSemanticTagStruct  =
    sendMsg mtrDataTypeSemanticTagStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRDataTypeSemanticTagStruct mtrDataTypeSemanticTagStruct, IsNSString value) => mtrDataTypeSemanticTagStruct -> value -> IO ()
setLabel mtrDataTypeSemanticTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeSemanticTagStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @namespaceID@
namespaceIDSelector :: Selector
namespaceIDSelector = mkSelector "namespaceID"

-- | @Selector@ for @setNamespaceID:@
setNamespaceIDSelector :: Selector
setNamespaceIDSelector = mkSelector "setNamespaceID:"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

