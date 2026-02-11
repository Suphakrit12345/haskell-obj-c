{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An optional information item present in the setup payload.
--
-- Note that while the Matter specification allows elements containing arbitrary TLV data types, this implementation currently only supports String and Int32 values.
--
-- Objects of this type are immutable; calling any deprecated property setters has no effect.
--
-- Generated bindings for @MTROptionalQRCodeInfo@.
module ObjC.Matter.MTROptionalQRCodeInfo
  ( MTROptionalQRCodeInfo
  , IsMTROptionalQRCodeInfo(..)
  , initWithTag_stringValue
  , initWithTag_int32Value
  , init_
  , setType
  , setTag
  , setIntegerValue
  , setStringValue
  , type_
  , tag
  , integerValue
  , stringValue
  , infoType
  , setInfoType
  , initWithTag_stringValueSelector
  , initWithTag_int32ValueSelector
  , initSelector
  , setTypeSelector
  , setTagSelector
  , setIntegerValueSelector
  , setStringValueSelector
  , typeSelector
  , tagSelector
  , integerValueSelector
  , stringValueSelector
  , infoTypeSelector
  , setInfoTypeSelector

  -- * Enum types
  , MTROptionalQRCodeInfoType(MTROptionalQRCodeInfoType)
  , pattern MTROptionalQRCodeInfoTypeUnknown
  , pattern MTROptionalQRCodeInfoTypeString
  , pattern MTROptionalQRCodeInfoTypeInt32

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
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes the object with a tag and string value. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- initWithTag:stringValue:@
initWithTag_stringValue :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber tag, IsNSString value) => mtrOptionalQRCodeInfo -> tag -> value -> IO (Id MTROptionalQRCodeInfo)
initWithTag_stringValue mtrOptionalQRCodeInfo  tag value =
  withObjCPtr tag $ \raw_tag ->
    withObjCPtr value $ \raw_value ->
        sendMsg mtrOptionalQRCodeInfo (mkSelector "initWithTag:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_tag :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes the object with a tag and int32 value. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- initWithTag:int32Value:@
initWithTag_int32Value :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber tag) => mtrOptionalQRCodeInfo -> tag -> CInt -> IO (Id MTROptionalQRCodeInfo)
initWithTag_int32Value mtrOptionalQRCodeInfo  tag value =
  withObjCPtr tag $ \raw_tag ->
      sendMsg mtrOptionalQRCodeInfo (mkSelector "initWithTag:int32Value:") (retPtr retVoid) [argPtr (castPtr raw_tag :: Ptr ()), argCInt value] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id MTROptionalQRCodeInfo)
init_ mtrOptionalQRCodeInfo  =
    sendMsg mtrOptionalQRCodeInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setType:@
setType :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> MTROptionalQRCodeInfoType -> IO ()
setType mtrOptionalQRCodeInfo  type_ =
    sendMsg mtrOptionalQRCodeInfo (mkSelector "setType:") retVoid [argCULong (coerce type_)]

-- | @- setTag:@
setTag :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber tag) => mtrOptionalQRCodeInfo -> tag -> IO ()
setTag mtrOptionalQRCodeInfo  tag =
  withObjCPtr tag $ \raw_tag ->
      sendMsg mtrOptionalQRCodeInfo (mkSelector "setTag:") retVoid [argPtr (castPtr raw_tag :: Ptr ())]

-- | @- setIntegerValue:@
setIntegerValue :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber integerValue) => mtrOptionalQRCodeInfo -> integerValue -> IO ()
setIntegerValue mtrOptionalQRCodeInfo  integerValue =
  withObjCPtr integerValue $ \raw_integerValue ->
      sendMsg mtrOptionalQRCodeInfo (mkSelector "setIntegerValue:") retVoid [argPtr (castPtr raw_integerValue :: Ptr ())]

-- | @- setStringValue:@
setStringValue :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSString stringValue) => mtrOptionalQRCodeInfo -> stringValue -> IO ()
setStringValue mtrOptionalQRCodeInfo  stringValue =
  withObjCPtr stringValue $ \raw_stringValue ->
      sendMsg mtrOptionalQRCodeInfo (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_stringValue :: Ptr ())]

-- | @- type@
type_ :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO MTROptionalQRCodeInfoType
type_ mtrOptionalQRCodeInfo  =
    fmap (coerce :: CULong -> MTROptionalQRCodeInfoType) $ sendMsg mtrOptionalQRCodeInfo (mkSelector "type") retCULong []

-- | The vendor-specific TLV tag number for this information item.
--
-- Vendor-specific elements have tags in the range 0x80 - 0xFF.
--
-- ObjC selector: @- tag@
tag :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSNumber)
tag mtrOptionalQRCodeInfo  =
    sendMsg mtrOptionalQRCodeInfo (mkSelector "tag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value held in this extension element, if @type@ is an integer type, or nil otherwise.
--
-- ObjC selector: @- integerValue@
integerValue :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSNumber)
integerValue mtrOptionalQRCodeInfo  =
    sendMsg mtrOptionalQRCodeInfo (mkSelector "integerValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value held in this extension element, if @type@ is @MTROptionalQRCodeInfoTypeString@, or nil otherwise.
--
-- ObjC selector: @- stringValue@
stringValue :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSString)
stringValue mtrOptionalQRCodeInfo  =
    sendMsg mtrOptionalQRCodeInfo (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- infoType@
infoType :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSNumber)
infoType mtrOptionalQRCodeInfo  =
    sendMsg mtrOptionalQRCodeInfo (mkSelector "infoType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInfoType:@
setInfoType :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber value) => mtrOptionalQRCodeInfo -> value -> IO ()
setInfoType mtrOptionalQRCodeInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOptionalQRCodeInfo (mkSelector "setInfoType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:stringValue:@
initWithTag_stringValueSelector :: Selector
initWithTag_stringValueSelector = mkSelector "initWithTag:stringValue:"

-- | @Selector@ for @initWithTag:int32Value:@
initWithTag_int32ValueSelector :: Selector
initWithTag_int32ValueSelector = mkSelector "initWithTag:int32Value:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @setIntegerValue:@
setIntegerValueSelector :: Selector
setIntegerValueSelector = mkSelector "setIntegerValue:"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @infoType@
infoTypeSelector :: Selector
infoTypeSelector = mkSelector "infoType"

-- | @Selector@ for @setInfoType:@
setInfoTypeSelector :: Selector
setInfoTypeSelector = mkSelector "setInfoType:"

