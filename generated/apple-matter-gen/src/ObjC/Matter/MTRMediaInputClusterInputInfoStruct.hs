{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaInputClusterInputInfoStruct@.
module ObjC.Matter.MTRMediaInputClusterInputInfoStruct
  ( MTRMediaInputClusterInputInfoStruct
  , IsMTRMediaInputClusterInputInfoStruct(..)
  , index
  , setIndex
  , inputType
  , setInputType
  , name
  , setName
  , descriptionString
  , setDescriptionString
  , indexSelector
  , setIndexSelector
  , inputTypeSelector
  , setInputTypeSelector
  , nameSelector
  , setNameSelector
  , descriptionStringSelector
  , setDescriptionStringSelector


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

-- | @- index@
index :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSNumber)
index mtrMediaInputClusterInputInfoStruct  =
    sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "index") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndex:@
setIndex :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSNumber value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setIndex mtrMediaInputClusterInputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "setIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inputType@
inputType :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSNumber)
inputType mtrMediaInputClusterInputInfoStruct  =
    sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "inputType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInputType:@
setInputType :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSNumber value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setInputType mtrMediaInputClusterInputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "setInputType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSString)
name mtrMediaInputClusterInputInfoStruct  =
    sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSString value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setName mtrMediaInputClusterInputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptionString@
descriptionString :: IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct => mtrMediaInputClusterInputInfoStruct -> IO (Id NSString)
descriptionString mtrMediaInputClusterInputInfoStruct  =
    sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "descriptionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRMediaInputClusterInputInfoStruct mtrMediaInputClusterInputInfoStruct, IsNSString value) => mtrMediaInputClusterInputInfoStruct -> value -> IO ()
setDescriptionString mtrMediaInputClusterInputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfoStruct (mkSelector "setDescriptionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @inputType@
inputTypeSelector :: Selector
inputTypeSelector = mkSelector "inputType"

-- | @Selector@ for @setInputType:@
setInputTypeSelector :: Selector
setInputTypeSelector = mkSelector "setInputType:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @descriptionString@
descriptionStringSelector :: Selector
descriptionStringSelector = mkSelector "descriptionString"

-- | @Selector@ for @setDescriptionString:@
setDescriptionStringSelector :: Selector
setDescriptionStringSelector = mkSelector "setDescriptionString:"

