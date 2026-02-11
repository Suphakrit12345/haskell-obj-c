{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaInputClusterInputInfo@.
module ObjC.Matter.MTRMediaInputClusterInputInfo
  ( MTRMediaInputClusterInputInfo
  , IsMTRMediaInputClusterInputInfo(..)
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
index :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSNumber)
index mtrMediaInputClusterInputInfo  =
    sendMsg mtrMediaInputClusterInputInfo (mkSelector "index") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndex:@
setIndex :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSNumber value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setIndex mtrMediaInputClusterInputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfo (mkSelector "setIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inputType@
inputType :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSNumber)
inputType mtrMediaInputClusterInputInfo  =
    sendMsg mtrMediaInputClusterInputInfo (mkSelector "inputType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInputType:@
setInputType :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSNumber value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setInputType mtrMediaInputClusterInputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfo (mkSelector "setInputType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSString)
name mtrMediaInputClusterInputInfo  =
    sendMsg mtrMediaInputClusterInputInfo (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSString value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setName mtrMediaInputClusterInputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfo (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- descriptionString@
descriptionString :: IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo => mtrMediaInputClusterInputInfo -> IO (Id NSString)
descriptionString mtrMediaInputClusterInputInfo  =
    sendMsg mtrMediaInputClusterInputInfo (mkSelector "descriptionString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDescriptionString:@
setDescriptionString :: (IsMTRMediaInputClusterInputInfo mtrMediaInputClusterInputInfo, IsNSString value) => mtrMediaInputClusterInputInfo -> value -> IO ()
setDescriptionString mtrMediaInputClusterInputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaInputClusterInputInfo (mkSelector "setDescriptionString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

