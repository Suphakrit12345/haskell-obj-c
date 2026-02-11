{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterModeOptionStruct@.
module ObjC.Matter.MTRWaterHeaterModeClusterModeOptionStruct
  ( MTRWaterHeaterModeClusterModeOptionStruct
  , IsMTRWaterHeaterModeClusterModeOptionStruct(..)
  , label
  , setLabel
  , mode
  , setMode
  , modeTags
  , setModeTags
  , labelSelector
  , setLabelSelector
  , modeSelector
  , setModeSelector
  , modeTagsSelector
  , setModeTagsSelector


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

-- | @- label@
label :: IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct => mtrWaterHeaterModeClusterModeOptionStruct -> IO (Id NSString)
label mtrWaterHeaterModeClusterModeOptionStruct  =
    sendMsg mtrWaterHeaterModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct, IsNSString value) => mtrWaterHeaterModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrWaterHeaterModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct => mtrWaterHeaterModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrWaterHeaterModeClusterModeOptionStruct  =
    sendMsg mtrWaterHeaterModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct, IsNSNumber value) => mtrWaterHeaterModeClusterModeOptionStruct -> value -> IO ()
setMode mtrWaterHeaterModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct => mtrWaterHeaterModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrWaterHeaterModeClusterModeOptionStruct  =
    sendMsg mtrWaterHeaterModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRWaterHeaterModeClusterModeOptionStruct mtrWaterHeaterModeClusterModeOptionStruct, IsNSArray value) => mtrWaterHeaterModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrWaterHeaterModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWaterHeaterModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @modeTags@
modeTagsSelector :: Selector
modeTagsSelector = mkSelector "modeTags"

-- | @Selector@ for @setModeTags:@
setModeTagsSelector :: Selector
setModeTagsSelector = mkSelector "setModeTags:"

