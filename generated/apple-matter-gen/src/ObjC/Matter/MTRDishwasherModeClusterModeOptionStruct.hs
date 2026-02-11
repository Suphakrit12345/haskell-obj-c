{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherModeClusterModeOptionStruct@.
module ObjC.Matter.MTRDishwasherModeClusterModeOptionStruct
  ( MTRDishwasherModeClusterModeOptionStruct
  , IsMTRDishwasherModeClusterModeOptionStruct(..)
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
label :: IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct => mtrDishwasherModeClusterModeOptionStruct -> IO (Id NSString)
label mtrDishwasherModeClusterModeOptionStruct  =
    sendMsg mtrDishwasherModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct, IsNSString value) => mtrDishwasherModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrDishwasherModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct => mtrDishwasherModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrDishwasherModeClusterModeOptionStruct  =
    sendMsg mtrDishwasherModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct, IsNSNumber value) => mtrDishwasherModeClusterModeOptionStruct -> value -> IO ()
setMode mtrDishwasherModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct => mtrDishwasherModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrDishwasherModeClusterModeOptionStruct  =
    sendMsg mtrDishwasherModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRDishwasherModeClusterModeOptionStruct mtrDishwasherModeClusterModeOptionStruct, IsNSArray value) => mtrDishwasherModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrDishwasherModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDishwasherModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

