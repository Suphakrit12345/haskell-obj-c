{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenModeClusterModeOptionStruct@.
module ObjC.Matter.MTROvenModeClusterModeOptionStruct
  ( MTROvenModeClusterModeOptionStruct
  , IsMTROvenModeClusterModeOptionStruct(..)
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
label :: IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct => mtrOvenModeClusterModeOptionStruct -> IO (Id NSString)
label mtrOvenModeClusterModeOptionStruct  =
    sendMsg mtrOvenModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct, IsNSString value) => mtrOvenModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrOvenModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct => mtrOvenModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrOvenModeClusterModeOptionStruct  =
    sendMsg mtrOvenModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct, IsNSNumber value) => mtrOvenModeClusterModeOptionStruct -> value -> IO ()
setMode mtrOvenModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct => mtrOvenModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrOvenModeClusterModeOptionStruct  =
    sendMsg mtrOvenModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTROvenModeClusterModeOptionStruct mtrOvenModeClusterModeOptionStruct, IsNSArray value) => mtrOvenModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrOvenModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

