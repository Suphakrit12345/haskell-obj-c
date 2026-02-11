{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMicrowaveOvenModeClusterModeOptionStruct@.
module ObjC.Matter.MTRMicrowaveOvenModeClusterModeOptionStruct
  ( MTRMicrowaveOvenModeClusterModeOptionStruct
  , IsMTRMicrowaveOvenModeClusterModeOptionStruct(..)
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
label :: IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct => mtrMicrowaveOvenModeClusterModeOptionStruct -> IO (Id NSString)
label mtrMicrowaveOvenModeClusterModeOptionStruct  =
    sendMsg mtrMicrowaveOvenModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct, IsNSString value) => mtrMicrowaveOvenModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrMicrowaveOvenModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct => mtrMicrowaveOvenModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrMicrowaveOvenModeClusterModeOptionStruct  =
    sendMsg mtrMicrowaveOvenModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct, IsNSNumber value) => mtrMicrowaveOvenModeClusterModeOptionStruct -> value -> IO ()
setMode mtrMicrowaveOvenModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct => mtrMicrowaveOvenModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrMicrowaveOvenModeClusterModeOptionStruct  =
    sendMsg mtrMicrowaveOvenModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRMicrowaveOvenModeClusterModeOptionStruct mtrMicrowaveOvenModeClusterModeOptionStruct, IsNSArray value) => mtrMicrowaveOvenModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrMicrowaveOvenModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMicrowaveOvenModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

