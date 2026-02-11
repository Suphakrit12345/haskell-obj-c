{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLaundryWasherModeClusterModeOptionStruct@.
module ObjC.Matter.MTRLaundryWasherModeClusterModeOptionStruct
  ( MTRLaundryWasherModeClusterModeOptionStruct
  , IsMTRLaundryWasherModeClusterModeOptionStruct(..)
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
label :: IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct => mtrLaundryWasherModeClusterModeOptionStruct -> IO (Id NSString)
label mtrLaundryWasherModeClusterModeOptionStruct  =
    sendMsg mtrLaundryWasherModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct, IsNSString value) => mtrLaundryWasherModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrLaundryWasherModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLaundryWasherModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct => mtrLaundryWasherModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrLaundryWasherModeClusterModeOptionStruct  =
    sendMsg mtrLaundryWasherModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct, IsNSNumber value) => mtrLaundryWasherModeClusterModeOptionStruct -> value -> IO ()
setMode mtrLaundryWasherModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLaundryWasherModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct => mtrLaundryWasherModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrLaundryWasherModeClusterModeOptionStruct  =
    sendMsg mtrLaundryWasherModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRLaundryWasherModeClusterModeOptionStruct mtrLaundryWasherModeClusterModeOptionStruct, IsNSArray value) => mtrLaundryWasherModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrLaundryWasherModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrLaundryWasherModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

