{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCCleanModeClusterModeOptionStruct@.
module ObjC.Matter.MTRRVCCleanModeClusterModeOptionStruct
  ( MTRRVCCleanModeClusterModeOptionStruct
  , IsMTRRVCCleanModeClusterModeOptionStruct(..)
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
label :: IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct => mtrrvcCleanModeClusterModeOptionStruct -> IO (Id NSString)
label mtrrvcCleanModeClusterModeOptionStruct  =
    sendMsg mtrrvcCleanModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct, IsNSString value) => mtrrvcCleanModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrrvcCleanModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct => mtrrvcCleanModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrrvcCleanModeClusterModeOptionStruct  =
    sendMsg mtrrvcCleanModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct, IsNSNumber value) => mtrrvcCleanModeClusterModeOptionStruct -> value -> IO ()
setMode mtrrvcCleanModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct => mtrrvcCleanModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrrvcCleanModeClusterModeOptionStruct  =
    sendMsg mtrrvcCleanModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRRVCCleanModeClusterModeOptionStruct mtrrvcCleanModeClusterModeOptionStruct, IsNSArray value) => mtrrvcCleanModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrrvcCleanModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcCleanModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

