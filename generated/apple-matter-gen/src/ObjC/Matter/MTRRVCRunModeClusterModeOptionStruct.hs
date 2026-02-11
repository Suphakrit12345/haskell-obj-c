{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCRunModeClusterModeOptionStruct@.
module ObjC.Matter.MTRRVCRunModeClusterModeOptionStruct
  ( MTRRVCRunModeClusterModeOptionStruct
  , IsMTRRVCRunModeClusterModeOptionStruct(..)
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
label :: IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct => mtrrvcRunModeClusterModeOptionStruct -> IO (Id NSString)
label mtrrvcRunModeClusterModeOptionStruct  =
    sendMsg mtrrvcRunModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct, IsNSString value) => mtrrvcRunModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrrvcRunModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct => mtrrvcRunModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrrvcRunModeClusterModeOptionStruct  =
    sendMsg mtrrvcRunModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct, IsNSNumber value) => mtrrvcRunModeClusterModeOptionStruct -> value -> IO ()
setMode mtrrvcRunModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct => mtrrvcRunModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrrvcRunModeClusterModeOptionStruct  =
    sendMsg mtrrvcRunModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRRVCRunModeClusterModeOptionStruct mtrrvcRunModeClusterModeOptionStruct, IsNSArray value) => mtrrvcRunModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrrvcRunModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcRunModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

