{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterModeOptionStruct@.
module ObjC.Matter.MTRModeSelectClusterModeOptionStruct
  ( MTRModeSelectClusterModeOptionStruct
  , IsMTRModeSelectClusterModeOptionStruct(..)
  , label
  , setLabel
  , mode
  , setMode
  , semanticTags
  , setSemanticTags
  , labelSelector
  , setLabelSelector
  , modeSelector
  , setModeSelector
  , semanticTagsSelector
  , setSemanticTagsSelector


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
label :: IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct => mtrModeSelectClusterModeOptionStruct -> IO (Id NSString)
label mtrModeSelectClusterModeOptionStruct  =
    sendMsg mtrModeSelectClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct, IsNSString value) => mtrModeSelectClusterModeOptionStruct -> value -> IO ()
setLabel mtrModeSelectClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct => mtrModeSelectClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrModeSelectClusterModeOptionStruct  =
    sendMsg mtrModeSelectClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct, IsNSNumber value) => mtrModeSelectClusterModeOptionStruct -> value -> IO ()
setMode mtrModeSelectClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- semanticTags@
semanticTags :: IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct => mtrModeSelectClusterModeOptionStruct -> IO (Id NSArray)
semanticTags mtrModeSelectClusterModeOptionStruct  =
    sendMsg mtrModeSelectClusterModeOptionStruct (mkSelector "semanticTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSemanticTags:@
setSemanticTags :: (IsMTRModeSelectClusterModeOptionStruct mtrModeSelectClusterModeOptionStruct, IsNSArray value) => mtrModeSelectClusterModeOptionStruct -> value -> IO ()
setSemanticTags mtrModeSelectClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrModeSelectClusterModeOptionStruct (mkSelector "setSemanticTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @semanticTags@
semanticTagsSelector :: Selector
semanticTagsSelector = mkSelector "semanticTags"

-- | @Selector@ for @setSemanticTags:@
setSemanticTagsSelector :: Selector
setSemanticTagsSelector = mkSelector "setSemanticTags:"

