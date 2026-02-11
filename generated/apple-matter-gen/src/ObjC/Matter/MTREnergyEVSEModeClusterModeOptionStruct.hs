{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEModeClusterModeOptionStruct@.
module ObjC.Matter.MTREnergyEVSEModeClusterModeOptionStruct
  ( MTREnergyEVSEModeClusterModeOptionStruct
  , IsMTREnergyEVSEModeClusterModeOptionStruct(..)
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
label :: IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct => mtrEnergyEVSEModeClusterModeOptionStruct -> IO (Id NSString)
label mtrEnergyEVSEModeClusterModeOptionStruct  =
    sendMsg mtrEnergyEVSEModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct, IsNSString value) => mtrEnergyEVSEModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrEnergyEVSEModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct => mtrEnergyEVSEModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrEnergyEVSEModeClusterModeOptionStruct  =
    sendMsg mtrEnergyEVSEModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct, IsNSNumber value) => mtrEnergyEVSEModeClusterModeOptionStruct -> value -> IO ()
setMode mtrEnergyEVSEModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct => mtrEnergyEVSEModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrEnergyEVSEModeClusterModeOptionStruct  =
    sendMsg mtrEnergyEVSEModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTREnergyEVSEModeClusterModeOptionStruct mtrEnergyEVSEModeClusterModeOptionStruct, IsNSArray value) => mtrEnergyEVSEModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrEnergyEVSEModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

