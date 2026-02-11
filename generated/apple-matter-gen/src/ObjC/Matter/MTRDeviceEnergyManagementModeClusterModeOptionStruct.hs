{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterModeOptionStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterModeOptionStruct
  ( MTRDeviceEnergyManagementModeClusterModeOptionStruct
  , IsMTRDeviceEnergyManagementModeClusterModeOptionStruct(..)
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
label :: IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> IO (Id NSString)
label mtrDeviceEnergyManagementModeClusterModeOptionStruct  =
    sendMsg mtrDeviceEnergyManagementModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct, IsNSString value) => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrDeviceEnergyManagementModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrDeviceEnergyManagementModeClusterModeOptionStruct  =
    sendMsg mtrDeviceEnergyManagementModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> value -> IO ()
setMode mtrDeviceEnergyManagementModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrDeviceEnergyManagementModeClusterModeOptionStruct  =
    sendMsg mtrDeviceEnergyManagementModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRDeviceEnergyManagementModeClusterModeOptionStruct mtrDeviceEnergyManagementModeClusterModeOptionStruct, IsNSArray value) => mtrDeviceEnergyManagementModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrDeviceEnergyManagementModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

