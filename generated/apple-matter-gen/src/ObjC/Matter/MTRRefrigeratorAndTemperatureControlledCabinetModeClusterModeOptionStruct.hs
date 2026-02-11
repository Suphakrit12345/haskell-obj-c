{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct@.
module ObjC.Matter.MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct
  ( MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct
  , IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct(..)
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
label :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> IO (Id NSString)
label mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct  =
    sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct, IsNSString value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> value -> IO ()
setLabel mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mode@
mode :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> IO (Id NSNumber)
mode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct  =
    sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct (mkSelector "mode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMode:@
setMode :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> value -> IO ()
setMode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct (mkSelector "setMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modeTags@
modeTags :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> IO (Id NSArray)
modeTags mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct  =
    sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct (mkSelector "modeTags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModeTags:@
setModeTags :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct, IsNSArray value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct -> value -> IO ()
setModeTags mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeOptionStruct (mkSelector "setModeTags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

