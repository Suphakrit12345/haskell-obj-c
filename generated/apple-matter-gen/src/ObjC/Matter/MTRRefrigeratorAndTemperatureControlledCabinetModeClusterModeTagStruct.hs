{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct@.
module ObjC.Matter.MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct
  ( MTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct
  , IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct(..)
  , mfgCode
  , setMfgCode
  , value
  , setValue
  , mfgCodeSelector
  , setMfgCodeSelector
  , valueSelector
  , setValueSelector


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

-- | @- mfgCode@
mfgCode :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct  =
    sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct  =
    sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct, IsNSNumber value) => mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct -> value -> IO ()
setValue mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrRefrigeratorAndTemperatureControlledCabinetModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

