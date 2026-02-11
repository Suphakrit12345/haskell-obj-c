{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterModeTagStruct@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterModeTagStruct
  ( MTRDeviceEnergyManagementModeClusterModeTagStruct
  , IsMTRDeviceEnergyManagementModeClusterModeTagStruct(..)
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
mfgCode :: IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct => mtrDeviceEnergyManagementModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrDeviceEnergyManagementModeClusterModeTagStruct  =
    sendMsg mtrDeviceEnergyManagementModeClusterModeTagStruct (mkSelector "mfgCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMfgCode:@
setMfgCode :: (IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrDeviceEnergyManagementModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterModeTagStruct (mkSelector "setMfgCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct => mtrDeviceEnergyManagementModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrDeviceEnergyManagementModeClusterModeTagStruct  =
    sendMsg mtrDeviceEnergyManagementModeClusterModeTagStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRDeviceEnergyManagementModeClusterModeTagStruct mtrDeviceEnergyManagementModeClusterModeTagStruct, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterModeTagStruct -> value -> IO ()
setValue mtrDeviceEnergyManagementModeClusterModeTagStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterModeTagStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

