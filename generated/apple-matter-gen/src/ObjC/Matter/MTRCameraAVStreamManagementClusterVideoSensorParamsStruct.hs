{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoSensorParamsStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoSensorParamsStruct
  ( MTRCameraAVStreamManagementClusterVideoSensorParamsStruct
  , IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct(..)
  , sensorWidth
  , setSensorWidth
  , sensorHeight
  , setSensorHeight
  , maxFPS
  , setMaxFPS
  , maxHDRFPS
  , setMaxHDRFPS
  , sensorWidthSelector
  , setSensorWidthSelector
  , sensorHeightSelector
  , setSensorHeightSelector
  , maxFPSSelector
  , setMaxFPSSelector
  , maxHDRFPSSelector
  , setMaxHDRFPSSelector


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

-- | @- sensorWidth@
sensorWidth :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
sensorWidth mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "sensorWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSensorWidth:@
setSensorWidth :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setSensorWidth mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "setSensorWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sensorHeight@
sensorHeight :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
sensorHeight mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "sensorHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSensorHeight:@
setSensorHeight :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setSensorHeight mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "setSensorHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxFPS@
maxFPS :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
maxFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "maxFPS") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxFPS:@
setMaxFPS :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setMaxFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "setMaxFPS:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- maxHDRFPS@
maxHDRFPS :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
maxHDRFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  =
    sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "maxHDRFPS") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaxHDRFPS:@
setMaxHDRFPS :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setMaxHDRFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCameraAVStreamManagementClusterVideoSensorParamsStruct (mkSelector "setMaxHDRFPS:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensorWidth@
sensorWidthSelector :: Selector
sensorWidthSelector = mkSelector "sensorWidth"

-- | @Selector@ for @setSensorWidth:@
setSensorWidthSelector :: Selector
setSensorWidthSelector = mkSelector "setSensorWidth:"

-- | @Selector@ for @sensorHeight@
sensorHeightSelector :: Selector
sensorHeightSelector = mkSelector "sensorHeight"

-- | @Selector@ for @setSensorHeight:@
setSensorHeightSelector :: Selector
setSensorHeightSelector = mkSelector "setSensorHeight:"

-- | @Selector@ for @maxFPS@
maxFPSSelector :: Selector
maxFPSSelector = mkSelector "maxFPS"

-- | @Selector@ for @setMaxFPS:@
setMaxFPSSelector :: Selector
setMaxFPSSelector = mkSelector "setMaxFPS:"

-- | @Selector@ for @maxHDRFPS@
maxHDRFPSSelector :: Selector
maxHDRFPSSelector = mkSelector "maxHDRFPS"

-- | @Selector@ for @setMaxHDRFPS:@
setMaxHDRFPSSelector :: Selector
setMaxHDRFPSSelector = mkSelector "setMaxHDRFPS:"

