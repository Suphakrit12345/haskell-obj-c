{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thermostat User Interface Configuration    An interface for configuring the user interface of a thermostat (which may be remote from the thermostat).
--
-- Generated bindings for @MTRClusterThermostatUserInterfaceConfiguration@.
module ObjC.Matter.MTRClusterThermostatUserInterfaceConfiguration
  ( MTRClusterThermostatUserInterfaceConfiguration
  , IsMTRClusterThermostatUserInterfaceConfiguration(..)
  , readAttributeTemperatureDisplayModeWithParams
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_params
  , readAttributeKeypadLockoutWithParams
  , writeAttributeKeypadLockoutWithValue_expectedValueInterval
  , writeAttributeKeypadLockoutWithValue_expectedValueInterval_params
  , readAttributeScheduleProgrammingVisibilityWithParams
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , readAttributeTemperatureDisplayModeWithParamsSelector
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector
  , readAttributeKeypadLockoutWithParamsSelector
  , writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector
  , writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector
  , readAttributeScheduleProgrammingVisibilityWithParamsSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , initWithDevice_endpointID_queueSelector


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

-- | @- readAttributeTemperatureDisplayModeWithParams:@
readAttributeTemperatureDisplayModeWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeTemperatureDisplayModeWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeTemperatureDisplayModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval mtrClusterThermostatUserInterfaceConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_params mtrClusterThermostatUserInterfaceConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeKeypadLockoutWithParams:@
readAttributeKeypadLockoutWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeKeypadLockoutWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeKeypadLockoutWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeKeypadLockoutWithValue:expectedValueInterval:@
writeAttributeKeypadLockoutWithValue_expectedValueInterval :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeKeypadLockoutWithValue_expectedValueInterval mtrClusterThermostatUserInterfaceConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeKeypadLockoutWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:@
writeAttributeKeypadLockoutWithValue_expectedValueInterval_params :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeKeypadLockoutWithValue_expectedValueInterval_params mtrClusterThermostatUserInterfaceConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeScheduleProgrammingVisibilityWithParams:@
readAttributeScheduleProgrammingVisibilityWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeScheduleProgrammingVisibilityWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeScheduleProgrammingVisibilityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval mtrClusterThermostatUserInterfaceConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_params :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_params mtrClusterThermostatUserInterfaceConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThermostatUserInterfaceConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration => mtrClusterThermostatUserInterfaceConfiguration -> IO (Id MTRClusterThermostatUserInterfaceConfiguration)
init_ mtrClusterThermostatUserInterfaceConfiguration  =
    sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterThermostatUserInterfaceConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThermostatUserInterfaceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterThermostatUserInterfaceConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpoint_queue mtrClusterThermostatUserInterfaceConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThermostatUserInterfaceConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpointID_queue mtrClusterThermostatUserInterfaceConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterThermostatUserInterfaceConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeTemperatureDisplayModeWithParams:@
readAttributeTemperatureDisplayModeWithParamsSelector :: Selector
readAttributeTemperatureDisplayModeWithParamsSelector = mkSelector "readAttributeTemperatureDisplayModeWithParams:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeKeypadLockoutWithParams:@
readAttributeKeypadLockoutWithParamsSelector :: Selector
readAttributeKeypadLockoutWithParamsSelector = mkSelector "readAttributeKeypadLockoutWithParams:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:expectedValueInterval:@
writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector :: Selector
writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeKeypadLockoutWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:@
writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeScheduleProgrammingVisibilityWithParams:@
readAttributeScheduleProgrammingVisibilityWithParamsSelector :: Selector
readAttributeScheduleProgrammingVisibilityWithParamsSelector = mkSelector "readAttributeScheduleProgrammingVisibilityWithParams:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector :: Selector
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

