{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Boolean State Configuration    This cluster is used to configure a boolean sensor.
--
-- Generated bindings for @MTRClusterBooleanStateConfiguration@.
module ObjC.Matter.MTRClusterBooleanStateConfiguration
  ( MTRClusterBooleanStateConfiguration
  , IsMTRClusterBooleanStateConfiguration(..)
  , suppressAlarmWithParams_expectedValues_expectedValueInterval_completion
  , enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentSensitivityLevelWithParams
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_params
  , readAttributeSupportedSensitivityLevelsWithParams
  , readAttributeDefaultSensitivityLevelWithParams
  , readAttributeAlarmsActiveWithParams
  , readAttributeAlarmsSuppressedWithParams
  , readAttributeAlarmsEnabledWithParams
  , readAttributeAlarmsSupportedWithParams
  , readAttributeSensorFaultWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeCurrentSensitivityLevelWithParamsSelector
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedSensitivityLevelsWithParamsSelector
  , readAttributeDefaultSensitivityLevelWithParamsSelector
  , readAttributeAlarmsActiveWithParamsSelector
  , readAttributeAlarmsSuppressedWithParamsSelector
  , readAttributeAlarmsEnabledWithParamsSelector
  , readAttributeAlarmsSupportedWithParamsSelector
  , readAttributeSensorFaultWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
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

-- | @- suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:@
suppressAlarmWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterSuppressAlarmParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBooleanStateConfiguration -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
suppressAlarmWithParams_expectedValues_expectedValueInterval_completion mtrClusterBooleanStateConfiguration  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBooleanStateConfiguration (mkSelector "suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:@
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterEnableDisableAlarmParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBooleanStateConfiguration -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completion mtrClusterBooleanStateConfiguration  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterBooleanStateConfiguration (mkSelector "enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentSensitivityLevelWithParams:@
readAttributeCurrentSensitivityLevelWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeCurrentSensitivityLevelWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeCurrentSensitivityLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBooleanStateConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval mtrClusterBooleanStateConfiguration  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterBooleanStateConfiguration (mkSelector "writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_params :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBooleanStateConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_params mtrClusterBooleanStateConfiguration  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterBooleanStateConfiguration (mkSelector "writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedSensitivityLevelsWithParams:@
readAttributeSupportedSensitivityLevelsWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeSupportedSensitivityLevelsWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeSupportedSensitivityLevelsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultSensitivityLevelWithParams:@
readAttributeDefaultSensitivityLevelWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeDefaultSensitivityLevelWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeDefaultSensitivityLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAlarmsActiveWithParams:@
readAttributeAlarmsActiveWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsActiveWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsActiveWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAlarmsSuppressedWithParams:@
readAttributeAlarmsSuppressedWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsSuppressedWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsSuppressedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAlarmsEnabledWithParams:@
readAttributeAlarmsEnabledWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsEnabledWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAlarmsSupportedWithParams:@
readAttributeAlarmsSupportedWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsSupportedWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsSupportedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSensorFaultWithParams:@
readAttributeSensorFaultWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeSensorFaultWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeSensorFaultWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBooleanStateConfiguration  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterBooleanStateConfiguration (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration => mtrClusterBooleanStateConfiguration -> IO (Id MTRClusterBooleanStateConfiguration)
init_ mtrClusterBooleanStateConfiguration  =
    sendMsg mtrClusterBooleanStateConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterBooleanStateConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBooleanStateConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBooleanStateConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterBooleanStateConfiguration)
initWithDevice_endpointID_queue mtrClusterBooleanStateConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterBooleanStateConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:@
suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:@
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentSensitivityLevelWithParams:@
readAttributeCurrentSensitivityLevelWithParamsSelector :: Selector
readAttributeCurrentSensitivityLevelWithParamsSelector = mkSelector "readAttributeCurrentSensitivityLevelWithParams:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedSensitivityLevelsWithParams:@
readAttributeSupportedSensitivityLevelsWithParamsSelector :: Selector
readAttributeSupportedSensitivityLevelsWithParamsSelector = mkSelector "readAttributeSupportedSensitivityLevelsWithParams:"

-- | @Selector@ for @readAttributeDefaultSensitivityLevelWithParams:@
readAttributeDefaultSensitivityLevelWithParamsSelector :: Selector
readAttributeDefaultSensitivityLevelWithParamsSelector = mkSelector "readAttributeDefaultSensitivityLevelWithParams:"

-- | @Selector@ for @readAttributeAlarmsActiveWithParams:@
readAttributeAlarmsActiveWithParamsSelector :: Selector
readAttributeAlarmsActiveWithParamsSelector = mkSelector "readAttributeAlarmsActiveWithParams:"

-- | @Selector@ for @readAttributeAlarmsSuppressedWithParams:@
readAttributeAlarmsSuppressedWithParamsSelector :: Selector
readAttributeAlarmsSuppressedWithParamsSelector = mkSelector "readAttributeAlarmsSuppressedWithParams:"

-- | @Selector@ for @readAttributeAlarmsEnabledWithParams:@
readAttributeAlarmsEnabledWithParamsSelector :: Selector
readAttributeAlarmsEnabledWithParamsSelector = mkSelector "readAttributeAlarmsEnabledWithParams:"

-- | @Selector@ for @readAttributeAlarmsSupportedWithParams:@
readAttributeAlarmsSupportedWithParamsSelector :: Selector
readAttributeAlarmsSupportedWithParamsSelector = mkSelector "readAttributeAlarmsSupportedWithParams:"

-- | @Selector@ for @readAttributeSensorFaultWithParams:@
readAttributeSensorFaultWithParamsSelector :: Selector
readAttributeSensorFaultWithParamsSelector = mkSelector "readAttributeSensorFaultWithParams:"

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

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

