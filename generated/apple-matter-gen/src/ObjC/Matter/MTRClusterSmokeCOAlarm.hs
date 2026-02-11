{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Smoke CO Alarm    This cluster provides an interface for observing and managing the state of smoke and CO alarms.
--
-- Generated bindings for @MTRClusterSmokeCOAlarm@.
module ObjC.Matter.MTRClusterSmokeCOAlarm
  ( MTRClusterSmokeCOAlarm
  , IsMTRClusterSmokeCOAlarm(..)
  , selfTestRequestWithParams_expectedValues_expectedValueInterval_completion
  , selfTestRequestWithExpectedValues_expectedValueInterval_completion
  , readAttributeExpressedStateWithParams
  , readAttributeSmokeStateWithParams
  , readAttributeCOStateWithParams
  , readAttributeBatteryAlertWithParams
  , readAttributeDeviceMutedWithParams
  , readAttributeTestInProgressWithParams
  , readAttributeHardwareFaultAlertWithParams
  , readAttributeEndOfServiceAlertWithParams
  , readAttributeInterconnectSmokeAlarmWithParams
  , readAttributeInterconnectCOAlarmWithParams
  , readAttributeContaminationStateWithParams
  , readAttributeSmokeSensitivityLevelWithParams
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_params
  , readAttributeExpiryDateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeExpressedStateWithParamsSelector
  , readAttributeSmokeStateWithParamsSelector
  , readAttributeCOStateWithParamsSelector
  , readAttributeBatteryAlertWithParamsSelector
  , readAttributeDeviceMutedWithParamsSelector
  , readAttributeTestInProgressWithParamsSelector
  , readAttributeHardwareFaultAlertWithParamsSelector
  , readAttributeEndOfServiceAlertWithParamsSelector
  , readAttributeInterconnectSmokeAlarmWithParamsSelector
  , readAttributeInterconnectCOAlarmWithParamsSelector
  , readAttributeContaminationStateWithParamsSelector
  , readAttributeSmokeSensitivityLevelWithParamsSelector
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeExpiryDateWithParamsSelector
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

-- | @- selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:@
selfTestRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRSmokeCOAlarmClusterSelfTestRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSmokeCOAlarm -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selfTestRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterSmokeCOAlarm  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterSmokeCOAlarm (mkSelector "selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- selfTestRequestWithExpectedValues:expectedValueInterval:completion:@
selfTestRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterSmokeCOAlarm -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
selfTestRequestWithExpectedValues_expectedValueInterval_completion mtrClusterSmokeCOAlarm  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterSmokeCOAlarm (mkSelector "selfTestRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeExpressedStateWithParams:@
readAttributeExpressedStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeExpressedStateWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeExpressedStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSmokeStateWithParams:@
readAttributeSmokeStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeSmokeStateWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeSmokeStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCOStateWithParams:@
readAttributeCOStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeCOStateWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeCOStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBatteryAlertWithParams:@
readAttributeBatteryAlertWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeBatteryAlertWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeBatteryAlertWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDeviceMutedWithParams:@
readAttributeDeviceMutedWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeDeviceMutedWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeDeviceMutedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTestInProgressWithParams:@
readAttributeTestInProgressWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeTestInProgressWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeTestInProgressWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHardwareFaultAlertWithParams:@
readAttributeHardwareFaultAlertWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeHardwareFaultAlertWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeHardwareFaultAlertWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeEndOfServiceAlertWithParams:@
readAttributeEndOfServiceAlertWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeEndOfServiceAlertWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeEndOfServiceAlertWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInterconnectSmokeAlarmWithParams:@
readAttributeInterconnectSmokeAlarmWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeInterconnectSmokeAlarmWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeInterconnectSmokeAlarmWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeInterconnectCOAlarmWithParams:@
readAttributeInterconnectCOAlarmWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeInterconnectCOAlarmWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeInterconnectCOAlarmWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeContaminationStateWithParams:@
readAttributeContaminationStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeContaminationStateWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeContaminationStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSmokeSensitivityLevelWithParams:@
readAttributeSmokeSensitivityLevelWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeSmokeSensitivityLevelWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeSmokeSensitivityLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterSmokeCOAlarm -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval mtrClusterSmokeCOAlarm  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterSmokeCOAlarm (mkSelector "writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_params :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterSmokeCOAlarm -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_params mtrClusterSmokeCOAlarm  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterSmokeCOAlarm (mkSelector "writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeExpiryDateWithParams:@
readAttributeExpiryDateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeExpiryDateWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeExpiryDateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSmokeCOAlarm  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterSmokeCOAlarm (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm => mtrClusterSmokeCOAlarm -> IO (Id MTRClusterSmokeCOAlarm)
init_ mtrClusterSmokeCOAlarm  =
    sendMsg mtrClusterSmokeCOAlarm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterSmokeCOAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSmokeCOAlarm"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSmokeCOAlarm -> device -> endpointID -> queue -> IO (Id MTRClusterSmokeCOAlarm)
initWithDevice_endpointID_queue mtrClusterSmokeCOAlarm  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterSmokeCOAlarm (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:@
selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @selfTestRequestWithExpectedValues:expectedValueInterval:completion:@
selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "selfTestRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeExpressedStateWithParams:@
readAttributeExpressedStateWithParamsSelector :: Selector
readAttributeExpressedStateWithParamsSelector = mkSelector "readAttributeExpressedStateWithParams:"

-- | @Selector@ for @readAttributeSmokeStateWithParams:@
readAttributeSmokeStateWithParamsSelector :: Selector
readAttributeSmokeStateWithParamsSelector = mkSelector "readAttributeSmokeStateWithParams:"

-- | @Selector@ for @readAttributeCOStateWithParams:@
readAttributeCOStateWithParamsSelector :: Selector
readAttributeCOStateWithParamsSelector = mkSelector "readAttributeCOStateWithParams:"

-- | @Selector@ for @readAttributeBatteryAlertWithParams:@
readAttributeBatteryAlertWithParamsSelector :: Selector
readAttributeBatteryAlertWithParamsSelector = mkSelector "readAttributeBatteryAlertWithParams:"

-- | @Selector@ for @readAttributeDeviceMutedWithParams:@
readAttributeDeviceMutedWithParamsSelector :: Selector
readAttributeDeviceMutedWithParamsSelector = mkSelector "readAttributeDeviceMutedWithParams:"

-- | @Selector@ for @readAttributeTestInProgressWithParams:@
readAttributeTestInProgressWithParamsSelector :: Selector
readAttributeTestInProgressWithParamsSelector = mkSelector "readAttributeTestInProgressWithParams:"

-- | @Selector@ for @readAttributeHardwareFaultAlertWithParams:@
readAttributeHardwareFaultAlertWithParamsSelector :: Selector
readAttributeHardwareFaultAlertWithParamsSelector = mkSelector "readAttributeHardwareFaultAlertWithParams:"

-- | @Selector@ for @readAttributeEndOfServiceAlertWithParams:@
readAttributeEndOfServiceAlertWithParamsSelector :: Selector
readAttributeEndOfServiceAlertWithParamsSelector = mkSelector "readAttributeEndOfServiceAlertWithParams:"

-- | @Selector@ for @readAttributeInterconnectSmokeAlarmWithParams:@
readAttributeInterconnectSmokeAlarmWithParamsSelector :: Selector
readAttributeInterconnectSmokeAlarmWithParamsSelector = mkSelector "readAttributeInterconnectSmokeAlarmWithParams:"

-- | @Selector@ for @readAttributeInterconnectCOAlarmWithParams:@
readAttributeInterconnectCOAlarmWithParamsSelector :: Selector
readAttributeInterconnectCOAlarmWithParamsSelector = mkSelector "readAttributeInterconnectCOAlarmWithParams:"

-- | @Selector@ for @readAttributeContaminationStateWithParams:@
readAttributeContaminationStateWithParamsSelector :: Selector
readAttributeContaminationStateWithParamsSelector = mkSelector "readAttributeContaminationStateWithParams:"

-- | @Selector@ for @readAttributeSmokeSensitivityLevelWithParams:@
readAttributeSmokeSensitivityLevelWithParamsSelector :: Selector
readAttributeSmokeSensitivityLevelWithParamsSelector = mkSelector "readAttributeSmokeSensitivityLevelWithParams:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeExpiryDateWithParams:@
readAttributeExpiryDateWithParamsSelector :: Selector
readAttributeExpiryDateWithParamsSelector = mkSelector "readAttributeExpiryDateWithParams:"

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

