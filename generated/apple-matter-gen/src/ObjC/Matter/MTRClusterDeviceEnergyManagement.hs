{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Device Energy Management    This cluster allows a client to manage the power draw of a device. An example of such a client could be an Energy Management System (EMS) which controls an Energy Smart Appliance (ESA).
--
-- Generated bindings for @MTRClusterDeviceEnergyManagement@.
module ObjC.Matter.MTRClusterDeviceEnergyManagement
  ( MTRClusterDeviceEnergyManagement
  , IsMTRClusterDeviceEnergyManagement(..)
  , powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completion
  , startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completion
  , pauseRequestWithParams_expectedValues_expectedValueInterval_completion
  , resumeRequestWithParams_expectedValues_expectedValueInterval_completion
  , resumeRequestWithExpectedValues_expectedValueInterval_completion
  , modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completion
  , requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completion
  , cancelRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelRequestWithExpectedValues_expectedValueInterval_completion
  , readAttributeESATypeWithParams
  , readAttributeESACanGenerateWithParams
  , readAttributeESAStateWithParams
  , readAttributeAbsMinPowerWithParams
  , readAttributeAbsMaxPowerWithParams
  , readAttributePowerAdjustmentCapabilityWithParams
  , readAttributeForecastWithParams
  , readAttributeOptOutStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector
  , startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , resumeRequestWithExpectedValues_expectedValueInterval_completionSelector
  , modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelRequestWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeESATypeWithParamsSelector
  , readAttributeESACanGenerateWithParamsSelector
  , readAttributeESAStateWithParamsSelector
  , readAttributeAbsMinPowerWithParamsSelector
  , readAttributeAbsMaxPowerWithParamsSelector
  , readAttributePowerAdjustmentCapabilityWithParamsSelector
  , readAttributeForecastWithParamsSelector
  , readAttributeOptOutStateWithParamsSelector
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

-- | @- powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDeviceEnergyManagement (mkSelector "cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseRequestWithParams:expectedValues:expectedValueInterval:completion:@
pauseRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPauseRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "pauseRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeRequestWithParams:expectedValues:expectedValueInterval:completion:@
resumeRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterResumeRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "resumeRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeRequestWithExpectedValues:expectedValueInterval:completion:@
resumeRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeRequestWithExpectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDeviceEnergyManagement (mkSelector "resumeRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:@
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "cancelRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelRequestWithExpectedValues:expectedValueInterval:completion:@
cancelRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelRequestWithExpectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterDeviceEnergyManagement (mkSelector "cancelRequestWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeESATypeWithParams:@
readAttributeESATypeWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeESATypeWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeESATypeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeESACanGenerateWithParams:@
readAttributeESACanGenerateWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeESACanGenerateWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeESACanGenerateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeESAStateWithParams:@
readAttributeESAStateWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeESAStateWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeESAStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAbsMinPowerWithParams:@
readAttributeAbsMinPowerWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAbsMinPowerWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeAbsMinPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAbsMaxPowerWithParams:@
readAttributeAbsMaxPowerWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAbsMaxPowerWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeAbsMaxPowerWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePowerAdjustmentCapabilityWithParams:@
readAttributePowerAdjustmentCapabilityWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributePowerAdjustmentCapabilityWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributePowerAdjustmentCapabilityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeForecastWithParams:@
readAttributeForecastWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeForecastWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeForecastWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOptOutStateWithParams:@
readAttributeOptOutStateWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeOptOutStateWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeOptOutStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDeviceEnergyManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterDeviceEnergyManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement => mtrClusterDeviceEnergyManagement -> IO (Id MTRClusterDeviceEnergyManagement)
init_ mtrClusterDeviceEnergyManagement  =
    sendMsg mtrClusterDeviceEnergyManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterDeviceEnergyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDeviceEnergyManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDeviceEnergyManagement -> device -> endpointID -> queue -> IO (Id MTRClusterDeviceEnergyManagement)
initWithDevice_endpointID_queue mtrClusterDeviceEnergyManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterDeviceEnergyManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseRequestWithParams:expectedValues:expectedValueInterval:completion:@
pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeRequestWithParams:expectedValues:expectedValueInterval:completion:@
resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeRequestWithExpectedValues:expectedValueInterval:completion:@
resumeRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resumeRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resumeRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:@
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelRequestWithExpectedValues:expectedValueInterval:completion:@
cancelRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector
cancelRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "cancelRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeESATypeWithParams:@
readAttributeESATypeWithParamsSelector :: Selector
readAttributeESATypeWithParamsSelector = mkSelector "readAttributeESATypeWithParams:"

-- | @Selector@ for @readAttributeESACanGenerateWithParams:@
readAttributeESACanGenerateWithParamsSelector :: Selector
readAttributeESACanGenerateWithParamsSelector = mkSelector "readAttributeESACanGenerateWithParams:"

-- | @Selector@ for @readAttributeESAStateWithParams:@
readAttributeESAStateWithParamsSelector :: Selector
readAttributeESAStateWithParamsSelector = mkSelector "readAttributeESAStateWithParams:"

-- | @Selector@ for @readAttributeAbsMinPowerWithParams:@
readAttributeAbsMinPowerWithParamsSelector :: Selector
readAttributeAbsMinPowerWithParamsSelector = mkSelector "readAttributeAbsMinPowerWithParams:"

-- | @Selector@ for @readAttributeAbsMaxPowerWithParams:@
readAttributeAbsMaxPowerWithParamsSelector :: Selector
readAttributeAbsMaxPowerWithParamsSelector = mkSelector "readAttributeAbsMaxPowerWithParams:"

-- | @Selector@ for @readAttributePowerAdjustmentCapabilityWithParams:@
readAttributePowerAdjustmentCapabilityWithParamsSelector :: Selector
readAttributePowerAdjustmentCapabilityWithParamsSelector = mkSelector "readAttributePowerAdjustmentCapabilityWithParams:"

-- | @Selector@ for @readAttributeForecastWithParams:@
readAttributeForecastWithParamsSelector :: Selector
readAttributeForecastWithParamsSelector = mkSelector "readAttributeForecastWithParams:"

-- | @Selector@ for @readAttributeOptOutStateWithParams:@
readAttributeOptOutStateWithParamsSelector :: Selector
readAttributeOptOutStateWithParamsSelector = mkSelector "readAttributeOptOutStateWithParams:"

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

