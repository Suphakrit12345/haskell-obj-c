{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster General Diagnostics    The General Diagnostics Cluster, along with other diagnostics clusters, provide a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRClusterGeneralDiagnostics@.
module ObjC.Matter.MTRClusterGeneralDiagnostics
  ( MTRClusterGeneralDiagnostics
  , IsMTRClusterGeneralDiagnostics(..)
  , testEventTriggerWithParams_expectedValues_expectedValueInterval_completion
  , timeSnapshotWithParams_expectedValues_expectedValueInterval_completion
  , timeSnapshotWithExpectedValues_expectedValueInterval_completion
  , payloadTestRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeNetworkInterfacesWithParams
  , readAttributeRebootCountWithParams
  , readAttributeUpTimeWithParams
  , readAttributeTotalOperationalHoursWithParams
  , readAttributeBootReasonWithParams
  , readAttributeActiveHardwareFaultsWithParams
  , readAttributeActiveRadioFaultsWithParams
  , readAttributeActiveNetworkFaultsWithParams
  , readAttributeTestEventTriggersEnabledWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , testEventTriggerWithParams_expectedValues_expectedValueInterval_completionHandler
  , readAttributeBootReasonsWithParams
  , initWithDevice_endpointID_queue
  , testEventTriggerWithParams_expectedValues_expectedValueInterval_completionSelector
  , timeSnapshotWithParams_expectedValues_expectedValueInterval_completionSelector
  , timeSnapshotWithExpectedValues_expectedValueInterval_completionSelector
  , payloadTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeNetworkInterfacesWithParamsSelector
  , readAttributeRebootCountWithParamsSelector
  , readAttributeUpTimeWithParamsSelector
  , readAttributeTotalOperationalHoursWithParamsSelector
  , readAttributeBootReasonWithParamsSelector
  , readAttributeActiveHardwareFaultsWithParamsSelector
  , readAttributeActiveRadioFaultsWithParamsSelector
  , readAttributeActiveNetworkFaultsWithParamsSelector
  , readAttributeTestEventTriggersEnabledWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , testEventTriggerWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , readAttributeBootReasonsWithParamsSelector
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

-- | @- testEventTriggerWithParams:expectedValues:expectedValueInterval:completion:@
testEventTriggerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterTestEventTriggerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testEventTriggerWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralDiagnostics (mkSelector "testEventTriggerWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- timeSnapshotWithParams:expectedValues:expectedValueInterval:completion:@
timeSnapshotWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterTimeSnapshotParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
timeSnapshotWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralDiagnostics (mkSelector "timeSnapshotWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- timeSnapshotWithExpectedValues:expectedValueInterval:completion:@
timeSnapshotWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
timeSnapshotWithExpectedValues_expectedValueInterval_completion mtrClusterGeneralDiagnostics  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterGeneralDiagnostics (mkSelector "timeSnapshotWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- payloadTestRequestWithParams:expectedValues:expectedValueInterval:completion:@
payloadTestRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
payloadTestRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralDiagnostics (mkSelector "payloadTestRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNetworkInterfacesWithParams:@
readAttributeNetworkInterfacesWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNetworkInterfacesWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeNetworkInterfacesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRebootCountWithParams:@
readAttributeRebootCountWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRebootCountWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeRebootCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUpTimeWithParams:@
readAttributeUpTimeWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeUpTimeWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeUpTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTotalOperationalHoursWithParams:@
readAttributeTotalOperationalHoursWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTotalOperationalHoursWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeTotalOperationalHoursWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBootReasonWithParams:@
readAttributeBootReasonWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBootReasonWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeBootReasonWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveHardwareFaultsWithParams:@
readAttributeActiveHardwareFaultsWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveHardwareFaultsWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeActiveHardwareFaultsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveRadioFaultsWithParams:@
readAttributeActiveRadioFaultsWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveRadioFaultsWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeActiveRadioFaultsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveNetworkFaultsWithParams:@
readAttributeActiveNetworkFaultsWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveNetworkFaultsWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeActiveNetworkFaultsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTestEventTriggersEnabledWithParams:@
readAttributeTestEventTriggersEnabledWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTestEventTriggersEnabledWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeTestEventTriggersEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics => mtrClusterGeneralDiagnostics -> IO (Id MTRClusterGeneralDiagnostics)
init_ mtrClusterGeneralDiagnostics  =
    sendMsg mtrClusterGeneralDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterGeneralDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGeneralDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterGeneralDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterGeneralDiagnostics)
initWithDevice_endpoint_queue mtrClusterGeneralDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterGeneralDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- testEventTriggerWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEventTriggerWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRGeneralDiagnosticsClusterTestEventTriggerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
testEventTriggerWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterGeneralDiagnostics (mkSelector "testEventTriggerWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeBootReasonsWithParams:@
readAttributeBootReasonsWithParams :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRReadParams params) => mtrClusterGeneralDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBootReasonsWithParams mtrClusterGeneralDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterGeneralDiagnostics (mkSelector "readAttributeBootReasonsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGeneralDiagnostics mtrClusterGeneralDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGeneralDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterGeneralDiagnostics)
initWithDevice_endpointID_queue mtrClusterGeneralDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterGeneralDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @testEventTriggerWithParams:expectedValues:expectedValueInterval:completion:@
testEventTriggerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
testEventTriggerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "testEventTriggerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @timeSnapshotWithParams:expectedValues:expectedValueInterval:completion:@
timeSnapshotWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
timeSnapshotWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "timeSnapshotWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @timeSnapshotWithExpectedValues:expectedValueInterval:completion:@
timeSnapshotWithExpectedValues_expectedValueInterval_completionSelector :: Selector
timeSnapshotWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "timeSnapshotWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @payloadTestRequestWithParams:expectedValues:expectedValueInterval:completion:@
payloadTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
payloadTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "payloadTestRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeNetworkInterfacesWithParams:@
readAttributeNetworkInterfacesWithParamsSelector :: Selector
readAttributeNetworkInterfacesWithParamsSelector = mkSelector "readAttributeNetworkInterfacesWithParams:"

-- | @Selector@ for @readAttributeRebootCountWithParams:@
readAttributeRebootCountWithParamsSelector :: Selector
readAttributeRebootCountWithParamsSelector = mkSelector "readAttributeRebootCountWithParams:"

-- | @Selector@ for @readAttributeUpTimeWithParams:@
readAttributeUpTimeWithParamsSelector :: Selector
readAttributeUpTimeWithParamsSelector = mkSelector "readAttributeUpTimeWithParams:"

-- | @Selector@ for @readAttributeTotalOperationalHoursWithParams:@
readAttributeTotalOperationalHoursWithParamsSelector :: Selector
readAttributeTotalOperationalHoursWithParamsSelector = mkSelector "readAttributeTotalOperationalHoursWithParams:"

-- | @Selector@ for @readAttributeBootReasonWithParams:@
readAttributeBootReasonWithParamsSelector :: Selector
readAttributeBootReasonWithParamsSelector = mkSelector "readAttributeBootReasonWithParams:"

-- | @Selector@ for @readAttributeActiveHardwareFaultsWithParams:@
readAttributeActiveHardwareFaultsWithParamsSelector :: Selector
readAttributeActiveHardwareFaultsWithParamsSelector = mkSelector "readAttributeActiveHardwareFaultsWithParams:"

-- | @Selector@ for @readAttributeActiveRadioFaultsWithParams:@
readAttributeActiveRadioFaultsWithParamsSelector :: Selector
readAttributeActiveRadioFaultsWithParamsSelector = mkSelector "readAttributeActiveRadioFaultsWithParams:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsWithParams:@
readAttributeActiveNetworkFaultsWithParamsSelector :: Selector
readAttributeActiveNetworkFaultsWithParamsSelector = mkSelector "readAttributeActiveNetworkFaultsWithParams:"

-- | @Selector@ for @readAttributeTestEventTriggersEnabledWithParams:@
readAttributeTestEventTriggersEnabledWithParamsSelector :: Selector
readAttributeTestEventTriggersEnabledWithParamsSelector = mkSelector "readAttributeTestEventTriggersEnabledWithParams:"

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

-- | @Selector@ for @testEventTriggerWithParams:expectedValues:expectedValueInterval:completionHandler:@
testEventTriggerWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
testEventTriggerWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "testEventTriggerWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @readAttributeBootReasonsWithParams:@
readAttributeBootReasonsWithParamsSelector :: Selector
readAttributeBootReasonsWithParamsSelector = mkSelector "readAttributeBootReasonsWithParams:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

