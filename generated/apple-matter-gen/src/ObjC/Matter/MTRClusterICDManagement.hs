{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster ICD Management    Allows servers to ensure that listed clients are notified when a server is available for communication.
--
-- Generated bindings for @MTRClusterICDManagement@.
module ObjC.Matter.MTRClusterICDManagement
  ( MTRClusterICDManagement
  , IsMTRClusterICDManagement(..)
  , registerClientWithParams_expectedValues_expectedValueInterval_completion
  , unregisterClientWithParams_expectedValues_expectedValueInterval_completion
  , stayActiveRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeIdleModeDurationWithParams
  , readAttributeActiveModeDurationWithParams
  , readAttributeActiveModeThresholdWithParams
  , readAttributeRegisteredClientsWithParams
  , readAttributeICDCounterWithParams
  , readAttributeClientsSupportedPerFabricWithParams
  , readAttributeUserActiveModeTriggerHintWithParams
  , readAttributeUserActiveModeTriggerInstructionWithParams
  , readAttributeOperatingModeWithParams
  , readAttributeMaximumCheckInBackOffWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , registerClientWithParams_expectedValues_expectedValueInterval_completionSelector
  , unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector
  , stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeIdleModeDurationWithParamsSelector
  , readAttributeActiveModeDurationWithParamsSelector
  , readAttributeActiveModeThresholdWithParamsSelector
  , readAttributeRegisteredClientsWithParamsSelector
  , readAttributeICDCounterWithParamsSelector
  , readAttributeClientsSupportedPerFabricWithParamsSelector
  , readAttributeUserActiveModeTriggerHintWithParamsSelector
  , readAttributeUserActiveModeTriggerInstructionWithParamsSelector
  , readAttributeOperatingModeWithParamsSelector
  , readAttributeMaximumCheckInBackOffWithParamsSelector
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

-- | @- registerClientWithParams:expectedValues:expectedValueInterval:completion:@
registerClientWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRICDManagementClusterRegisterClientParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterICDManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
registerClientWithParams_expectedValues_expectedValueInterval_completion mtrClusterICDManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterICDManagement (mkSelector "registerClientWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unregisterClientWithParams:expectedValues:expectedValueInterval:completion:@
unregisterClientWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRICDManagementClusterUnregisterClientParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterICDManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unregisterClientWithParams_expectedValues_expectedValueInterval_completion mtrClusterICDManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterICDManagement (mkSelector "unregisterClientWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:@
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRICDManagementClusterStayActiveRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterICDManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterICDManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterICDManagement (mkSelector "stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeIdleModeDurationWithParams:@
readAttributeIdleModeDurationWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeIdleModeDurationWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeIdleModeDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveModeDurationWithParams:@
readAttributeActiveModeDurationWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeActiveModeDurationWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeActiveModeDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveModeThresholdWithParams:@
readAttributeActiveModeThresholdWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeActiveModeThresholdWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeActiveModeThresholdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRegisteredClientsWithParams:@
readAttributeRegisteredClientsWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeRegisteredClientsWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeRegisteredClientsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeICDCounterWithParams:@
readAttributeICDCounterWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeICDCounterWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeICDCounterWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClientsSupportedPerFabricWithParams:@
readAttributeClientsSupportedPerFabricWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeClientsSupportedPerFabricWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeClientsSupportedPerFabricWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUserActiveModeTriggerHintWithParams:@
readAttributeUserActiveModeTriggerHintWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeUserActiveModeTriggerHintWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeUserActiveModeTriggerHintWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeUserActiveModeTriggerInstructionWithParams:@
readAttributeUserActiveModeTriggerInstructionWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeUserActiveModeTriggerInstructionWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeUserActiveModeTriggerInstructionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperatingModeWithParams:@
readAttributeOperatingModeWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeOperatingModeWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeOperatingModeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaximumCheckInBackOffWithParams:@
readAttributeMaximumCheckInBackOffWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeMaximumCheckInBackOffWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeMaximumCheckInBackOffWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterICDManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterICDManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterICDManagement mtrClusterICDManagement => mtrClusterICDManagement -> IO (Id MTRClusterICDManagement)
init_ mtrClusterICDManagement  =
    sendMsg mtrClusterICDManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterICDManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterICDManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterICDManagement -> device -> endpointID -> queue -> IO (Id MTRClusterICDManagement)
initWithDevice_endpointID_queue mtrClusterICDManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterICDManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerClientWithParams:expectedValues:expectedValueInterval:completion:@
registerClientWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
registerClientWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "registerClientWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unregisterClientWithParams:expectedValues:expectedValueInterval:completion:@
unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unregisterClientWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:@
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeIdleModeDurationWithParams:@
readAttributeIdleModeDurationWithParamsSelector :: Selector
readAttributeIdleModeDurationWithParamsSelector = mkSelector "readAttributeIdleModeDurationWithParams:"

-- | @Selector@ for @readAttributeActiveModeDurationWithParams:@
readAttributeActiveModeDurationWithParamsSelector :: Selector
readAttributeActiveModeDurationWithParamsSelector = mkSelector "readAttributeActiveModeDurationWithParams:"

-- | @Selector@ for @readAttributeActiveModeThresholdWithParams:@
readAttributeActiveModeThresholdWithParamsSelector :: Selector
readAttributeActiveModeThresholdWithParamsSelector = mkSelector "readAttributeActiveModeThresholdWithParams:"

-- | @Selector@ for @readAttributeRegisteredClientsWithParams:@
readAttributeRegisteredClientsWithParamsSelector :: Selector
readAttributeRegisteredClientsWithParamsSelector = mkSelector "readAttributeRegisteredClientsWithParams:"

-- | @Selector@ for @readAttributeICDCounterWithParams:@
readAttributeICDCounterWithParamsSelector :: Selector
readAttributeICDCounterWithParamsSelector = mkSelector "readAttributeICDCounterWithParams:"

-- | @Selector@ for @readAttributeClientsSupportedPerFabricWithParams:@
readAttributeClientsSupportedPerFabricWithParamsSelector :: Selector
readAttributeClientsSupportedPerFabricWithParamsSelector = mkSelector "readAttributeClientsSupportedPerFabricWithParams:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerHintWithParams:@
readAttributeUserActiveModeTriggerHintWithParamsSelector :: Selector
readAttributeUserActiveModeTriggerHintWithParamsSelector = mkSelector "readAttributeUserActiveModeTriggerHintWithParams:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerInstructionWithParams:@
readAttributeUserActiveModeTriggerInstructionWithParamsSelector :: Selector
readAttributeUserActiveModeTriggerInstructionWithParamsSelector = mkSelector "readAttributeUserActiveModeTriggerInstructionWithParams:"

-- | @Selector@ for @readAttributeOperatingModeWithParams:@
readAttributeOperatingModeWithParamsSelector :: Selector
readAttributeOperatingModeWithParamsSelector = mkSelector "readAttributeOperatingModeWithParams:"

-- | @Selector@ for @readAttributeMaximumCheckInBackOffWithParams:@
readAttributeMaximumCheckInBackOffWithParamsSelector :: Selector
readAttributeMaximumCheckInBackOffWithParamsSelector = mkSelector "readAttributeMaximumCheckInBackOffWithParams:"

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

