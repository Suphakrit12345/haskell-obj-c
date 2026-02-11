{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Smoke CO Alarm
--
-- This cluster provides an interface for observing and managing the state of smoke and CO alarms.
--
-- Generated bindings for @MTRBaseClusterSmokeCOAlarm@.
module ObjC.Matter.MTRBaseClusterSmokeCOAlarm
  ( MTRBaseClusterSmokeCOAlarm
  , IsMTRBaseClusterSmokeCOAlarm(..)
  , selfTestRequestWithParams_completion
  , selfTestRequestWithCompletion
  , readAttributeExpressedStateWithCompletion
  , subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSmokeStateWithCompletion
  , subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeCOStateWithCompletion
  , subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCOStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeBatteryAlertWithCompletion
  , subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandler
  , readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completion
  , readAttributeDeviceMutedWithCompletion
  , subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandler
  , readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completion
  , readAttributeTestInProgressWithCompletion
  , subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandler
  , readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completion
  , readAttributeHardwareFaultAlertWithCompletion
  , subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandler
  , readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndOfServiceAlertWithCompletion
  , subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterconnectSmokeAlarmWithCompletion
  , subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterconnectCOAlarmWithCompletion
  , subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completion
  , readAttributeContaminationStateWithCompletion
  , subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSmokeSensitivityLevelWithCompletion
  , writeAttributeSmokeSensitivityLevelWithValue_completion
  , writeAttributeSmokeSensitivityLevelWithValue_params_completion
  , subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeExpiryDateWithCompletion
  , subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeGeneratedCommandListWithCompletion
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAcceptedCommandListWithCompletion
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAttributeListWithCompletion
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeFeatureMapWithCompletion
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeClusterRevisionWithCompletion
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion
  , init_
  , new
  , initWithDevice_endpointID_queue
  , selfTestRequestWithParams_completionSelector
  , selfTestRequestWithCompletionSelector
  , readAttributeExpressedStateWithCompletionSelector
  , subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSmokeStateWithCompletionSelector
  , subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCOStateWithCompletionSelector
  , subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBatteryAlertWithCompletionSelector
  , subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDeviceMutedWithCompletionSelector
  , subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTestInProgressWithCompletionSelector
  , subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareFaultAlertWithCompletionSelector
  , subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndOfServiceAlertWithCompletionSelector
  , subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterconnectSmokeAlarmWithCompletionSelector
  , subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterconnectCOAlarmWithCompletionSelector
  , subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeContaminationStateWithCompletionSelector
  , subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSmokeSensitivityLevelWithCompletionSelector
  , writeAttributeSmokeSensitivityLevelWithValue_completionSelector
  , writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector
  , subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeExpiryDateWithCompletionSelector
  , subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SelfTestRequest
--
-- This command SHALL initiate a device self-test.
--
-- ObjC selector: @- selfTestRequestWithParams:completion:@
selfTestRequestWithParams_completion :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSmokeCOAlarmClusterSelfTestRequestParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> IO ()
selfTestRequestWithParams_completion mtrBaseClusterSmokeCOAlarm  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "selfTestRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- selfTestRequestWithCompletion:@
selfTestRequestWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
selfTestRequestWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "selfTestRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeExpressedStateWithCompletion:@
readAttributeExpressedStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeExpressedStateWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeExpressedStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSmokeStateWithCompletion:@
readAttributeSmokeStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeSmokeStateWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeSmokeStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCOStateWithCompletion:@
readAttributeCOStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeCOStateWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeCOStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCOStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCOStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBatteryAlertWithCompletion:@
readAttributeBatteryAlertWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeBatteryAlertWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeBatteryAlertWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDeviceMutedWithCompletion:@
readAttributeDeviceMutedWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeDeviceMutedWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeDeviceMutedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:@
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTestInProgressWithCompletion:@
readAttributeTestInProgressWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeTestInProgressWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeTestInProgressWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeHardwareFaultAlertWithCompletion:@
readAttributeHardwareFaultAlertWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeHardwareFaultAlertWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeHardwareFaultAlertWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEndOfServiceAlertWithCompletion:@
readAttributeEndOfServiceAlertWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeEndOfServiceAlertWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeEndOfServiceAlertWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInterconnectSmokeAlarmWithCompletion:@
readAttributeInterconnectSmokeAlarmWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeInterconnectSmokeAlarmWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeInterconnectSmokeAlarmWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInterconnectCOAlarmWithCompletion:@
readAttributeInterconnectCOAlarmWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeInterconnectCOAlarmWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeInterconnectCOAlarmWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeContaminationStateWithCompletion:@
readAttributeContaminationStateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeContaminationStateWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeContaminationStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSmokeSensitivityLevelWithCompletion:@
readAttributeSmokeSensitivityLevelWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeSmokeSensitivityLevelWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeSmokeSensitivityLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSmokeSensitivityLevelWithValue:completion:@
writeAttributeSmokeSensitivityLevelWithValue_completion :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsNSNumber value) => mtrBaseClusterSmokeCOAlarm -> value -> Ptr () -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_completion mtrBaseClusterSmokeCOAlarm  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "writeAttributeSmokeSensitivityLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSmokeSensitivityLevelWithValue:params:completion:@
writeAttributeSmokeSensitivityLevelWithValue_params_completion :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterSmokeCOAlarm -> value -> params -> Ptr () -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_params_completion mtrBaseClusterSmokeCOAlarm  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "writeAttributeSmokeSensitivityLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeExpiryDateWithCompletion:@
readAttributeExpiryDateWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeExpiryDateWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeExpiryDateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterSmokeCOAlarm  completion =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRSubscribeParams params) => mtrBaseClusterSmokeCOAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSmokeCOAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm => mtrBaseClusterSmokeCOAlarm -> IO (Id MTRBaseClusterSmokeCOAlarm)
init_ mtrBaseClusterSmokeCOAlarm  =
    sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterSmokeCOAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterSmokeCOAlarm"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterSmokeCOAlarm mtrBaseClusterSmokeCOAlarm, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterSmokeCOAlarm -> device -> endpointID -> queue -> IO (Id MTRBaseClusterSmokeCOAlarm)
initWithDevice_endpointID_queue mtrBaseClusterSmokeCOAlarm  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterSmokeCOAlarm (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selfTestRequestWithParams:completion:@
selfTestRequestWithParams_completionSelector :: Selector
selfTestRequestWithParams_completionSelector = mkSelector "selfTestRequestWithParams:completion:"

-- | @Selector@ for @selfTestRequestWithCompletion:@
selfTestRequestWithCompletionSelector :: Selector
selfTestRequestWithCompletionSelector = mkSelector "selfTestRequestWithCompletion:"

-- | @Selector@ for @readAttributeExpressedStateWithCompletion:@
readAttributeExpressedStateWithCompletionSelector :: Selector
readAttributeExpressedStateWithCompletionSelector = mkSelector "readAttributeExpressedStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeExpressedStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExpressedStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeExpressedStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeExpressedStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSmokeStateWithCompletion:@
readAttributeSmokeStateWithCompletionSelector :: Selector
readAttributeSmokeStateWithCompletionSelector = mkSelector "readAttributeSmokeStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSmokeStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSmokeStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSmokeStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSmokeStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCOStateWithCompletion:@
readAttributeCOStateWithCompletionSelector :: Selector
readAttributeCOStateWithCompletionSelector = mkSelector "readAttributeCOStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCOStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCOStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCOStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCOStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBatteryAlertWithCompletion:@
readAttributeBatteryAlertWithCompletionSelector :: Selector
readAttributeBatteryAlertWithCompletionSelector = mkSelector "readAttributeBatteryAlertWithCompletion:"

-- | @Selector@ for @subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBatteryAlertWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBatteryAlertWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBatteryAlertWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBatteryAlertWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDeviceMutedWithCompletion:@
readAttributeDeviceMutedWithCompletionSelector :: Selector
readAttributeDeviceMutedWithCompletionSelector = mkSelector "readAttributeDeviceMutedWithCompletion:"

-- | @Selector@ for @subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDeviceMutedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDeviceMutedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:@
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDeviceMutedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDeviceMutedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTestInProgressWithCompletion:@
readAttributeTestInProgressWithCompletionSelector :: Selector
readAttributeTestInProgressWithCompletionSelector = mkSelector "readAttributeTestInProgressWithCompletion:"

-- | @Selector@ for @subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTestInProgressWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTestInProgressWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:@
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTestInProgressWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTestInProgressWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHardwareFaultAlertWithCompletion:@
readAttributeHardwareFaultAlertWithCompletionSelector :: Selector
readAttributeHardwareFaultAlertWithCompletionSelector = mkSelector "readAttributeHardwareFaultAlertWithCompletion:"

-- | @Selector@ for @subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHardwareFaultAlertWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareFaultAlertWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHardwareFaultAlertWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHardwareFaultAlertWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndOfServiceAlertWithCompletion:@
readAttributeEndOfServiceAlertWithCompletionSelector :: Selector
readAttributeEndOfServiceAlertWithCompletionSelector = mkSelector "readAttributeEndOfServiceAlertWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEndOfServiceAlertWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndOfServiceAlertWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEndOfServiceAlertWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndOfServiceAlertWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterconnectSmokeAlarmWithCompletion:@
readAttributeInterconnectSmokeAlarmWithCompletionSelector :: Selector
readAttributeInterconnectSmokeAlarmWithCompletionSelector = mkSelector "readAttributeInterconnectSmokeAlarmWithCompletion:"

-- | @Selector@ for @subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInterconnectSmokeAlarmWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterconnectSmokeAlarmWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInterconnectSmokeAlarmWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterconnectSmokeAlarmWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterconnectCOAlarmWithCompletion:@
readAttributeInterconnectCOAlarmWithCompletionSelector :: Selector
readAttributeInterconnectCOAlarmWithCompletionSelector = mkSelector "readAttributeInterconnectCOAlarmWithCompletion:"

-- | @Selector@ for @subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInterconnectCOAlarmWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterconnectCOAlarmWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInterconnectCOAlarmWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterconnectCOAlarmWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeContaminationStateWithCompletion:@
readAttributeContaminationStateWithCompletionSelector :: Selector
readAttributeContaminationStateWithCompletionSelector = mkSelector "readAttributeContaminationStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeContaminationStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeContaminationStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeContaminationStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeContaminationStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSmokeSensitivityLevelWithCompletion:@
readAttributeSmokeSensitivityLevelWithCompletionSelector :: Selector
readAttributeSmokeSensitivityLevelWithCompletionSelector = mkSelector "readAttributeSmokeSensitivityLevelWithCompletion:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:completion:@
writeAttributeSmokeSensitivityLevelWithValue_completionSelector :: Selector
writeAttributeSmokeSensitivityLevelWithValue_completionSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:params:completion:@
writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector :: Selector
writeAttributeSmokeSensitivityLevelWithValue_params_completionSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSmokeSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSmokeSensitivityLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSmokeSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSmokeSensitivityLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeExpiryDateWithCompletion:@
readAttributeExpiryDateWithCompletionSelector :: Selector
readAttributeExpiryDateWithCompletionSelector = mkSelector "readAttributeExpiryDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeExpiryDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeExpiryDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeExpiryDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeExpiryDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletionSelector :: Selector
readAttributeGeneratedCommandListWithCompletionSelector = mkSelector "readAttributeGeneratedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletionSelector :: Selector
readAttributeAcceptedCommandListWithCompletionSelector = mkSelector "readAttributeAcceptedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletionSelector :: Selector
readAttributeAttributeListWithCompletionSelector = mkSelector "readAttributeAttributeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletionSelector :: Selector
readAttributeFeatureMapWithCompletionSelector = mkSelector "readAttributeFeatureMapWithCompletion:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletionSelector :: Selector
readAttributeClusterRevisionWithCompletionSelector = mkSelector "readAttributeClusterRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

