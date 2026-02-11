{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Device Energy Management
--
-- This cluster allows a client to manage the power draw of a device. An example of such a client could be an Energy Management System (EMS) which controls an Energy Smart Appliance (ESA).
--
-- Generated bindings for @MTRBaseClusterDeviceEnergyManagement@.
module ObjC.Matter.MTRBaseClusterDeviceEnergyManagement
  ( MTRBaseClusterDeviceEnergyManagement
  , IsMTRBaseClusterDeviceEnergyManagement(..)
  , powerAdjustRequestWithParams_completion
  , cancelPowerAdjustRequestWithParams_completion
  , cancelPowerAdjustRequestWithCompletion
  , startTimeAdjustRequestWithParams_completion
  , pauseRequestWithParams_completion
  , resumeRequestWithParams_completion
  , resumeRequestWithCompletion
  , modifyForecastRequestWithParams_completion
  , requestConstraintBasedForecastWithParams_completion
  , cancelRequestWithParams_completion
  , cancelRequestWithCompletion
  , readAttributeESATypeWithCompletion
  , subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeESATypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeESACanGenerateWithCompletion
  , subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandler
  , readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completion
  , readAttributeESAStateWithCompletion
  , subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeESAStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeAbsMinPowerWithCompletion
  , subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeAbsMaxPowerWithCompletion
  , subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerAdjustmentCapabilityWithCompletion
  , subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completion
  , readAttributeForecastWithCompletion
  , subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandler
  , readAttributeForecastWithClusterStateCache_endpoint_queue_completion
  , readAttributeOptOutStateWithCompletion
  , subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completion
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
  , powerAdjustRequestWithParams_completionSelector
  , cancelPowerAdjustRequestWithParams_completionSelector
  , cancelPowerAdjustRequestWithCompletionSelector
  , startTimeAdjustRequestWithParams_completionSelector
  , pauseRequestWithParams_completionSelector
  , resumeRequestWithParams_completionSelector
  , resumeRequestWithCompletionSelector
  , modifyForecastRequestWithParams_completionSelector
  , requestConstraintBasedForecastWithParams_completionSelector
  , cancelRequestWithParams_completionSelector
  , cancelRequestWithCompletionSelector
  , readAttributeESATypeWithCompletionSelector
  , subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeESACanGenerateWithCompletionSelector
  , subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeESAStateWithCompletionSelector
  , subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAbsMinPowerWithCompletionSelector
  , subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAbsMaxPowerWithCompletionSelector
  , subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerAdjustmentCapabilityWithCompletionSelector
  , subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeForecastWithCompletionSelector
  , subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOptOutStateWithCompletionSelector
  , subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command PowerAdjustRequest
--
-- Allows a client to request an adjustment in the power consumption of an ESA for a specified duration.
--
-- ObjC selector: @- powerAdjustRequestWithParams:completion:@
powerAdjustRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
powerAdjustRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "powerAdjustRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CancelPowerAdjustRequest
--
-- Allows a client to cancel an ongoing PowerAdjustmentRequest operation.
--
-- ObjC selector: @- cancelPowerAdjustRequestWithParams:completion:@
cancelPowerAdjustRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
cancelPowerAdjustRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "cancelPowerAdjustRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelPowerAdjustRequestWithCompletion:@
cancelPowerAdjustRequestWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
cancelPowerAdjustRequestWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "cancelPowerAdjustRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command StartTimeAdjustRequest
--
-- Allows a client to adjust the start time of a Forecast sequence that has not yet started operation (i.e. where the current Forecast StartTime is in the future).
--
-- ObjC selector: @- startTimeAdjustRequestWithParams:completion:@
startTimeAdjustRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
startTimeAdjustRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "startTimeAdjustRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command PauseRequest
--
-- Allows a client to temporarily pause an operation and reduce the ESAs energy demand.
--
-- ObjC selector: @- pauseRequestWithParams:completion:@
pauseRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPauseRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
pauseRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "pauseRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ResumeRequest
--
-- Allows a client to cancel the PauseRequest command and enable earlier resumption of operation.
--
-- ObjC selector: @- resumeRequestWithParams:completion:@
resumeRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterResumeRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
resumeRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "resumeRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeRequestWithCompletion:@
resumeRequestWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
resumeRequestWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "resumeRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command ModifyForecastRequest
--
-- Allows a client to modify a Forecast within the limits allowed by the ESA.
--
-- ObjC selector: @- modifyForecastRequestWithParams:completion:@
modifyForecastRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
modifyForecastRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "modifyForecastRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RequestConstraintBasedForecast
--
-- Allows a client to ask the ESA to recompute its Forecast based on power and time constraints.
--
-- ObjC selector: @- requestConstraintBasedForecastWithParams:completion:@
requestConstraintBasedForecastWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
requestConstraintBasedForecastWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "requestConstraintBasedForecastWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CancelRequest
--
-- Allows a client to request cancellation of a previous adjustment request in a StartTimeAdjustRequest, ModifyForecastRequest or RequestConstraintBasedForecast command.
--
-- ObjC selector: @- cancelRequestWithParams:completion:@
cancelRequestWithParams_completion :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelRequestParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> IO ()
cancelRequestWithParams_completion mtrBaseClusterDeviceEnergyManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "cancelRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- cancelRequestWithCompletion:@
cancelRequestWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
cancelRequestWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "cancelRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeESATypeWithCompletion:@
readAttributeESATypeWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeESATypeWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeESATypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeESATypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeESATypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeESACanGenerateWithCompletion:@
readAttributeESACanGenerateWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeESACanGenerateWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeESACanGenerateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeESAStateWithCompletion:@
readAttributeESAStateWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeESAStateWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeESAStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESAStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeESAStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAbsMinPowerWithCompletion:@
readAttributeAbsMinPowerWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAbsMinPowerWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeAbsMinPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAbsMaxPowerWithCompletion:@
readAttributeAbsMaxPowerWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAbsMaxPowerWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeAbsMaxPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePowerAdjustmentCapabilityWithCompletion:@
readAttributePowerAdjustmentCapabilityWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributePowerAdjustmentCapabilityWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributePowerAdjustmentCapabilityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeForecastWithCompletion:@
readAttributeForecastWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeForecastWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeForecastWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeForecastWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeForecastWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOptOutStateWithCompletion:@
readAttributeOptOutStateWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeOptOutStateWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeOptOutStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterDeviceEnergyManagement  completion =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRSubscribeParams params) => mtrBaseClusterDeviceEnergyManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterDeviceEnergyManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement => mtrBaseClusterDeviceEnergyManagement -> IO (Id MTRBaseClusterDeviceEnergyManagement)
init_ mtrBaseClusterDeviceEnergyManagement  =
    sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterDeviceEnergyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterDeviceEnergyManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterDeviceEnergyManagement mtrBaseClusterDeviceEnergyManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterDeviceEnergyManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterDeviceEnergyManagement)
initWithDevice_endpointID_queue mtrBaseClusterDeviceEnergyManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterDeviceEnergyManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerAdjustRequestWithParams:completion:@
powerAdjustRequestWithParams_completionSelector :: Selector
powerAdjustRequestWithParams_completionSelector = mkSelector "powerAdjustRequestWithParams:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithParams:completion:@
cancelPowerAdjustRequestWithParams_completionSelector :: Selector
cancelPowerAdjustRequestWithParams_completionSelector = mkSelector "cancelPowerAdjustRequestWithParams:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithCompletion:@
cancelPowerAdjustRequestWithCompletionSelector :: Selector
cancelPowerAdjustRequestWithCompletionSelector = mkSelector "cancelPowerAdjustRequestWithCompletion:"

-- | @Selector@ for @startTimeAdjustRequestWithParams:completion:@
startTimeAdjustRequestWithParams_completionSelector :: Selector
startTimeAdjustRequestWithParams_completionSelector = mkSelector "startTimeAdjustRequestWithParams:completion:"

-- | @Selector@ for @pauseRequestWithParams:completion:@
pauseRequestWithParams_completionSelector :: Selector
pauseRequestWithParams_completionSelector = mkSelector "pauseRequestWithParams:completion:"

-- | @Selector@ for @resumeRequestWithParams:completion:@
resumeRequestWithParams_completionSelector :: Selector
resumeRequestWithParams_completionSelector = mkSelector "resumeRequestWithParams:completion:"

-- | @Selector@ for @resumeRequestWithCompletion:@
resumeRequestWithCompletionSelector :: Selector
resumeRequestWithCompletionSelector = mkSelector "resumeRequestWithCompletion:"

-- | @Selector@ for @modifyForecastRequestWithParams:completion:@
modifyForecastRequestWithParams_completionSelector :: Selector
modifyForecastRequestWithParams_completionSelector = mkSelector "modifyForecastRequestWithParams:completion:"

-- | @Selector@ for @requestConstraintBasedForecastWithParams:completion:@
requestConstraintBasedForecastWithParams_completionSelector :: Selector
requestConstraintBasedForecastWithParams_completionSelector = mkSelector "requestConstraintBasedForecastWithParams:completion:"

-- | @Selector@ for @cancelRequestWithParams:completion:@
cancelRequestWithParams_completionSelector :: Selector
cancelRequestWithParams_completionSelector = mkSelector "cancelRequestWithParams:completion:"

-- | @Selector@ for @cancelRequestWithCompletion:@
cancelRequestWithCompletionSelector :: Selector
cancelRequestWithCompletionSelector = mkSelector "cancelRequestWithCompletion:"

-- | @Selector@ for @readAttributeESATypeWithCompletion:@
readAttributeESATypeWithCompletionSelector :: Selector
readAttributeESATypeWithCompletionSelector = mkSelector "readAttributeESATypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeESATypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeESATypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeESATypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeESATypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeESACanGenerateWithCompletion:@
readAttributeESACanGenerateWithCompletionSelector :: Selector
readAttributeESACanGenerateWithCompletionSelector = mkSelector "readAttributeESACanGenerateWithCompletion:"

-- | @Selector@ for @subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeESACanGenerateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeESACanGenerateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeESACanGenerateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeESACanGenerateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeESAStateWithCompletion:@
readAttributeESAStateWithCompletionSelector :: Selector
readAttributeESAStateWithCompletionSelector = mkSelector "readAttributeESAStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeESAStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeESAStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeESAStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeESAStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAbsMinPowerWithCompletion:@
readAttributeAbsMinPowerWithCompletionSelector :: Selector
readAttributeAbsMinPowerWithCompletionSelector = mkSelector "readAttributeAbsMinPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAbsMinPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAbsMinPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAbsMinPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAbsMinPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAbsMaxPowerWithCompletion:@
readAttributeAbsMaxPowerWithCompletionSelector :: Selector
readAttributeAbsMaxPowerWithCompletionSelector = mkSelector "readAttributeAbsMaxPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAbsMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAbsMaxPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAbsMaxPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAbsMaxPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerAdjustmentCapabilityWithCompletion:@
readAttributePowerAdjustmentCapabilityWithCompletionSelector :: Selector
readAttributePowerAdjustmentCapabilityWithCompletionSelector = mkSelector "readAttributePowerAdjustmentCapabilityWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePowerAdjustmentCapabilityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerAdjustmentCapabilityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePowerAdjustmentCapabilityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerAdjustmentCapabilityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeForecastWithCompletion:@
readAttributeForecastWithCompletionSelector :: Selector
readAttributeForecastWithCompletionSelector = mkSelector "readAttributeForecastWithCompletion:"

-- | @Selector@ for @subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeForecastWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeForecastWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeForecastWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeForecastWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOptOutStateWithCompletion:@
readAttributeOptOutStateWithCompletionSelector :: Selector
readAttributeOptOutStateWithCompletionSelector = mkSelector "readAttributeOptOutStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOptOutStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOptOutStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOptOutStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOptOutStateWithClusterStateCache:endpoint:queue:completion:"

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

