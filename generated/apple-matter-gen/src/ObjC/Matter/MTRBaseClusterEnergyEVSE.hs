{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy EVSE
--
-- Electric Vehicle Supply Equipment (EVSE) is equipment used to charge an Electric Vehicle (EV) or Plug-In Hybrid Electric Vehicle. This cluster provides an interface to the functionality of Electric Vehicle Supply Equipment (EVSE) management.
--
-- Generated bindings for @MTRBaseClusterEnergyEVSE@.
module ObjC.Matter.MTRBaseClusterEnergyEVSE
  ( MTRBaseClusterEnergyEVSE
  , IsMTRBaseClusterEnergyEVSE(..)
  , disableWithParams_completion
  , disableWithCompletion
  , enableChargingWithParams_completion
  , enableDischargingWithParams_completion
  , startDiagnosticsWithParams_completion
  , startDiagnosticsWithCompletion
  , setTargetsWithParams_completion
  , getTargetsWithParams_completion
  , getTargetsWithCompletion
  , clearTargetsWithParams_completion
  , clearTargetsWithCompletion
  , readAttributeStateWithCompletion
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupplyStateWithCompletion
  , subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeFaultStateWithCompletion
  , subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeFaultStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeChargingEnabledUntilWithCompletion
  , subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandler
  , readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completion
  , readAttributeDischargingEnabledUntilWithCompletion
  , subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandler
  , readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completion
  , readAttributeCircuitCapacityWithCompletion
  , subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandler
  , readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinimumChargeCurrentWithCompletion
  , subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumChargeCurrentWithCompletion
  , subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaximumDischargeCurrentWithCompletion
  , subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeUserMaximumChargeCurrentWithCompletion
  , writeAttributeUserMaximumChargeCurrentWithValue_completion
  , writeAttributeUserMaximumChargeCurrentWithValue_params_completion
  , subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeRandomizationDelayWindowWithCompletion
  , writeAttributeRandomizationDelayWindowWithValue_completion
  , writeAttributeRandomizationDelayWindowWithValue_params_completion
  , subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandler
  , readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeStartTimeWithCompletion
  , subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeTargetTimeWithCompletion
  , subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeRequiredEnergyWithCompletion
  , subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextChargeTargetSoCWithCompletion
  , subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completion
  , readAttributeApproximateEVEfficiencyWithCompletion
  , writeAttributeApproximateEVEfficiencyWithValue_completion
  , writeAttributeApproximateEVEfficiencyWithValue_params_completion
  , subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeStateOfChargeWithCompletion
  , subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandler
  , readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completion
  , readAttributeBatteryCapacityWithCompletion
  , subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandler
  , readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completion
  , readAttributeVehicleIDWithCompletion
  , subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionIDWithCompletion
  , subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionDurationWithCompletion
  , subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionEnergyChargedWithCompletion
  , subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completion
  , readAttributeSessionEnergyDischargedWithCompletion
  , subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandler
  , readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completion
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
  , disableWithParams_completionSelector
  , disableWithCompletionSelector
  , enableChargingWithParams_completionSelector
  , enableDischargingWithParams_completionSelector
  , startDiagnosticsWithParams_completionSelector
  , startDiagnosticsWithCompletionSelector
  , setTargetsWithParams_completionSelector
  , getTargetsWithParams_completionSelector
  , getTargetsWithCompletionSelector
  , clearTargetsWithParams_completionSelector
  , clearTargetsWithCompletionSelector
  , readAttributeStateWithCompletionSelector
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupplyStateWithCompletionSelector
  , subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFaultStateWithCompletionSelector
  , subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeChargingEnabledUntilWithCompletionSelector
  , subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDischargingEnabledUntilWithCompletionSelector
  , subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCircuitCapacityWithCompletionSelector
  , subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinimumChargeCurrentWithCompletionSelector
  , subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumChargeCurrentWithCompletionSelector
  , subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaximumDischargeCurrentWithCompletionSelector
  , subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUserMaximumChargeCurrentWithCompletionSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_completionSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector
  , subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRandomizationDelayWindowWithCompletionSelector
  , writeAttributeRandomizationDelayWindowWithValue_completionSelector
  , writeAttributeRandomizationDelayWindowWithValue_params_completionSelector
  , subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeStartTimeWithCompletionSelector
  , subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeTargetTimeWithCompletionSelector
  , subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeRequiredEnergyWithCompletionSelector
  , subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextChargeTargetSoCWithCompletionSelector
  , subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApproximateEVEfficiencyWithCompletionSelector
  , writeAttributeApproximateEVEfficiencyWithValue_completionSelector
  , writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector
  , subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStateOfChargeWithCompletionSelector
  , subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBatteryCapacityWithCompletionSelector
  , subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVehicleIDWithCompletionSelector
  , subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionIDWithCompletionSelector
  , subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionDurationWithCompletionSelector
  , subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionEnergyChargedWithCompletionSelector
  , subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSessionEnergyDischargedWithCompletionSelector
  , subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command Disable
--
-- Allows a client to disable the EVSE from charging and discharging.
--
-- ObjC selector: @- disableWithParams:completion:@
disableWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterDisableParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
disableWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "disableWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableWithCompletion:@
disableWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
disableWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "disableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command EnableCharging
--
-- This command allows a client to enable the EVSE to charge an EV, and to provide or update the maximum and minimum charge current.
--
-- ObjC selector: @- enableChargingWithParams:completion:@
enableChargingWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableChargingParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
enableChargingWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "enableChargingWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command EnableDischarging
--
-- Upon receipt, this SHALL allow a client to enable the discharge of an EV, and to provide or update the maximum discharge current.
--
-- ObjC selector: @- enableDischargingWithParams:completion:@
enableDischargingWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableDischargingParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
enableDischargingWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "enableDischargingWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StartDiagnostics
--
-- Allows a client to put the EVSE into a self-diagnostics mode.
--
-- ObjC selector: @- startDiagnosticsWithParams:completion:@
startDiagnosticsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterStartDiagnosticsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
startDiagnosticsWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "startDiagnosticsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startDiagnosticsWithCompletion:@
startDiagnosticsWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
startDiagnosticsWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "startDiagnosticsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SetTargets
--
-- Allows a client to set the user specified charging targets.
--
-- ObjC selector: @- setTargetsWithParams:completion:@
setTargetsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterSetTargetsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
setTargetsWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "setTargetsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command GetTargets
--
-- Allows a client to retrieve the current set of charging targets.
--
-- ObjC selector: @- getTargetsWithParams:completion:@
getTargetsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterGetTargetsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
getTargetsWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "getTargetsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getTargetsWithCompletion:@
getTargetsWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
getTargetsWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "getTargetsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command ClearTargets
--
-- Allows a client to clear all stored charging targets.
--
-- ObjC selector: @- clearTargetsWithParams:completion:@
clearTargetsWithParams_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTREnergyEVSEClusterClearTargetsParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> IO ()
clearTargetsWithParams_completion mtrBaseClusterEnergyEVSE  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "clearTargetsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- clearTargetsWithCompletion:@
clearTargetsWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
clearTargetsWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "clearTargetsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStateWithCompletion:@
readAttributeStateWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeStateWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupplyStateWithCompletion:@
readAttributeSupplyStateWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSupplyStateWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeSupplyStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFaultStateWithCompletion:@
readAttributeFaultStateWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeFaultStateWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeFaultStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeChargingEnabledUntilWithCompletion:@
readAttributeChargingEnabledUntilWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeChargingEnabledUntilWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeChargingEnabledUntilWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDischargingEnabledUntilWithCompletion:@
readAttributeDischargingEnabledUntilWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeDischargingEnabledUntilWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeDischargingEnabledUntilWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCircuitCapacityWithCompletion:@
readAttributeCircuitCapacityWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeCircuitCapacityWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeCircuitCapacityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinimumChargeCurrentWithCompletion:@
readAttributeMinimumChargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeMinimumChargeCurrentWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeMinimumChargeCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaximumChargeCurrentWithCompletion:@
readAttributeMaximumChargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeMaximumChargeCurrentWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeMaximumChargeCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaximumDischargeCurrentWithCompletion:@
readAttributeMaximumDischargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeMaximumDischargeCurrentWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeMaximumDischargeCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUserMaximumChargeCurrentWithCompletion:@
readAttributeUserMaximumChargeCurrentWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeUserMaximumChargeCurrentWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeUserMaximumChargeCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value) => mtrBaseClusterEnergyEVSE -> value -> Ptr () -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_completion mtrBaseClusterEnergyEVSE  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:params:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_params_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyEVSE -> value -> params -> Ptr () -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_params_completion mtrBaseClusterEnergyEVSE  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterEnergyEVSE (mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRandomizationDelayWindowWithCompletion:@
readAttributeRandomizationDelayWindowWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeRandomizationDelayWindowWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeRandomizationDelayWindowWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeRandomizationDelayWindowWithValue:completion:@
writeAttributeRandomizationDelayWindowWithValue_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value) => mtrBaseClusterEnergyEVSE -> value -> Ptr () -> IO ()
writeAttributeRandomizationDelayWindowWithValue_completion mtrBaseClusterEnergyEVSE  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "writeAttributeRandomizationDelayWindowWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeRandomizationDelayWindowWithValue:params:completion:@
writeAttributeRandomizationDelayWindowWithValue_params_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyEVSE -> value -> params -> Ptr () -> IO ()
writeAttributeRandomizationDelayWindowWithValue_params_completion mtrBaseClusterEnergyEVSE  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterEnergyEVSE (mkSelector "writeAttributeRandomizationDelayWindowWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextChargeStartTimeWithCompletion:@
readAttributeNextChargeStartTimeWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeStartTimeWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeNextChargeStartTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextChargeTargetTimeWithCompletion:@
readAttributeNextChargeTargetTimeWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeTargetTimeWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeNextChargeTargetTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextChargeRequiredEnergyWithCompletion:@
readAttributeNextChargeRequiredEnergyWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeRequiredEnergyWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeNextChargeRequiredEnergyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNextChargeTargetSoCWithCompletion:@
readAttributeNextChargeTargetSoCWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeNextChargeTargetSoCWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeNextChargeTargetSoCWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApproximateEVEfficiencyWithCompletion:@
readAttributeApproximateEVEfficiencyWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeApproximateEVEfficiencyWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeApproximateEVEfficiencyWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeApproximateEVEfficiencyWithValue:completion:@
writeAttributeApproximateEVEfficiencyWithValue_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value) => mtrBaseClusterEnergyEVSE -> value -> Ptr () -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_completion mtrBaseClusterEnergyEVSE  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "writeAttributeApproximateEVEfficiencyWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeApproximateEVEfficiencyWithValue:params:completion:@
writeAttributeApproximateEVEfficiencyWithValue_params_completion :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyEVSE -> value -> params -> Ptr () -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_params_completion mtrBaseClusterEnergyEVSE  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterEnergyEVSE (mkSelector "writeAttributeApproximateEVEfficiencyWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStateOfChargeWithCompletion:@
readAttributeStateOfChargeWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeStateOfChargeWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeStateOfChargeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBatteryCapacityWithCompletion:@
readAttributeBatteryCapacityWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeBatteryCapacityWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeBatteryCapacityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVehicleIDWithCompletion:@
readAttributeVehicleIDWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeVehicleIDWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeVehicleIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSessionIDWithCompletion:@
readAttributeSessionIDWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionIDWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeSessionIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSessionDurationWithCompletion:@
readAttributeSessionDurationWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionDurationWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeSessionDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSessionEnergyChargedWithCompletion:@
readAttributeSessionEnergyChargedWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionEnergyChargedWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeSessionEnergyChargedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSessionEnergyDischargedWithCompletion:@
readAttributeSessionEnergyDischargedWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeSessionEnergyDischargedWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeSessionEnergyDischargedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterEnergyEVSE  completion =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRSubscribeParams params) => mtrBaseClusterEnergyEVSE -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyEVSE  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyEVSE (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE => mtrBaseClusterEnergyEVSE -> IO (Id MTRBaseClusterEnergyEVSE)
init_ mtrBaseClusterEnergyEVSE  =
    sendMsg mtrBaseClusterEnergyEVSE (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterEnergyEVSE)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyEVSE"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterEnergyEVSE mtrBaseClusterEnergyEVSE, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterEnergyEVSE -> device -> endpointID -> queue -> IO (Id MTRBaseClusterEnergyEVSE)
initWithDevice_endpointID_queue mtrBaseClusterEnergyEVSE  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterEnergyEVSE (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disableWithParams:completion:@
disableWithParams_completionSelector :: Selector
disableWithParams_completionSelector = mkSelector "disableWithParams:completion:"

-- | @Selector@ for @disableWithCompletion:@
disableWithCompletionSelector :: Selector
disableWithCompletionSelector = mkSelector "disableWithCompletion:"

-- | @Selector@ for @enableChargingWithParams:completion:@
enableChargingWithParams_completionSelector :: Selector
enableChargingWithParams_completionSelector = mkSelector "enableChargingWithParams:completion:"

-- | @Selector@ for @enableDischargingWithParams:completion:@
enableDischargingWithParams_completionSelector :: Selector
enableDischargingWithParams_completionSelector = mkSelector "enableDischargingWithParams:completion:"

-- | @Selector@ for @startDiagnosticsWithParams:completion:@
startDiagnosticsWithParams_completionSelector :: Selector
startDiagnosticsWithParams_completionSelector = mkSelector "startDiagnosticsWithParams:completion:"

-- | @Selector@ for @startDiagnosticsWithCompletion:@
startDiagnosticsWithCompletionSelector :: Selector
startDiagnosticsWithCompletionSelector = mkSelector "startDiagnosticsWithCompletion:"

-- | @Selector@ for @setTargetsWithParams:completion:@
setTargetsWithParams_completionSelector :: Selector
setTargetsWithParams_completionSelector = mkSelector "setTargetsWithParams:completion:"

-- | @Selector@ for @getTargetsWithParams:completion:@
getTargetsWithParams_completionSelector :: Selector
getTargetsWithParams_completionSelector = mkSelector "getTargetsWithParams:completion:"

-- | @Selector@ for @getTargetsWithCompletion:@
getTargetsWithCompletionSelector :: Selector
getTargetsWithCompletionSelector = mkSelector "getTargetsWithCompletion:"

-- | @Selector@ for @clearTargetsWithParams:completion:@
clearTargetsWithParams_completionSelector :: Selector
clearTargetsWithParams_completionSelector = mkSelector "clearTargetsWithParams:completion:"

-- | @Selector@ for @clearTargetsWithCompletion:@
clearTargetsWithCompletionSelector :: Selector
clearTargetsWithCompletionSelector = mkSelector "clearTargetsWithCompletion:"

-- | @Selector@ for @readAttributeStateWithCompletion:@
readAttributeStateWithCompletionSelector :: Selector
readAttributeStateWithCompletionSelector = mkSelector "readAttributeStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupplyStateWithCompletion:@
readAttributeSupplyStateWithCompletionSelector :: Selector
readAttributeSupplyStateWithCompletionSelector = mkSelector "readAttributeSupplyStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupplyStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupplyStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupplyStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupplyStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFaultStateWithCompletion:@
readAttributeFaultStateWithCompletionSelector :: Selector
readAttributeFaultStateWithCompletionSelector = mkSelector "readAttributeFaultStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFaultStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFaultStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFaultStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFaultStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeChargingEnabledUntilWithCompletion:@
readAttributeChargingEnabledUntilWithCompletionSelector :: Selector
readAttributeChargingEnabledUntilWithCompletionSelector = mkSelector "readAttributeChargingEnabledUntilWithCompletion:"

-- | @Selector@ for @subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeChargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeChargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDischargingEnabledUntilWithCompletion:@
readAttributeDischargingEnabledUntilWithCompletionSelector :: Selector
readAttributeDischargingEnabledUntilWithCompletionSelector = mkSelector "readAttributeDischargingEnabledUntilWithCompletion:"

-- | @Selector@ for @subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDischargingEnabledUntilWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDischargingEnabledUntilWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:@
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDischargingEnabledUntilWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDischargingEnabledUntilWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCircuitCapacityWithCompletion:@
readAttributeCircuitCapacityWithCompletionSelector :: Selector
readAttributeCircuitCapacityWithCompletionSelector = mkSelector "readAttributeCircuitCapacityWithCompletion:"

-- | @Selector@ for @subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCircuitCapacityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCircuitCapacityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCircuitCapacityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCircuitCapacityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinimumChargeCurrentWithCompletion:@
readAttributeMinimumChargeCurrentWithCompletionSelector :: Selector
readAttributeMinimumChargeCurrentWithCompletionSelector = mkSelector "readAttributeMinimumChargeCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinimumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinimumChargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinimumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinimumChargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumChargeCurrentWithCompletion:@
readAttributeMaximumChargeCurrentWithCompletionSelector :: Selector
readAttributeMaximumChargeCurrentWithCompletionSelector = mkSelector "readAttributeMaximumChargeCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaximumDischargeCurrentWithCompletion:@
readAttributeMaximumDischargeCurrentWithCompletionSelector :: Selector
readAttributeMaximumDischargeCurrentWithCompletionSelector = mkSelector "readAttributeMaximumDischargeCurrentWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaximumDischargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaximumDischargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaximumDischargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaximumDischargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUserMaximumChargeCurrentWithCompletion:@
readAttributeUserMaximumChargeCurrentWithCompletionSelector :: Selector
readAttributeUserMaximumChargeCurrentWithCompletionSelector = mkSelector "readAttributeUserMaximumChargeCurrentWithCompletion:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_completionSelector :: Selector
writeAttributeUserMaximumChargeCurrentWithValue_completionSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:completion:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:params:completion:@
writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector :: Selector
writeAttributeUserMaximumChargeCurrentWithValue_params_completionSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUserMaximumChargeCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUserMaximumChargeCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUserMaximumChargeCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUserMaximumChargeCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRandomizationDelayWindowWithCompletion:@
readAttributeRandomizationDelayWindowWithCompletionSelector :: Selector
readAttributeRandomizationDelayWindowWithCompletionSelector = mkSelector "readAttributeRandomizationDelayWindowWithCompletion:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:completion:@
writeAttributeRandomizationDelayWindowWithValue_completionSelector :: Selector
writeAttributeRandomizationDelayWindowWithValue_completionSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:completion:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:params:completion:@
writeAttributeRandomizationDelayWindowWithValue_params_completionSelector :: Selector
writeAttributeRandomizationDelayWindowWithValue_params_completionSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRandomizationDelayWindowWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRandomizationDelayWindowWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:@
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRandomizationDelayWindowWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRandomizationDelayWindowWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeStartTimeWithCompletion:@
readAttributeNextChargeStartTimeWithCompletionSelector :: Selector
readAttributeNextChargeStartTimeWithCompletionSelector = mkSelector "readAttributeNextChargeStartTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextChargeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeStartTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextChargeStartTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeStartTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeTargetTimeWithCompletion:@
readAttributeNextChargeTargetTimeWithCompletionSelector :: Selector
readAttributeNextChargeTargetTimeWithCompletionSelector = mkSelector "readAttributeNextChargeTargetTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextChargeTargetTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeTargetTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextChargeTargetTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeTargetTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeRequiredEnergyWithCompletion:@
readAttributeNextChargeRequiredEnergyWithCompletionSelector :: Selector
readAttributeNextChargeRequiredEnergyWithCompletionSelector = mkSelector "readAttributeNextChargeRequiredEnergyWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextChargeRequiredEnergyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeRequiredEnergyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextChargeRequiredEnergyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeRequiredEnergyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextChargeTargetSoCWithCompletion:@
readAttributeNextChargeTargetSoCWithCompletionSelector :: Selector
readAttributeNextChargeTargetSoCWithCompletionSelector = mkSelector "readAttributeNextChargeTargetSoCWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNextChargeTargetSoCWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextChargeTargetSoCWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNextChargeTargetSoCWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextChargeTargetSoCWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApproximateEVEfficiencyWithCompletion:@
readAttributeApproximateEVEfficiencyWithCompletionSelector :: Selector
readAttributeApproximateEVEfficiencyWithCompletionSelector = mkSelector "readAttributeApproximateEVEfficiencyWithCompletion:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:completion:@
writeAttributeApproximateEVEfficiencyWithValue_completionSelector :: Selector
writeAttributeApproximateEVEfficiencyWithValue_completionSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:completion:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:params:completion:@
writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector :: Selector
writeAttributeApproximateEVEfficiencyWithValue_params_completionSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApproximateEVEfficiencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApproximateEVEfficiencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApproximateEVEfficiencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApproximateEVEfficiencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStateOfChargeWithCompletion:@
readAttributeStateOfChargeWithCompletionSelector :: Selector
readAttributeStateOfChargeWithCompletionSelector = mkSelector "readAttributeStateOfChargeWithCompletion:"

-- | @Selector@ for @subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStateOfChargeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStateOfChargeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStateOfChargeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStateOfChargeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBatteryCapacityWithCompletion:@
readAttributeBatteryCapacityWithCompletionSelector :: Selector
readAttributeBatteryCapacityWithCompletionSelector = mkSelector "readAttributeBatteryCapacityWithCompletion:"

-- | @Selector@ for @subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBatteryCapacityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBatteryCapacityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:@
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBatteryCapacityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBatteryCapacityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVehicleIDWithCompletion:@
readAttributeVehicleIDWithCompletionSelector :: Selector
readAttributeVehicleIDWithCompletionSelector = mkSelector "readAttributeVehicleIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVehicleIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVehicleIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeVehicleIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVehicleIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionIDWithCompletion:@
readAttributeSessionIDWithCompletionSelector :: Selector
readAttributeSessionIDWithCompletionSelector = mkSelector "readAttributeSessionIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSessionIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSessionIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionDurationWithCompletion:@
readAttributeSessionDurationWithCompletionSelector :: Selector
readAttributeSessionDurationWithCompletionSelector = mkSelector "readAttributeSessionDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSessionDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSessionDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionEnergyChargedWithCompletion:@
readAttributeSessionEnergyChargedWithCompletionSelector :: Selector
readAttributeSessionEnergyChargedWithCompletionSelector = mkSelector "readAttributeSessionEnergyChargedWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSessionEnergyChargedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionEnergyChargedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSessionEnergyChargedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionEnergyChargedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSessionEnergyDischargedWithCompletion:@
readAttributeSessionEnergyDischargedWithCompletionSelector :: Selector
readAttributeSessionEnergyDischargedWithCompletionSelector = mkSelector "readAttributeSessionEnergyDischargedWithCompletion:"

-- | @Selector@ for @subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSessionEnergyDischargedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSessionEnergyDischargedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSessionEnergyDischargedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSessionEnergyDischargedWithClusterStateCache:endpoint:queue:completion:"

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

