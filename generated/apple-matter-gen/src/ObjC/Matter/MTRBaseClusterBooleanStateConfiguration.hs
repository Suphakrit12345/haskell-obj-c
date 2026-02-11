{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Boolean State Configuration
--
-- This cluster is used to configure a boolean sensor.
--
-- Generated bindings for @MTRBaseClusterBooleanStateConfiguration@.
module ObjC.Matter.MTRBaseClusterBooleanStateConfiguration
  ( MTRBaseClusterBooleanStateConfiguration
  , IsMTRBaseClusterBooleanStateConfiguration(..)
  , suppressAlarmWithParams_completion
  , enableDisableAlarmWithParams_completion
  , readAttributeCurrentSensitivityLevelWithCompletion
  , writeAttributeCurrentSensitivityLevelWithValue_completion
  , writeAttributeCurrentSensitivityLevelWithValue_params_completion
  , subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedSensitivityLevelsWithCompletion
  , subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultSensitivityLevelWithCompletion
  , subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsActiveWithCompletion
  , subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsSuppressedWithCompletion
  , subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsEnabledWithCompletion
  , subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeAlarmsSupportedWithCompletion
  , subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completion
  , readAttributeSensorFaultWithCompletion
  , subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandler
  , readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completion
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
  , suppressAlarmWithParams_completionSelector
  , enableDisableAlarmWithParams_completionSelector
  , readAttributeCurrentSensitivityLevelWithCompletionSelector
  , writeAttributeCurrentSensitivityLevelWithValue_completionSelector
  , writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector
  , subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedSensitivityLevelsWithCompletionSelector
  , subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultSensitivityLevelWithCompletionSelector
  , subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsActiveWithCompletionSelector
  , subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsSuppressedWithCompletionSelector
  , subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsEnabledWithCompletionSelector
  , subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAlarmsSupportedWithCompletionSelector
  , subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSensorFaultWithCompletionSelector
  , subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SuppressAlarm
--
-- This command is used to suppress the specified alarm mode.
--
-- ObjC selector: @- suppressAlarmWithParams:completion:@
suppressAlarmWithParams_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterSuppressAlarmParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> IO ()
suppressAlarmWithParams_completion mtrBaseClusterBooleanStateConfiguration  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "suppressAlarmWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command EnableDisableAlarm
--
-- This command is used to enable or disable the specified alarm mode.
--
-- ObjC selector: @- enableDisableAlarmWithParams:completion:@
enableDisableAlarmWithParams_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterEnableDisableAlarmParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> IO ()
enableDisableAlarmWithParams_completion mtrBaseClusterBooleanStateConfiguration  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "enableDisableAlarmWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentSensitivityLevelWithCompletion:@
readAttributeCurrentSensitivityLevelWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeCurrentSensitivityLevelWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeCurrentSensitivityLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentSensitivityLevelWithValue:completion:@
writeAttributeCurrentSensitivityLevelWithValue_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsNSNumber value) => mtrBaseClusterBooleanStateConfiguration -> value -> Ptr () -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_completion mtrBaseClusterBooleanStateConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "writeAttributeCurrentSensitivityLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentSensitivityLevelWithValue:params:completion:@
writeAttributeCurrentSensitivityLevelWithValue_params_completion :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBooleanStateConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_params_completion mtrBaseClusterBooleanStateConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "writeAttributeCurrentSensitivityLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedSensitivityLevelsWithCompletion:@
readAttributeSupportedSensitivityLevelsWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeSupportedSensitivityLevelsWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeSupportedSensitivityLevelsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultSensitivityLevelWithCompletion:@
readAttributeDefaultSensitivityLevelWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeDefaultSensitivityLevelWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeDefaultSensitivityLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAlarmsActiveWithCompletion:@
readAttributeAlarmsActiveWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsActiveWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsActiveWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAlarmsSuppressedWithCompletion:@
readAttributeAlarmsSuppressedWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsSuppressedWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsSuppressedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAlarmsEnabledWithCompletion:@
readAttributeAlarmsEnabledWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsEnabledWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsEnabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAlarmsSupportedWithCompletion:@
readAttributeAlarmsSupportedWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAlarmsSupportedWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeAlarmsSupportedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSensorFaultWithCompletion:@
readAttributeSensorFaultWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeSensorFaultWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeSensorFaultWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBooleanStateConfiguration  completion =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterBooleanStateConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBooleanStateConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration => mtrBaseClusterBooleanStateConfiguration -> IO (Id MTRBaseClusterBooleanStateConfiguration)
init_ mtrBaseClusterBooleanStateConfiguration  =
    sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterBooleanStateConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBooleanStateConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBooleanStateConfiguration mtrBaseClusterBooleanStateConfiguration, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBooleanStateConfiguration -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBooleanStateConfiguration)
initWithDevice_endpointID_queue mtrBaseClusterBooleanStateConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterBooleanStateConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @suppressAlarmWithParams:completion:@
suppressAlarmWithParams_completionSelector :: Selector
suppressAlarmWithParams_completionSelector = mkSelector "suppressAlarmWithParams:completion:"

-- | @Selector@ for @enableDisableAlarmWithParams:completion:@
enableDisableAlarmWithParams_completionSelector :: Selector
enableDisableAlarmWithParams_completionSelector = mkSelector "enableDisableAlarmWithParams:completion:"

-- | @Selector@ for @readAttributeCurrentSensitivityLevelWithCompletion:@
readAttributeCurrentSensitivityLevelWithCompletionSelector :: Selector
readAttributeCurrentSensitivityLevelWithCompletionSelector = mkSelector "readAttributeCurrentSensitivityLevelWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:completion:@
writeAttributeCurrentSensitivityLevelWithValue_completionSelector :: Selector
writeAttributeCurrentSensitivityLevelWithValue_completionSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:params:completion:@
writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector :: Selector
writeAttributeCurrentSensitivityLevelWithValue_params_completionSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentSensitivityLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentSensitivityLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedSensitivityLevelsWithCompletion:@
readAttributeSupportedSensitivityLevelsWithCompletionSelector :: Selector
readAttributeSupportedSensitivityLevelsWithCompletionSelector = mkSelector "readAttributeSupportedSensitivityLevelsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedSensitivityLevelsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedSensitivityLevelsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedSensitivityLevelsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedSensitivityLevelsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultSensitivityLevelWithCompletion:@
readAttributeDefaultSensitivityLevelWithCompletionSelector :: Selector
readAttributeDefaultSensitivityLevelWithCompletionSelector = mkSelector "readAttributeDefaultSensitivityLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultSensitivityLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultSensitivityLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultSensitivityLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultSensitivityLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsActiveWithCompletion:@
readAttributeAlarmsActiveWithCompletionSelector :: Selector
readAttributeAlarmsActiveWithCompletionSelector = mkSelector "readAttributeAlarmsActiveWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAlarmsActiveWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsActiveWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAlarmsActiveWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsActiveWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsSuppressedWithCompletion:@
readAttributeAlarmsSuppressedWithCompletionSelector :: Selector
readAttributeAlarmsSuppressedWithCompletionSelector = mkSelector "readAttributeAlarmsSuppressedWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAlarmsSuppressedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsSuppressedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAlarmsSuppressedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsSuppressedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsEnabledWithCompletion:@
readAttributeAlarmsEnabledWithCompletionSelector :: Selector
readAttributeAlarmsEnabledWithCompletionSelector = mkSelector "readAttributeAlarmsEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAlarmsEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAlarmsEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAlarmsSupportedWithCompletion:@
readAttributeAlarmsSupportedWithCompletionSelector :: Selector
readAttributeAlarmsSupportedWithCompletionSelector = mkSelector "readAttributeAlarmsSupportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAlarmsSupportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAlarmsSupportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAlarmsSupportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAlarmsSupportedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSensorFaultWithCompletion:@
readAttributeSensorFaultWithCompletionSelector :: Selector
readAttributeSensorFaultWithCompletionSelector = mkSelector "readAttributeSensorFaultWithCompletion:"

-- | @Selector@ for @subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSensorFaultWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSensorFaultWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSensorFaultWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSensorFaultWithClusterStateCache:endpoint:queue:completion:"

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

