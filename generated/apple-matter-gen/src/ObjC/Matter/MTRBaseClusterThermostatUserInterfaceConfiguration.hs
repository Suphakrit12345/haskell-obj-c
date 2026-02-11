{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thermostat User Interface Configuration
--
-- An interface for configuring the user interface of a thermostat (which may be remote from the thermostat).
--
-- Generated bindings for @MTRBaseClusterThermostatUserInterfaceConfiguration@.
module ObjC.Matter.MTRBaseClusterThermostatUserInterfaceConfiguration
  ( MTRBaseClusterThermostatUserInterfaceConfiguration
  , IsMTRBaseClusterThermostatUserInterfaceConfiguration(..)
  , readAttributeTemperatureDisplayModeWithCompletion
  , writeAttributeTemperatureDisplayModeWithValue_completion
  , writeAttributeTemperatureDisplayModeWithValue_params_completion
  , subscribeAttributeTemperatureDisplayModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeTemperatureDisplayModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeKeypadLockoutWithCompletion
  , writeAttributeKeypadLockoutWithValue_completion
  , writeAttributeKeypadLockoutWithValue_params_completion
  , subscribeAttributeKeypadLockoutWithParams_subscriptionEstablished_reportHandler
  , readAttributeKeypadLockoutWithClusterStateCache_endpoint_queue_completion
  , readAttributeScheduleProgrammingVisibilityWithCompletion
  , writeAttributeScheduleProgrammingVisibilityWithValue_completion
  , writeAttributeScheduleProgrammingVisibilityWithValue_params_completion
  , subscribeAttributeScheduleProgrammingVisibilityWithParams_subscriptionEstablished_reportHandler
  , readAttributeScheduleProgrammingVisibilityWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpoint_queue
  , readAttributeTemperatureDisplayModeWithCompletionHandler
  , writeAttributeTemperatureDisplayModeWithValue_completionHandler
  , writeAttributeTemperatureDisplayModeWithValue_params_completionHandler
  , subscribeAttributeTemperatureDisplayModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTemperatureDisplayModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeKeypadLockoutWithCompletionHandler
  , writeAttributeKeypadLockoutWithValue_completionHandler
  , writeAttributeKeypadLockoutWithValue_params_completionHandler
  , subscribeAttributeKeypadLockoutWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeKeypadLockoutWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeScheduleProgrammingVisibilityWithCompletionHandler
  , writeAttributeScheduleProgrammingVisibilityWithValue_completionHandler
  , writeAttributeScheduleProgrammingVisibilityWithValue_params_completionHandler
  , subscribeAttributeScheduleProgrammingVisibilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeScheduleProgrammingVisibilityWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , readAttributeTemperatureDisplayModeWithCompletionSelector
  , writeAttributeTemperatureDisplayModeWithValue_completionSelector
  , writeAttributeTemperatureDisplayModeWithValue_params_completionSelector
  , subscribeAttributeTemperatureDisplayModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTemperatureDisplayModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeKeypadLockoutWithCompletionSelector
  , writeAttributeKeypadLockoutWithValue_completionSelector
  , writeAttributeKeypadLockoutWithValue_params_completionSelector
  , subscribeAttributeKeypadLockoutWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeKeypadLockoutWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScheduleProgrammingVisibilityWithCompletionSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_completionSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_params_completionSelector
  , subscribeAttributeScheduleProgrammingVisibilityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScheduleProgrammingVisibilityWithClusterStateCache_endpoint_queue_completionSelector
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
  , initWithDevice_endpoint_queueSelector
  , readAttributeTemperatureDisplayModeWithCompletionHandlerSelector
  , writeAttributeTemperatureDisplayModeWithValue_completionHandlerSelector
  , writeAttributeTemperatureDisplayModeWithValue_params_completionHandlerSelector
  , subscribeAttributeTemperatureDisplayModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeTemperatureDisplayModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeKeypadLockoutWithCompletionHandlerSelector
  , writeAttributeKeypadLockoutWithValue_completionHandlerSelector
  , writeAttributeKeypadLockoutWithValue_params_completionHandlerSelector
  , subscribeAttributeKeypadLockoutWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeKeypadLockoutWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeScheduleProgrammingVisibilityWithCompletionHandlerSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_completionHandlerSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_params_completionHandlerSelector
  , subscribeAttributeScheduleProgrammingVisibilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeScheduleProgrammingVisibilityWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributeTemperatureDisplayModeWithCompletion:@
readAttributeTemperatureDisplayModeWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeTemperatureDisplayModeWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeTemperatureDisplayModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeTemperatureDisplayModeWithValue:completion:@
writeAttributeTemperatureDisplayModeWithValue_completion :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> Ptr () -> IO ()
writeAttributeTemperatureDisplayModeWithValue_completion mtrBaseClusterThermostatUserInterfaceConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeTemperatureDisplayModeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeTemperatureDisplayModeWithValue:params:completion:@
writeAttributeTemperatureDisplayModeWithValue_params_completion :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeTemperatureDisplayModeWithValue_params_completion mtrBaseClusterThermostatUserInterfaceConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeTemperatureDisplayModeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTemperatureDisplayModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureDisplayModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTemperatureDisplayModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeTemperatureDisplayModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTemperatureDisplayModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTemperatureDisplayModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTemperatureDisplayModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTemperatureDisplayModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeKeypadLockoutWithCompletion:@
readAttributeKeypadLockoutWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeKeypadLockoutWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeKeypadLockoutWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeKeypadLockoutWithValue:completion:@
writeAttributeKeypadLockoutWithValue_completion :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> Ptr () -> IO ()
writeAttributeKeypadLockoutWithValue_completion mtrBaseClusterThermostatUserInterfaceConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeKeypadLockoutWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeKeypadLockoutWithValue:params:completion:@
writeAttributeKeypadLockoutWithValue_params_completion :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeKeypadLockoutWithValue_params_completion mtrBaseClusterThermostatUserInterfaceConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeKeypadLockoutWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeKeypadLockoutWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeKeypadLockoutWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeKeypadLockoutWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeKeypadLockoutWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeKeypadLockoutWithClusterStateCache:endpoint:queue:completion:@
readAttributeKeypadLockoutWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeKeypadLockoutWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeKeypadLockoutWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScheduleProgrammingVisibilityWithCompletion:@
readAttributeScheduleProgrammingVisibilityWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeScheduleProgrammingVisibilityWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeScheduleProgrammingVisibilityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:completion:@
writeAttributeScheduleProgrammingVisibilityWithValue_completion :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> Ptr () -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_completion mtrBaseClusterThermostatUserInterfaceConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:params:completion:@
writeAttributeScheduleProgrammingVisibilityWithValue_params_completion :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_params_completion mtrBaseClusterThermostatUserInterfaceConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScheduleProgrammingVisibilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduleProgrammingVisibilityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScheduleProgrammingVisibilityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeScheduleProgrammingVisibilityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScheduleProgrammingVisibilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduleProgrammingVisibilityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScheduleProgrammingVisibilityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScheduleProgrammingVisibilityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterThermostatUserInterfaceConfiguration  completion =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> IO (Id MTRBaseClusterThermostatUserInterfaceConfiguration)
init_ mtrBaseClusterThermostatUserInterfaceConfiguration  =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterThermostatUserInterfaceConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterThermostatUserInterfaceConfiguration -> device -> CUShort -> queue -> IO (Id MTRBaseClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpoint_queue mtrBaseClusterThermostatUserInterfaceConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeTemperatureDisplayModeWithCompletionHandler:@
readAttributeTemperatureDisplayModeWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeTemperatureDisplayModeWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeTemperatureDisplayModeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeTemperatureDisplayModeWithValue:completionHandler:@
writeAttributeTemperatureDisplayModeWithValue_completionHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> Ptr () -> IO ()
writeAttributeTemperatureDisplayModeWithValue_completionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeTemperatureDisplayModeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeTemperatureDisplayModeWithValue:params:completionHandler:@
writeAttributeTemperatureDisplayModeWithValue_params_completionHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeTemperatureDisplayModeWithValue_params_completionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeTemperatureDisplayModeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeTemperatureDisplayModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureDisplayModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTemperatureDisplayModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeTemperatureDisplayModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTemperatureDisplayModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTemperatureDisplayModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTemperatureDisplayModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTemperatureDisplayModeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeKeypadLockoutWithCompletionHandler:@
readAttributeKeypadLockoutWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeKeypadLockoutWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeKeypadLockoutWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeKeypadLockoutWithValue:completionHandler:@
writeAttributeKeypadLockoutWithValue_completionHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> Ptr () -> IO ()
writeAttributeKeypadLockoutWithValue_completionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeKeypadLockoutWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeKeypadLockoutWithValue:params:completionHandler:@
writeAttributeKeypadLockoutWithValue_params_completionHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeKeypadLockoutWithValue_params_completionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeKeypadLockoutWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeKeypadLockoutWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeKeypadLockoutWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeKeypadLockoutWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeKeypadLockoutWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeKeypadLockoutWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeKeypadLockoutWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeKeypadLockoutWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeKeypadLockoutWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeScheduleProgrammingVisibilityWithCompletionHandler:@
readAttributeScheduleProgrammingVisibilityWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeScheduleProgrammingVisibilityWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeScheduleProgrammingVisibilityWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:completionHandler:@
writeAttributeScheduleProgrammingVisibilityWithValue_completionHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> Ptr () -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_completionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:params:completionHandler:@
writeAttributeScheduleProgrammingVisibilityWithValue_params_completionHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_params_completionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeScheduleProgrammingVisibilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduleProgrammingVisibilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScheduleProgrammingVisibilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeScheduleProgrammingVisibilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScheduleProgrammingVisibilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScheduleProgrammingVisibilityWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScheduleProgrammingVisibilityWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScheduleProgrammingVisibilityWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration => mtrBaseClusterThermostatUserInterfaceConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterThermostatUserInterfaceConfiguration  completionHandler =
    sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterThermostatUserInterfaceConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterThermostatUserInterfaceConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterThermostatUserInterfaceConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterThermostatUserInterfaceConfiguration mtrBaseClusterThermostatUserInterfaceConfiguration, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterThermostatUserInterfaceConfiguration -> device -> endpointID -> queue -> IO (Id MTRBaseClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpointID_queue mtrBaseClusterThermostatUserInterfaceConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterThermostatUserInterfaceConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeTemperatureDisplayModeWithCompletion:@
readAttributeTemperatureDisplayModeWithCompletionSelector :: Selector
readAttributeTemperatureDisplayModeWithCompletionSelector = mkSelector "readAttributeTemperatureDisplayModeWithCompletion:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:completion:@
writeAttributeTemperatureDisplayModeWithValue_completionSelector :: Selector
writeAttributeTemperatureDisplayModeWithValue_completionSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:completion:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:params:completion:@
writeAttributeTemperatureDisplayModeWithValue_params_completionSelector :: Selector
writeAttributeTemperatureDisplayModeWithValue_params_completionSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeTemperatureDisplayModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureDisplayModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTemperatureDisplayModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTemperatureDisplayModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTemperatureDisplayModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTemperatureDisplayModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTemperatureDisplayModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTemperatureDisplayModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeKeypadLockoutWithCompletion:@
readAttributeKeypadLockoutWithCompletionSelector :: Selector
readAttributeKeypadLockoutWithCompletionSelector = mkSelector "readAttributeKeypadLockoutWithCompletion:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:completion:@
writeAttributeKeypadLockoutWithValue_completionSelector :: Selector
writeAttributeKeypadLockoutWithValue_completionSelector = mkSelector "writeAttributeKeypadLockoutWithValue:completion:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:params:completion:@
writeAttributeKeypadLockoutWithValue_params_completionSelector :: Selector
writeAttributeKeypadLockoutWithValue_params_completionSelector = mkSelector "writeAttributeKeypadLockoutWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeKeypadLockoutWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeKeypadLockoutWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeKeypadLockoutWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeKeypadLockoutWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeKeypadLockoutWithClusterStateCache:endpoint:queue:completion:@
readAttributeKeypadLockoutWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeKeypadLockoutWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeKeypadLockoutWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScheduleProgrammingVisibilityWithCompletion:@
readAttributeScheduleProgrammingVisibilityWithCompletionSelector :: Selector
readAttributeScheduleProgrammingVisibilityWithCompletionSelector = mkSelector "readAttributeScheduleProgrammingVisibilityWithCompletion:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:completion:@
writeAttributeScheduleProgrammingVisibilityWithValue_completionSelector :: Selector
writeAttributeScheduleProgrammingVisibilityWithValue_completionSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:completion:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:params:completion:@
writeAttributeScheduleProgrammingVisibilityWithValue_params_completionSelector :: Selector
writeAttributeScheduleProgrammingVisibilityWithValue_params_completionSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeScheduleProgrammingVisibilityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduleProgrammingVisibilityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScheduleProgrammingVisibilityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScheduleProgrammingVisibilityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScheduleProgrammingVisibilityWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduleProgrammingVisibilityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScheduleProgrammingVisibilityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScheduleProgrammingVisibilityWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeTemperatureDisplayModeWithCompletionHandler:@
readAttributeTemperatureDisplayModeWithCompletionHandlerSelector :: Selector
readAttributeTemperatureDisplayModeWithCompletionHandlerSelector = mkSelector "readAttributeTemperatureDisplayModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:completionHandler:@
writeAttributeTemperatureDisplayModeWithValue_completionHandlerSelector :: Selector
writeAttributeTemperatureDisplayModeWithValue_completionHandlerSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:params:completionHandler:@
writeAttributeTemperatureDisplayModeWithValue_params_completionHandlerSelector :: Selector
writeAttributeTemperatureDisplayModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeTemperatureDisplayModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTemperatureDisplayModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTemperatureDisplayModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTemperatureDisplayModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTemperatureDisplayModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTemperatureDisplayModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeTemperatureDisplayModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTemperatureDisplayModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeKeypadLockoutWithCompletionHandler:@
readAttributeKeypadLockoutWithCompletionHandlerSelector :: Selector
readAttributeKeypadLockoutWithCompletionHandlerSelector = mkSelector "readAttributeKeypadLockoutWithCompletionHandler:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:completionHandler:@
writeAttributeKeypadLockoutWithValue_completionHandlerSelector :: Selector
writeAttributeKeypadLockoutWithValue_completionHandlerSelector = mkSelector "writeAttributeKeypadLockoutWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:params:completionHandler:@
writeAttributeKeypadLockoutWithValue_params_completionHandlerSelector :: Selector
writeAttributeKeypadLockoutWithValue_params_completionHandlerSelector = mkSelector "writeAttributeKeypadLockoutWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeKeypadLockoutWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeKeypadLockoutWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeKeypadLockoutWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeKeypadLockoutWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeKeypadLockoutWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeKeypadLockoutWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeKeypadLockoutWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeKeypadLockoutWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeScheduleProgrammingVisibilityWithCompletionHandler:@
readAttributeScheduleProgrammingVisibilityWithCompletionHandlerSelector :: Selector
readAttributeScheduleProgrammingVisibilityWithCompletionHandlerSelector = mkSelector "readAttributeScheduleProgrammingVisibilityWithCompletionHandler:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:completionHandler:@
writeAttributeScheduleProgrammingVisibilityWithValue_completionHandlerSelector :: Selector
writeAttributeScheduleProgrammingVisibilityWithValue_completionHandlerSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:params:completionHandler:@
writeAttributeScheduleProgrammingVisibilityWithValue_params_completionHandlerSelector :: Selector
writeAttributeScheduleProgrammingVisibilityWithValue_params_completionHandlerSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeScheduleProgrammingVisibilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduleProgrammingVisibilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScheduleProgrammingVisibilityWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScheduleProgrammingVisibilityWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScheduleProgrammingVisibilityWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeScheduleProgrammingVisibilityWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeScheduleProgrammingVisibilityWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeScheduleProgrammingVisibilityWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

