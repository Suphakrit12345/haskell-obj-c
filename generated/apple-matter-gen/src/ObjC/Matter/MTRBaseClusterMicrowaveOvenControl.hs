{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Microwave Oven Control
--
-- Attributes and commands for configuring the microwave oven control, and reporting cooking stats.
--
-- Generated bindings for @MTRBaseClusterMicrowaveOvenControl@.
module ObjC.Matter.MTRBaseClusterMicrowaveOvenControl
  ( MTRBaseClusterMicrowaveOvenControl
  , IsMTRBaseClusterMicrowaveOvenControl(..)
  , setCookingParametersWithParams_completion
  , setCookingParametersWithCompletion
  , addMoreTimeWithParams_completion
  , readAttributeCookTimeWithCompletion
  , subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCookTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxCookTimeWithCompletion
  , subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerSettingWithCompletion
  , subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerSettingWithClusterStateCache_endpoint_queue_completion
  , readAttributeMinPowerWithCompletion
  , subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeMinPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxPowerWithCompletion
  , subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completion
  , readAttributePowerStepWithCompletion
  , subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandler
  , readAttributePowerStepWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedWattsWithCompletion
  , subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedWattIndexWithCompletion
  , subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completion
  , readAttributeWattRatingWithCompletion
  , subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandler
  , readAttributeWattRatingWithClusterStateCache_endpoint_queue_completion
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
  , setCookingParametersWithParams_completionSelector
  , setCookingParametersWithCompletionSelector
  , addMoreTimeWithParams_completionSelector
  , readAttributeCookTimeWithCompletionSelector
  , subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxCookTimeWithCompletionSelector
  , subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerSettingWithCompletionSelector
  , subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMinPowerWithCompletionSelector
  , subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxPowerWithCompletionSelector
  , subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePowerStepWithCompletionSelector
  , subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedWattsWithCompletionSelector
  , subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedWattIndexWithCompletionSelector
  , subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWattRatingWithCompletionSelector
  , subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SetCookingParameters
--
-- This command is used to set the cooking parameters associated with the operation of the device.
--
-- ObjC selector: @- setCookingParametersWithParams:completion:@
setCookingParametersWithParams_completion :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterSetCookingParametersParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> IO ()
setCookingParametersWithParams_completion mtrBaseClusterMicrowaveOvenControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "setCookingParametersWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setCookingParametersWithCompletion:@
setCookingParametersWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
setCookingParametersWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "setCookingParametersWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command AddMoreTime
--
-- This command is used to add more time to the CookTime attribute of the server.
--
-- ObjC selector: @- addMoreTimeWithParams:completion:@
addMoreTimeWithParams_completion :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterAddMoreTimeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> IO ()
addMoreTimeWithParams_completion mtrBaseClusterMicrowaveOvenControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "addMoreTimeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCookTimeWithCompletion:@
readAttributeCookTimeWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeCookTimeWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeCookTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxCookTimeWithCompletion:@
readAttributeMaxCookTimeWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeMaxCookTimeWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeMaxCookTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePowerSettingWithCompletion:@
readAttributePowerSettingWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributePowerSettingWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributePowerSettingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMinPowerWithCompletion:@
readAttributeMinPowerWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeMinPowerWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeMinPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxPowerWithCompletion:@
readAttributeMaxPowerWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeMaxPowerWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeMaxPowerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePowerStepWithCompletion:@
readAttributePowerStepWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributePowerStepWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributePowerStepWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerStepWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePowerStepWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedWattsWithCompletion:@
readAttributeSupportedWattsWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeSupportedWattsWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeSupportedWattsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSelectedWattIndexWithCompletion:@
readAttributeSelectedWattIndexWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeSelectedWattIndexWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeSelectedWattIndexWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeWattRatingWithCompletion:@
readAttributeWattRatingWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeWattRatingWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeWattRatingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMicrowaveOvenControl  completion =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRSubscribeParams params) => mtrBaseClusterMicrowaveOvenControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMicrowaveOvenControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl => mtrBaseClusterMicrowaveOvenControl -> IO (Id MTRBaseClusterMicrowaveOvenControl)
init_ mtrBaseClusterMicrowaveOvenControl  =
    sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterMicrowaveOvenControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMicrowaveOvenControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMicrowaveOvenControl mtrBaseClusterMicrowaveOvenControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMicrowaveOvenControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMicrowaveOvenControl)
initWithDevice_endpointID_queue mtrBaseClusterMicrowaveOvenControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterMicrowaveOvenControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCookingParametersWithParams:completion:@
setCookingParametersWithParams_completionSelector :: Selector
setCookingParametersWithParams_completionSelector = mkSelector "setCookingParametersWithParams:completion:"

-- | @Selector@ for @setCookingParametersWithCompletion:@
setCookingParametersWithCompletionSelector :: Selector
setCookingParametersWithCompletionSelector = mkSelector "setCookingParametersWithCompletion:"

-- | @Selector@ for @addMoreTimeWithParams:completion:@
addMoreTimeWithParams_completionSelector :: Selector
addMoreTimeWithParams_completionSelector = mkSelector "addMoreTimeWithParams:completion:"

-- | @Selector@ for @readAttributeCookTimeWithCompletion:@
readAttributeCookTimeWithCompletionSelector :: Selector
readAttributeCookTimeWithCompletionSelector = mkSelector "readAttributeCookTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCookTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCookTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCookTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCookTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxCookTimeWithCompletion:@
readAttributeMaxCookTimeWithCompletionSelector :: Selector
readAttributeMaxCookTimeWithCompletionSelector = mkSelector "readAttributeMaxCookTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxCookTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxCookTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxCookTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxCookTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerSettingWithCompletion:@
readAttributePowerSettingWithCompletionSelector :: Selector
readAttributePowerSettingWithCompletionSelector = mkSelector "readAttributePowerSettingWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePowerSettingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerSettingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePowerSettingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerSettingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMinPowerWithCompletion:@
readAttributeMinPowerWithCompletionSelector :: Selector
readAttributeMinPowerWithCompletionSelector = mkSelector "readAttributeMinPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMinPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMinPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMinPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMinPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxPowerWithCompletion:@
readAttributeMaxPowerWithCompletionSelector :: Selector
readAttributeMaxPowerWithCompletionSelector = mkSelector "readAttributeMaxPowerWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxPowerWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxPowerWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxPowerWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxPowerWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePowerStepWithCompletion:@
readAttributePowerStepWithCompletionSelector :: Selector
readAttributePowerStepWithCompletionSelector = mkSelector "readAttributePowerStepWithCompletion:"

-- | @Selector@ for @subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePowerStepWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePowerStepWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:@
readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePowerStepWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePowerStepWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedWattsWithCompletion:@
readAttributeSupportedWattsWithCompletionSelector :: Selector
readAttributeSupportedWattsWithCompletionSelector = mkSelector "readAttributeSupportedWattsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedWattsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedWattsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedWattsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedWattsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedWattIndexWithCompletion:@
readAttributeSelectedWattIndexWithCompletionSelector :: Selector
readAttributeSelectedWattIndexWithCompletionSelector = mkSelector "readAttributeSelectedWattIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSelectedWattIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedWattIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSelectedWattIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedWattIndexWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeWattRatingWithCompletion:@
readAttributeWattRatingWithCompletionSelector :: Selector
readAttributeWattRatingWithCompletionSelector = mkSelector "readAttributeWattRatingWithCompletion:"

-- | @Selector@ for @subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWattRatingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWattRatingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:@
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeWattRatingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWattRatingWithClusterStateCache:endpoint:queue:completion:"

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

