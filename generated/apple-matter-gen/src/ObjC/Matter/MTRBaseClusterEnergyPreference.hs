{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy Preference
--
-- This cluster provides an interface to specify preferences for how devices should consume energy.
--
-- Generated bindings for @MTRBaseClusterEnergyPreference@.
module ObjC.Matter.MTRBaseClusterEnergyPreference
  ( MTRBaseClusterEnergyPreference
  , IsMTRBaseClusterEnergyPreference(..)
  , readAttributeEnergyBalancesWithCompletion
  , subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentEnergyBalanceWithCompletion
  , writeAttributeCurrentEnergyBalanceWithValue_completion
  , writeAttributeCurrentEnergyBalanceWithValue_params_completion
  , subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completion
  , readAttributeEnergyPrioritiesWithCompletion
  , subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completion
  , readAttributeLowPowerModeSensitivitiesWithCompletion
  , subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandler
  , readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentLowPowerModeSensitivityWithCompletion
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_completion
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completion
  , subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeEnergyBalancesWithCompletionSelector
  , subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentEnergyBalanceWithCompletionSelector
  , writeAttributeCurrentEnergyBalanceWithValue_completionSelector
  , writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector
  , subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEnergyPrioritiesWithCompletionSelector
  , subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLowPowerModeSensitivitiesWithCompletionSelector
  , subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector
  , subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeEnergyBalancesWithCompletion:@
readAttributeEnergyBalancesWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeEnergyBalancesWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeEnergyBalancesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentEnergyBalanceWithCompletion:@
readAttributeCurrentEnergyBalanceWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeCurrentEnergyBalanceWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeCurrentEnergyBalanceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentEnergyBalanceWithValue:completion:@
writeAttributeCurrentEnergyBalanceWithValue_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value) => mtrBaseClusterEnergyPreference -> value -> Ptr () -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_completion mtrBaseClusterEnergyPreference  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "writeAttributeCurrentEnergyBalanceWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentEnergyBalanceWithValue:params:completion:@
writeAttributeCurrentEnergyBalanceWithValue_params_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyPreference -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_params_completion mtrBaseClusterEnergyPreference  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterEnergyPreference (mkSelector "writeAttributeCurrentEnergyBalanceWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEnergyPrioritiesWithCompletion:@
readAttributeEnergyPrioritiesWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeEnergyPrioritiesWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeEnergyPrioritiesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLowPowerModeSensitivitiesWithCompletion:@
readAttributeLowPowerModeSensitivitiesWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeLowPowerModeSensitivitiesWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeLowPowerModeSensitivitiesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentLowPowerModeSensitivityWithCompletion:@
readAttributeCurrentLowPowerModeSensitivityWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeCurrentLowPowerModeSensitivityWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeCurrentLowPowerModeSensitivityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value) => mtrBaseClusterEnergyPreference -> value -> Ptr () -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_completion mtrBaseClusterEnergyPreference  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completion :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterEnergyPreference -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completion mtrBaseClusterEnergyPreference  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterEnergyPreference (mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterEnergyPreference  completion =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRSubscribeParams params) => mtrBaseClusterEnergyPreference -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterEnergyPreference  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterEnergyPreference (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference => mtrBaseClusterEnergyPreference -> IO (Id MTRBaseClusterEnergyPreference)
init_ mtrBaseClusterEnergyPreference  =
    sendMsg mtrBaseClusterEnergyPreference (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterEnergyPreference)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterEnergyPreference"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterEnergyPreference mtrBaseClusterEnergyPreference, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterEnergyPreference -> device -> endpointID -> queue -> IO (Id MTRBaseClusterEnergyPreference)
initWithDevice_endpointID_queue mtrBaseClusterEnergyPreference  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterEnergyPreference (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeEnergyBalancesWithCompletion:@
readAttributeEnergyBalancesWithCompletionSelector :: Selector
readAttributeEnergyBalancesWithCompletionSelector = mkSelector "readAttributeEnergyBalancesWithCompletion:"

-- | @Selector@ for @subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEnergyBalancesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnergyBalancesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEnergyBalancesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnergyBalancesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentEnergyBalanceWithCompletion:@
readAttributeCurrentEnergyBalanceWithCompletionSelector :: Selector
readAttributeCurrentEnergyBalanceWithCompletionSelector = mkSelector "readAttributeCurrentEnergyBalanceWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:completion:@
writeAttributeCurrentEnergyBalanceWithValue_completionSelector :: Selector
writeAttributeCurrentEnergyBalanceWithValue_completionSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:params:completion:@
writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector :: Selector
writeAttributeCurrentEnergyBalanceWithValue_params_completionSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentEnergyBalanceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentEnergyBalanceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentEnergyBalanceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentEnergyBalanceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEnergyPrioritiesWithCompletion:@
readAttributeEnergyPrioritiesWithCompletionSelector :: Selector
readAttributeEnergyPrioritiesWithCompletionSelector = mkSelector "readAttributeEnergyPrioritiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEnergyPrioritiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnergyPrioritiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEnergyPrioritiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnergyPrioritiesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLowPowerModeSensitivitiesWithCompletion:@
readAttributeLowPowerModeSensitivitiesWithCompletionSelector :: Selector
readAttributeLowPowerModeSensitivitiesWithCompletionSelector = mkSelector "readAttributeLowPowerModeSensitivitiesWithCompletion:"

-- | @Selector@ for @subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLowPowerModeSensitivitiesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLowPowerModeSensitivitiesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLowPowerModeSensitivitiesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLowPowerModeSensitivitiesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentLowPowerModeSensitivityWithCompletion:@
readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector :: Selector
readAttributeCurrentLowPowerModeSensitivityWithCompletionSelector = mkSelector "readAttributeCurrentLowPowerModeSensitivityWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector :: Selector
writeAttributeCurrentLowPowerModeSensitivityWithValue_completionSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector :: Selector
writeAttributeCurrentLowPowerModeSensitivityWithValue_params_completionSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentLowPowerModeSensitivityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentLowPowerModeSensitivityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentLowPowerModeSensitivityWithClusterStateCache:endpoint:queue:completion:"

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

