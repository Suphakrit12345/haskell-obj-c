{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Border Router Management
--
-- Manage the Thread network of Thread Border Router
--
-- Generated bindings for @MTRBaseClusterThreadBorderRouterManagement@.
module ObjC.Matter.MTRBaseClusterThreadBorderRouterManagement
  ( MTRBaseClusterThreadBorderRouterManagement
  , IsMTRBaseClusterThreadBorderRouterManagement(..)
  , getActiveDatasetRequestWithParams_completion
  , getActiveDatasetRequestWithCompletion
  , getPendingDatasetRequestWithParams_completion
  , getPendingDatasetRequestWithCompletion
  , setActiveDatasetRequestWithParams_completion
  , setPendingDatasetRequestWithParams_completion
  , readAttributeBorderRouterNameWithCompletion
  , subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeBorderAgentIDWithCompletion
  , subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadVersionWithCompletion
  , subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeInterfaceEnabledWithCompletion
  , subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveDatasetTimestampWithCompletion
  , subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completion
  , readAttributePendingDatasetTimestampWithCompletion
  , subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandler
  , readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completion
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
  , getActiveDatasetRequestWithParams_completionSelector
  , getActiveDatasetRequestWithCompletionSelector
  , getPendingDatasetRequestWithParams_completionSelector
  , getPendingDatasetRequestWithCompletionSelector
  , setActiveDatasetRequestWithParams_completionSelector
  , setPendingDatasetRequestWithParams_completionSelector
  , readAttributeBorderRouterNameWithCompletionSelector
  , subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBorderAgentIDWithCompletionSelector
  , subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadVersionWithCompletionSelector
  , subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeInterfaceEnabledWithCompletionSelector
  , subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveDatasetTimestampWithCompletionSelector
  , subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePendingDatasetTimestampWithCompletionSelector
  , subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command GetActiveDatasetRequest
--
-- This command SHALL be used to request the active operational dataset of the Thread network to which the border router is connected.
--
-- ObjC selector: @- getActiveDatasetRequestWithParams:completion:@
getActiveDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
getActiveDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "getActiveDatasetRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getActiveDatasetRequestWithCompletion:@
getActiveDatasetRequestWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
getActiveDatasetRequestWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "getActiveDatasetRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command GetPendingDatasetRequest
--
-- This command SHALL be used to request the pending dataset of the Thread network to which the border router is connected.
--
-- ObjC selector: @- getPendingDatasetRequestWithParams:completion:@
getPendingDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
getPendingDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "getPendingDatasetRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getPendingDatasetRequestWithCompletion:@
getPendingDatasetRequestWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
getPendingDatasetRequestWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "getPendingDatasetRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SetActiveDatasetRequest
--
-- This command SHALL be used to set the active Dataset of the Thread network to which the Border Router is connected, when there is no active dataset already.
--
-- ObjC selector: @- setActiveDatasetRequestWithParams:completion:@
setActiveDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
setActiveDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "setActiveDatasetRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetPendingDatasetRequest
--
-- This command SHALL be used to set or update the pending Dataset of the Thread network to which the Border Router is connected, if the Border Router supports PANChange Feature.
--
-- ObjC selector: @- setPendingDatasetRequestWithParams:completion:@
setPendingDatasetRequestWithParams_completion :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> IO ()
setPendingDatasetRequestWithParams_completion mtrBaseClusterThreadBorderRouterManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "setPendingDatasetRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBorderRouterNameWithCompletion:@
readAttributeBorderRouterNameWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeBorderRouterNameWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeBorderRouterNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBorderAgentIDWithCompletion:@
readAttributeBorderAgentIDWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeBorderAgentIDWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeBorderAgentIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeThreadVersionWithCompletion:@
readAttributeThreadVersionWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeThreadVersionWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeThreadVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInterfaceEnabledWithCompletion:@
readAttributeInterfaceEnabledWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeInterfaceEnabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveDatasetTimestampWithCompletion:@
readAttributeActiveDatasetTimestampWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeActiveDatasetTimestampWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeActiveDatasetTimestampWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePendingDatasetTimestampWithCompletion:@
readAttributePendingDatasetTimestampWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributePendingDatasetTimestampWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributePendingDatasetTimestampWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterThreadBorderRouterManagement  completion =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRSubscribeParams params) => mtrBaseClusterThreadBorderRouterManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadBorderRouterManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement => mtrBaseClusterThreadBorderRouterManagement -> IO (Id MTRBaseClusterThreadBorderRouterManagement)
init_ mtrBaseClusterThreadBorderRouterManagement  =
    sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterThreadBorderRouterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadBorderRouterManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterThreadBorderRouterManagement mtrBaseClusterThreadBorderRouterManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterThreadBorderRouterManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterThreadBorderRouterManagement)
initWithDevice_endpointID_queue mtrBaseClusterThreadBorderRouterManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterThreadBorderRouterManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getActiveDatasetRequestWithParams:completion:@
getActiveDatasetRequestWithParams_completionSelector :: Selector
getActiveDatasetRequestWithParams_completionSelector = mkSelector "getActiveDatasetRequestWithParams:completion:"

-- | @Selector@ for @getActiveDatasetRequestWithCompletion:@
getActiveDatasetRequestWithCompletionSelector :: Selector
getActiveDatasetRequestWithCompletionSelector = mkSelector "getActiveDatasetRequestWithCompletion:"

-- | @Selector@ for @getPendingDatasetRequestWithParams:completion:@
getPendingDatasetRequestWithParams_completionSelector :: Selector
getPendingDatasetRequestWithParams_completionSelector = mkSelector "getPendingDatasetRequestWithParams:completion:"

-- | @Selector@ for @getPendingDatasetRequestWithCompletion:@
getPendingDatasetRequestWithCompletionSelector :: Selector
getPendingDatasetRequestWithCompletionSelector = mkSelector "getPendingDatasetRequestWithCompletion:"

-- | @Selector@ for @setActiveDatasetRequestWithParams:completion:@
setActiveDatasetRequestWithParams_completionSelector :: Selector
setActiveDatasetRequestWithParams_completionSelector = mkSelector "setActiveDatasetRequestWithParams:completion:"

-- | @Selector@ for @setPendingDatasetRequestWithParams:completion:@
setPendingDatasetRequestWithParams_completionSelector :: Selector
setPendingDatasetRequestWithParams_completionSelector = mkSelector "setPendingDatasetRequestWithParams:completion:"

-- | @Selector@ for @readAttributeBorderRouterNameWithCompletion:@
readAttributeBorderRouterNameWithCompletionSelector :: Selector
readAttributeBorderRouterNameWithCompletionSelector = mkSelector "readAttributeBorderRouterNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBorderRouterNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBorderRouterNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBorderRouterNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBorderRouterNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBorderAgentIDWithCompletion:@
readAttributeBorderAgentIDWithCompletionSelector :: Selector
readAttributeBorderAgentIDWithCompletionSelector = mkSelector "readAttributeBorderAgentIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBorderAgentIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBorderAgentIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBorderAgentIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBorderAgentIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadVersionWithCompletion:@
readAttributeThreadVersionWithCompletionSelector :: Selector
readAttributeThreadVersionWithCompletionSelector = mkSelector "readAttributeThreadVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeThreadVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeThreadVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithCompletion:@
readAttributeInterfaceEnabledWithCompletionSelector :: Selector
readAttributeInterfaceEnabledWithCompletionSelector = mkSelector "readAttributeInterfaceEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInterfaceEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInterfaceEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInterfaceEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInterfaceEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveDatasetTimestampWithCompletion:@
readAttributeActiveDatasetTimestampWithCompletionSelector :: Selector
readAttributeActiveDatasetTimestampWithCompletionSelector = mkSelector "readAttributeActiveDatasetTimestampWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveDatasetTimestampWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveDatasetTimestampWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePendingDatasetTimestampWithCompletion:@
readAttributePendingDatasetTimestampWithCompletionSelector :: Selector
readAttributePendingDatasetTimestampWithCompletionSelector = mkSelector "readAttributePendingDatasetTimestampWithCompletion:"

-- | @Selector@ for @subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePendingDatasetTimestampWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePendingDatasetTimestampWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:@
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePendingDatasetTimestampWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePendingDatasetTimestampWithClusterStateCache:endpoint:queue:completion:"

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

