{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Scenes Management
--
-- Attributes and commands for scene configuration and manipulation.
--
-- Generated bindings for @MTRBaseClusterScenesManagement@.
module ObjC.Matter.MTRBaseClusterScenesManagement
  ( MTRBaseClusterScenesManagement
  , IsMTRBaseClusterScenesManagement(..)
  , addSceneWithParams_completion
  , viewSceneWithParams_completion
  , removeSceneWithParams_completion
  , removeAllScenesWithParams_completion
  , storeSceneWithParams_completion
  , recallSceneWithParams_completion
  , getSceneMembershipWithParams_completion
  , copySceneWithParams_completion
  , readAttributeSceneTableSizeWithCompletion
  , subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completion
  , readAttributeFabricSceneInfoWithParams_completion
  , subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandler
  , readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completion
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
  , addSceneWithParams_completionSelector
  , viewSceneWithParams_completionSelector
  , removeSceneWithParams_completionSelector
  , removeAllScenesWithParams_completionSelector
  , storeSceneWithParams_completionSelector
  , recallSceneWithParams_completionSelector
  , getSceneMembershipWithParams_completionSelector
  , copySceneWithParams_completionSelector
  , readAttributeSceneTableSizeWithCompletionSelector
  , subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFabricSceneInfoWithParams_completionSelector
  , subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command AddScene
--
-- Add a scene to the scene table. Extension field sets are input as '{"ClusterID": VALUE, "AttributeValueList":[{"AttributeID": VALUE, "Value*": VALUE}]}'.
--
-- ObjC selector: @- addSceneWithParams:completion:@
addSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterAddSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
addSceneWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "addSceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ViewScene
--
-- Retrieves the requested scene entry from its Scene table.
--
-- ObjC selector: @- viewSceneWithParams:completion:@
viewSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterViewSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
viewSceneWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "viewSceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveScene
--
-- Removes the requested scene entry, corresponding to the value of the GroupID field, from its Scene Table
--
-- ObjC selector: @- removeSceneWithParams:completion:@
removeSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterRemoveSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
removeSceneWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "removeSceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveAllScenes
--
-- Remove all scenes, corresponding to the value of the GroupID field, from its Scene Table
--
-- ObjC selector: @- removeAllScenesWithParams:completion:@
removeAllScenesWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterRemoveAllScenesParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
removeAllScenesWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "removeAllScenesWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StoreScene
--
-- Adds the scene entry into its Scene Table along with all extension field sets corresponding to the current state of other clusters on the same endpoint
--
-- ObjC selector: @- storeSceneWithParams:completion:@
storeSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterStoreSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
storeSceneWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "storeSceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RecallScene
--
-- Set the attributes and corresponding state for each other cluster implemented on the endpoint accordingly to the resquested scene entry in the Scene Table
--
-- ObjC selector: @- recallSceneWithParams:completion:@
recallSceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterRecallSceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
recallSceneWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "recallSceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command GetSceneMembership
--
-- This command can be used to get the used scene identifiers within a certain group, for the endpoint that implements this cluster.
--
-- ObjC selector: @- getSceneMembershipWithParams:completion:@
getSceneMembershipWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterGetSceneMembershipParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
getSceneMembershipWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "getSceneMembershipWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CopyScene
--
-- This command allows a client to efficiently copy scenes from one group/scene identifier pair to another group/scene identifier pair.
--
-- ObjC selector: @- copySceneWithParams:completion:@
copySceneWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRScenesManagementClusterCopySceneParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
copySceneWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "copySceneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSceneTableSizeWithCompletion:@
readAttributeSceneTableSizeWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeSceneTableSizeWithCompletion mtrBaseClusterScenesManagement  completion =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeSceneTableSizeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFabricSceneInfoWithParams:completion:@
readAttributeFabricSceneInfoWithParams_completion :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRReadParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> IO ()
readAttributeFabricSceneInfoWithParams_completion mtrBaseClusterScenesManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeFabricSceneInfoWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterScenesManagement  completion =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterScenesManagement  completion =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterScenesManagement  completion =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterScenesManagement  completion =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterScenesManagement  completion =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRSubscribeParams params) => mtrBaseClusterScenesManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterScenesManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterScenesManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement => mtrBaseClusterScenesManagement -> IO (Id MTRBaseClusterScenesManagement)
init_ mtrBaseClusterScenesManagement  =
    sendMsg mtrBaseClusterScenesManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterScenesManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterScenesManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterScenesManagement mtrBaseClusterScenesManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterScenesManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterScenesManagement)
initWithDevice_endpointID_queue mtrBaseClusterScenesManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterScenesManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSceneWithParams:completion:@
addSceneWithParams_completionSelector :: Selector
addSceneWithParams_completionSelector = mkSelector "addSceneWithParams:completion:"

-- | @Selector@ for @viewSceneWithParams:completion:@
viewSceneWithParams_completionSelector :: Selector
viewSceneWithParams_completionSelector = mkSelector "viewSceneWithParams:completion:"

-- | @Selector@ for @removeSceneWithParams:completion:@
removeSceneWithParams_completionSelector :: Selector
removeSceneWithParams_completionSelector = mkSelector "removeSceneWithParams:completion:"

-- | @Selector@ for @removeAllScenesWithParams:completion:@
removeAllScenesWithParams_completionSelector :: Selector
removeAllScenesWithParams_completionSelector = mkSelector "removeAllScenesWithParams:completion:"

-- | @Selector@ for @storeSceneWithParams:completion:@
storeSceneWithParams_completionSelector :: Selector
storeSceneWithParams_completionSelector = mkSelector "storeSceneWithParams:completion:"

-- | @Selector@ for @recallSceneWithParams:completion:@
recallSceneWithParams_completionSelector :: Selector
recallSceneWithParams_completionSelector = mkSelector "recallSceneWithParams:completion:"

-- | @Selector@ for @getSceneMembershipWithParams:completion:@
getSceneMembershipWithParams_completionSelector :: Selector
getSceneMembershipWithParams_completionSelector = mkSelector "getSceneMembershipWithParams:completion:"

-- | @Selector@ for @copySceneWithParams:completion:@
copySceneWithParams_completionSelector :: Selector
copySceneWithParams_completionSelector = mkSelector "copySceneWithParams:completion:"

-- | @Selector@ for @readAttributeSceneTableSizeWithCompletion:@
readAttributeSceneTableSizeWithCompletionSelector :: Selector
readAttributeSceneTableSizeWithCompletionSelector = mkSelector "readAttributeSceneTableSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSceneTableSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSceneTableSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSceneTableSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSceneTableSizeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFabricSceneInfoWithParams:completion:@
readAttributeFabricSceneInfoWithParams_completionSelector :: Selector
readAttributeFabricSceneInfoWithParams_completionSelector = mkSelector "readAttributeFabricSceneInfoWithParams:completion:"

-- | @Selector@ for @subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeFabricSceneInfoWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFabricSceneInfoWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeFabricSceneInfoWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFabricSceneInfoWithClusterStateCache:endpoint:queue:completion:"

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

