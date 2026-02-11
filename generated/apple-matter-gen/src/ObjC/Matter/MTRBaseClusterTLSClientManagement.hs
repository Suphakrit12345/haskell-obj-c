{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Client Management
--
-- This Cluster is used to provision TLS Endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRBaseClusterTLSClientManagement@.
module ObjC.Matter.MTRBaseClusterTLSClientManagement
  ( MTRBaseClusterTLSClientManagement
  , IsMTRBaseClusterTLSClientManagement(..)
  , provisionEndpointWithParams_completion
  , findEndpointWithParams_completion
  , removeEndpointWithParams_completion
  , readAttributeMaxProvisionedWithCompletion
  , subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completion
  , readAttributeProvisionedEndpointsWithParams_completion
  , subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandler
  , readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completion
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
  , provisionEndpointWithParams_completionSelector
  , findEndpointWithParams_completionSelector
  , removeEndpointWithParams_completionSelector
  , readAttributeMaxProvisionedWithCompletionSelector
  , subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProvisionedEndpointsWithParams_completionSelector
  , subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command ProvisionEndpoint
--
-- This command is used to provision a TLS Endpoint for the provided Hostname / Port combination.
--
-- ObjC selector: @- provisionEndpointWithParams:completion:@
provisionEndpointWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRTLSClientManagementClusterProvisionEndpointParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
provisionEndpointWithParams_completion mtrBaseClusterTLSClientManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "provisionEndpointWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command FindEndpoint
--
-- This command is used to find a TLS Endpoint by its ID.
--
-- ObjC selector: @- findEndpointWithParams:completion:@
findEndpointWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRTLSClientManagementClusterFindEndpointParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
findEndpointWithParams_completion mtrBaseClusterTLSClientManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "findEndpointWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveEndpoint
--
-- This command is used to remove a TLS Endpoint by its ID.
--
-- ObjC selector: @- removeEndpointWithParams:completion:@
removeEndpointWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRTLSClientManagementClusterRemoveEndpointParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
removeEndpointWithParams_completion mtrBaseClusterTLSClientManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "removeEndpointWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxProvisionedWithCompletion:@
readAttributeMaxProvisionedWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeMaxProvisionedWithCompletion mtrBaseClusterTLSClientManagement  completion =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeMaxProvisionedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProvisionedEndpointsWithParams:completion:@
readAttributeProvisionedEndpointsWithParams_completion :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRReadParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> IO ()
readAttributeProvisionedEndpointsWithParams_completion mtrBaseClusterTLSClientManagement  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeProvisionedEndpointsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTLSClientManagement  completion =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTLSClientManagement  completion =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTLSClientManagement  completion =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTLSClientManagement  completion =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTLSClientManagement  completion =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSClientManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSClientManagement  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTLSClientManagement (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement => mtrBaseClusterTLSClientManagement -> IO (Id MTRBaseClusterTLSClientManagement)
init_ mtrBaseClusterTLSClientManagement  =
    sendMsg mtrBaseClusterTLSClientManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterTLSClientManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSClientManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTLSClientManagement mtrBaseClusterTLSClientManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTLSClientManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTLSClientManagement)
initWithDevice_endpointID_queue mtrBaseClusterTLSClientManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterTLSClientManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionEndpointWithParams:completion:@
provisionEndpointWithParams_completionSelector :: Selector
provisionEndpointWithParams_completionSelector = mkSelector "provisionEndpointWithParams:completion:"

-- | @Selector@ for @findEndpointWithParams:completion:@
findEndpointWithParams_completionSelector :: Selector
findEndpointWithParams_completionSelector = mkSelector "findEndpointWithParams:completion:"

-- | @Selector@ for @removeEndpointWithParams:completion:@
removeEndpointWithParams_completionSelector :: Selector
removeEndpointWithParams_completionSelector = mkSelector "removeEndpointWithParams:completion:"

-- | @Selector@ for @readAttributeMaxProvisionedWithCompletion:@
readAttributeMaxProvisionedWithCompletionSelector :: Selector
readAttributeMaxProvisionedWithCompletionSelector = mkSelector "readAttributeMaxProvisionedWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxProvisionedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxProvisionedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxProvisionedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxProvisionedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProvisionedEndpointsWithParams:completion:@
readAttributeProvisionedEndpointsWithParams_completionSelector :: Selector
readAttributeProvisionedEndpointsWithParams_completionSelector = mkSelector "readAttributeProvisionedEndpointsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProvisionedEndpointsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProvisionedEndpointsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProvisionedEndpointsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProvisionedEndpointsWithClusterStateCache:endpoint:queue:completion:"

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

