{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Administrator Commissioning
--
-- Commands to trigger a Node to allow a new Administrator to commission it.
--
-- Generated bindings for @MTRBaseClusterAdministratorCommissioning@.
module ObjC.Matter.MTRBaseClusterAdministratorCommissioning
  ( MTRBaseClusterAdministratorCommissioning
  , IsMTRBaseClusterAdministratorCommissioning(..)
  , openCommissioningWindowWithParams_completion
  , openBasicCommissioningWindowWithParams_completion
  , revokeCommissioningWithParams_completion
  , revokeCommissioningWithCompletion
  , readAttributeWindowStatusWithCompletion
  , subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeAdminFabricIndexWithCompletion
  , subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completion
  , readAttributeAdminVendorIdWithCompletion
  , subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completion
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
  , openCommissioningWindowWithParams_completionHandler
  , openBasicCommissioningWindowWithParams_completionHandler
  , revokeCommissioningWithParams_completionHandler
  , revokeCommissioningWithCompletionHandler
  , readAttributeWindowStatusWithCompletionHandler
  , subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAdminFabricIndexWithCompletionHandler
  , subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAdminVendorIdWithCompletionHandler
  , subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandler
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
  , openCommissioningWindowWithParams_completionSelector
  , openBasicCommissioningWindowWithParams_completionSelector
  , revokeCommissioningWithParams_completionSelector
  , revokeCommissioningWithCompletionSelector
  , readAttributeWindowStatusWithCompletionSelector
  , subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdminFabricIndexWithCompletionSelector
  , subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdminVendorIdWithCompletionSelector
  , subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector
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
  , openCommissioningWindowWithParams_completionHandlerSelector
  , openBasicCommissioningWindowWithParams_completionHandlerSelector
  , revokeCommissioningWithParams_completionHandlerSelector
  , revokeCommissioningWithCompletionHandlerSelector
  , readAttributeWindowStatusWithCompletionHandlerSelector
  , subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAdminFabricIndexWithCompletionHandlerSelector
  , subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAdminVendorIdWithCompletionHandlerSelector
  , subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command OpenCommissioningWindow
--
-- This command is used by a current Administrator to instruct a Node to go into commissioning mode.
--
-- ObjC selector: @- openCommissioningWindowWithParams:completion:@
openCommissioningWindowWithParams_completion :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openCommissioningWindowWithParams_completion mtrBaseClusterAdministratorCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "openCommissioningWindowWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command OpenBasicCommissioningWindow
--
-- This command MAY be used by a current Administrator to instruct a Node to go into commissioning mode, if the node supports the Basic Commissioning Method.
--
-- ObjC selector: @- openBasicCommissioningWindowWithParams:completion:@
openBasicCommissioningWindowWithParams_completion :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openBasicCommissioningWindowWithParams_completion mtrBaseClusterAdministratorCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "openBasicCommissioningWindowWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RevokeCommissioning
--
-- This command is used by a current Administrator to instruct a Node to revoke any active OpenCommissioningWindow or OpenBasicCommissioningWindow command.
--
-- ObjC selector: @- revokeCommissioningWithParams:completion:@
revokeCommissioningWithParams_completion :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterRevokeCommissioningParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
revokeCommissioningWithParams_completion mtrBaseClusterAdministratorCommissioning  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- revokeCommissioningWithCompletion:@
revokeCommissioningWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
revokeCommissioningWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeWindowStatusWithCompletion:@
readAttributeWindowStatusWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeWindowStatusWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeWindowStatusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAdminFabricIndexWithCompletion:@
readAttributeAdminFabricIndexWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAdminFabricIndexWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAdminVendorIdWithCompletion:@
readAttributeAdminVendorIdWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminVendorIdWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAdminVendorIdWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterAdministratorCommissioning  completion =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> IO (Id MTRBaseClusterAdministratorCommissioning)
init_ mtrBaseClusterAdministratorCommissioning  =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterAdministratorCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterAdministratorCommissioning -> device -> CUShort -> queue -> IO (Id MTRBaseClusterAdministratorCommissioning)
initWithDevice_endpoint_queue mtrBaseClusterAdministratorCommissioning  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- openCommissioningWindowWithParams:completionHandler:@
openCommissioningWindowWithParams_completionHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openCommissioningWindowWithParams_completionHandler mtrBaseClusterAdministratorCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "openCommissioningWindowWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- openBasicCommissioningWindowWithParams:completionHandler:@
openBasicCommissioningWindowWithParams_completionHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openBasicCommissioningWindowWithParams_completionHandler mtrBaseClusterAdministratorCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "openBasicCommissioningWindowWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- revokeCommissioningWithParams:completionHandler:@
revokeCommissioningWithParams_completionHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterRevokeCommissioningParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
revokeCommissioningWithParams_completionHandler mtrBaseClusterAdministratorCommissioning  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- revokeCommissioningWithCompletionHandler:@
revokeCommissioningWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
revokeCommissioningWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "revokeCommissioningWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeWindowStatusWithCompletionHandler:@
readAttributeWindowStatusWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeWindowStatusWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeWindowStatusWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAdminFabricIndexWithCompletionHandler:@
readAttributeAdminFabricIndexWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAdminFabricIndexWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAdminVendorIdWithCompletionHandler:@
readAttributeAdminVendorIdWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminVendorIdWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAdminVendorIdWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterAdministratorCommissioning  completionHandler =
    sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterAdministratorCommissioning -> device -> endpointID -> queue -> IO (Id MTRBaseClusterAdministratorCommissioning)
initWithDevice_endpointID_queue mtrBaseClusterAdministratorCommissioning  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterAdministratorCommissioning (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openCommissioningWindowWithParams:completion:@
openCommissioningWindowWithParams_completionSelector :: Selector
openCommissioningWindowWithParams_completionSelector = mkSelector "openCommissioningWindowWithParams:completion:"

-- | @Selector@ for @openBasicCommissioningWindowWithParams:completion:@
openBasicCommissioningWindowWithParams_completionSelector :: Selector
openBasicCommissioningWindowWithParams_completionSelector = mkSelector "openBasicCommissioningWindowWithParams:completion:"

-- | @Selector@ for @revokeCommissioningWithParams:completion:@
revokeCommissioningWithParams_completionSelector :: Selector
revokeCommissioningWithParams_completionSelector = mkSelector "revokeCommissioningWithParams:completion:"

-- | @Selector@ for @revokeCommissioningWithCompletion:@
revokeCommissioningWithCompletionSelector :: Selector
revokeCommissioningWithCompletionSelector = mkSelector "revokeCommissioningWithCompletion:"

-- | @Selector@ for @readAttributeWindowStatusWithCompletion:@
readAttributeWindowStatusWithCompletionSelector :: Selector
readAttributeWindowStatusWithCompletionSelector = mkSelector "readAttributeWindowStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithCompletion:@
readAttributeAdminFabricIndexWithCompletionSelector :: Selector
readAttributeAdminFabricIndexWithCompletionSelector = mkSelector "readAttributeAdminFabricIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAdminVendorIdWithCompletion:@
readAttributeAdminVendorIdWithCompletionSelector :: Selector
readAttributeAdminVendorIdWithCompletionSelector = mkSelector "readAttributeAdminVendorIdWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @openCommissioningWindowWithParams:completionHandler:@
openCommissioningWindowWithParams_completionHandlerSelector :: Selector
openCommissioningWindowWithParams_completionHandlerSelector = mkSelector "openCommissioningWindowWithParams:completionHandler:"

-- | @Selector@ for @openBasicCommissioningWindowWithParams:completionHandler:@
openBasicCommissioningWindowWithParams_completionHandlerSelector :: Selector
openBasicCommissioningWindowWithParams_completionHandlerSelector = mkSelector "openBasicCommissioningWindowWithParams:completionHandler:"

-- | @Selector@ for @revokeCommissioningWithParams:completionHandler:@
revokeCommissioningWithParams_completionHandlerSelector :: Selector
revokeCommissioningWithParams_completionHandlerSelector = mkSelector "revokeCommissioningWithParams:completionHandler:"

-- | @Selector@ for @revokeCommissioningWithCompletionHandler:@
revokeCommissioningWithCompletionHandlerSelector :: Selector
revokeCommissioningWithCompletionHandlerSelector = mkSelector "revokeCommissioningWithCompletionHandler:"

-- | @Selector@ for @readAttributeWindowStatusWithCompletionHandler:@
readAttributeWindowStatusWithCompletionHandlerSelector :: Selector
readAttributeWindowStatusWithCompletionHandlerSelector = mkSelector "readAttributeWindowStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithCompletionHandler:@
readAttributeAdminFabricIndexWithCompletionHandlerSelector :: Selector
readAttributeAdminFabricIndexWithCompletionHandlerSelector = mkSelector "readAttributeAdminFabricIndexWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAdminVendorIdWithCompletionHandler:@
readAttributeAdminVendorIdWithCompletionHandlerSelector :: Selector
readAttributeAdminVendorIdWithCompletionHandlerSelector = mkSelector "readAttributeAdminVendorIdWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:"

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

