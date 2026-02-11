{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Administrator
--
-- An instance of the Joint Fabric Administrator Cluster only applies to Joint Fabric Administrator nodes fulfilling the role of Anchor CA.
--
-- Generated bindings for @MTRBaseClusterJointFabricAdministrator@.
module ObjC.Matter.MTRBaseClusterJointFabricAdministrator
  ( MTRBaseClusterJointFabricAdministrator
  , IsMTRBaseClusterJointFabricAdministrator(..)
  , icaccsrRequestWithParams_completion
  , icaccsrRequestWithCompletion
  , addICACWithParams_completion
  , openJointCommissioningWindowWithParams_completion
  , transferAnchorRequestWithParams_completion
  , transferAnchorRequestWithCompletion
  , transferAnchorCompleteWithParams_completion
  , transferAnchorCompleteWithCompletion
  , announceJointFabricAdministratorWithParams_completion
  , readAttributeAdministratorFabricIndexWithCompletion
  , subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completion
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
  , icaccsrRequestWithParams_completionSelector
  , icaccsrRequestWithCompletionSelector
  , addICACWithParams_completionSelector
  , openJointCommissioningWindowWithParams_completionSelector
  , transferAnchorRequestWithParams_completionSelector
  , transferAnchorRequestWithCompletionSelector
  , transferAnchorCompleteWithParams_completionSelector
  , transferAnchorCompleteWithCompletionSelector
  , announceJointFabricAdministratorWithParams_completionSelector
  , readAttributeAdministratorFabricIndexWithCompletionSelector
  , subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command ICACCSRRequest
--
-- This command SHALL be generated during Joint Commissioning Method and subsequently be responded in the form of an ICACCSRResponse command.
--
-- ObjC selector: @- ICACCSRRequestWithParams:completion:@
icaccsrRequestWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterICACCSRRequestParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
icaccsrRequestWithParams_completion mtrBaseClusterJointFabricAdministrator  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "ICACCSRRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- ICACCSRRequestWithCompletion:@
icaccsrRequestWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
icaccsrRequestWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "ICACCSRRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command AddICAC
--
-- This command SHALL be generated and executed during Joint Commissioning Method and subsequently be responded in the form of an ICACResponse command.
--
-- ObjC selector: @- addICACWithParams:completion:@
addICACWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAddICACParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
addICACWithParams_completion mtrBaseClusterJointFabricAdministrator  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "addICACWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command OpenJointCommissioningWindow
--
-- This command SHALL fail with a InvalidAdministratorFabricIndex status code sent back to the initiator if the AdministratorFabricIndex field has the value of null.
--
-- ObjC selector: @- openJointCommissioningWindowWithParams:completion:@
openJointCommissioningWindowWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
openJointCommissioningWindowWithParams_completion mtrBaseClusterJointFabricAdministrator  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "openJointCommissioningWindowWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command TransferAnchorRequest
--
-- This command SHALL be sent by a candidate Joint Fabric Anchor Administrator to the current Joint Fabric Anchor Administrator to request transfer of the Anchor Fabric.
--
-- ObjC selector: @- transferAnchorRequestWithParams:completion:@
transferAnchorRequestWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorRequestParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
transferAnchorRequestWithParams_completion mtrBaseClusterJointFabricAdministrator  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "transferAnchorRequestWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- transferAnchorRequestWithCompletion:@
transferAnchorRequestWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
transferAnchorRequestWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "transferAnchorRequestWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command TransferAnchorComplete
--
-- This command SHALL indicate the completion of the transfer of the Anchor Fabric to another Joint Fabric Ecosystem Administrator.
--
-- ObjC selector: @- transferAnchorCompleteWithParams:completion:@
transferAnchorCompleteWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorCompleteParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
transferAnchorCompleteWithParams_completion mtrBaseClusterJointFabricAdministrator  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "transferAnchorCompleteWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- transferAnchorCompleteWithCompletion:@
transferAnchorCompleteWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
transferAnchorCompleteWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "transferAnchorCompleteWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command AnnounceJointFabricAdministrator
--
-- This command SHALL be used for communicating to client the endpoint that holds the Joint Fabric Administrator Cluster.
--
-- ObjC selector: @- announceJointFabricAdministratorWithParams:completion:@
announceJointFabricAdministratorWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
announceJointFabricAdministratorWithParams_completion mtrBaseClusterJointFabricAdministrator  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "announceJointFabricAdministratorWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAdministratorFabricIndexWithCompletion:@
readAttributeAdministratorFabricIndexWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeAdministratorFabricIndexWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "readAttributeAdministratorFabricIndexWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterJointFabricAdministrator  completion =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> IO (Id MTRBaseClusterJointFabricAdministrator)
init_ mtrBaseClusterJointFabricAdministrator  =
    sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterJointFabricAdministrator)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterJointFabricAdministrator -> device -> endpointID -> queue -> IO (Id MTRBaseClusterJointFabricAdministrator)
initWithDevice_endpointID_queue mtrBaseClusterJointFabricAdministrator  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterJointFabricAdministrator (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ICACCSRRequestWithParams:completion:@
icaccsrRequestWithParams_completionSelector :: Selector
icaccsrRequestWithParams_completionSelector = mkSelector "ICACCSRRequestWithParams:completion:"

-- | @Selector@ for @ICACCSRRequestWithCompletion:@
icaccsrRequestWithCompletionSelector :: Selector
icaccsrRequestWithCompletionSelector = mkSelector "ICACCSRRequestWithCompletion:"

-- | @Selector@ for @addICACWithParams:completion:@
addICACWithParams_completionSelector :: Selector
addICACWithParams_completionSelector = mkSelector "addICACWithParams:completion:"

-- | @Selector@ for @openJointCommissioningWindowWithParams:completion:@
openJointCommissioningWindowWithParams_completionSelector :: Selector
openJointCommissioningWindowWithParams_completionSelector = mkSelector "openJointCommissioningWindowWithParams:completion:"

-- | @Selector@ for @transferAnchorRequestWithParams:completion:@
transferAnchorRequestWithParams_completionSelector :: Selector
transferAnchorRequestWithParams_completionSelector = mkSelector "transferAnchorRequestWithParams:completion:"

-- | @Selector@ for @transferAnchorRequestWithCompletion:@
transferAnchorRequestWithCompletionSelector :: Selector
transferAnchorRequestWithCompletionSelector = mkSelector "transferAnchorRequestWithCompletion:"

-- | @Selector@ for @transferAnchorCompleteWithParams:completion:@
transferAnchorCompleteWithParams_completionSelector :: Selector
transferAnchorCompleteWithParams_completionSelector = mkSelector "transferAnchorCompleteWithParams:completion:"

-- | @Selector@ for @transferAnchorCompleteWithCompletion:@
transferAnchorCompleteWithCompletionSelector :: Selector
transferAnchorCompleteWithCompletionSelector = mkSelector "transferAnchorCompleteWithCompletion:"

-- | @Selector@ for @announceJointFabricAdministratorWithParams:completion:@
announceJointFabricAdministratorWithParams_completionSelector :: Selector
announceJointFabricAdministratorWithParams_completionSelector = mkSelector "announceJointFabricAdministratorWithParams:completion:"

-- | @Selector@ for @readAttributeAdministratorFabricIndexWithCompletion:@
readAttributeAdministratorFabricIndexWithCompletionSelector :: Selector
readAttributeAdministratorFabricIndexWithCompletionSelector = mkSelector "readAttributeAdministratorFabricIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:"

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

