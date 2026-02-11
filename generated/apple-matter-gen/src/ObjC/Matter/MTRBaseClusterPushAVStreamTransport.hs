{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Push AV Stream Transport
--
-- This cluster implements the upload of Audio and Video streams from the Push AV Stream Transport Cluster using suitable push-based transports.
--
-- Generated bindings for @MTRBaseClusterPushAVStreamTransport@.
module ObjC.Matter.MTRBaseClusterPushAVStreamTransport
  ( MTRBaseClusterPushAVStreamTransport
  , IsMTRBaseClusterPushAVStreamTransport(..)
  , allocatePushTransportWithParams_completion
  , deallocatePushTransportWithParams_completion
  , modifyPushTransportWithParams_completion
  , setTransportStatusWithParams_completion
  , manuallyTriggerTransportWithParams_completion
  , findTransportWithParams_completion
  , readAttributeSupportedFormatsWithCompletion
  , subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentConnectionsWithParams_completion
  , subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completion
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
  , allocatePushTransportWithParams_completionSelector
  , deallocatePushTransportWithParams_completionSelector
  , modifyPushTransportWithParams_completionSelector
  , setTransportStatusWithParams_completionSelector
  , manuallyTriggerTransportWithParams_completionSelector
  , findTransportWithParams_completionSelector
  , readAttributeSupportedFormatsWithCompletionSelector
  , subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentConnectionsWithParams_completionSelector
  , subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command AllocatePushTransport
--
-- This command SHALL allocate a transport and return a PushTransportConnectionID.
--
-- ObjC selector: @- allocatePushTransportWithParams:completion:@
allocatePushTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterAllocatePushTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
allocatePushTransportWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "allocatePushTransportWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command DeallocatePushTransport
--
-- This command SHALL be generated to request the Node deallocates the specified transport.
--
-- ObjC selector: @- deallocatePushTransportWithParams:completion:@
deallocatePushTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterDeallocatePushTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
deallocatePushTransportWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "deallocatePushTransportWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ModifyPushTransport
--
-- This command is used to request the Node modifies the configuration of the specified push transport.
--
-- ObjC selector: @- modifyPushTransportWithParams:completion:@
modifyPushTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterModifyPushTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
modifyPushTransportWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "modifyPushTransportWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetTransportStatus
--
-- This command SHALL be generated to request the Node modifies the Transport Status of a specified transport or all transports.
--
-- ObjC selector: @- setTransportStatusWithParams:completion:@
setTransportStatusWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterSetTransportStatusParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
setTransportStatusWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "setTransportStatusWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ManuallyTriggerTransport
--
-- This command SHALL be generated to request the Node to manually start the specified push transport.
--
-- ObjC selector: @- manuallyTriggerTransportWithParams:completion:@
manuallyTriggerTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
manuallyTriggerTransportWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "manuallyTriggerTransportWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command FindTransport
--
-- This command SHALL return the Transport Configuration for the specified push transport or all allocated transports for the fabric if null.
--
-- ObjC selector: @- findTransportWithParams:completion:@
findTransportWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterFindTransportParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
findTransportWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "findTransportWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedFormatsWithCompletion:@
readAttributeSupportedFormatsWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeSupportedFormatsWithCompletion mtrBaseClusterPushAVStreamTransport  completion =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeSupportedFormatsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentConnectionsWithParams:completion:@
readAttributeCurrentConnectionsWithParams_completion :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRReadParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> IO ()
readAttributeCurrentConnectionsWithParams_completion mtrBaseClusterPushAVStreamTransport  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeCurrentConnectionsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterPushAVStreamTransport  completion =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterPushAVStreamTransport  completion =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterPushAVStreamTransport  completion =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterPushAVStreamTransport  completion =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterPushAVStreamTransport  completion =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRSubscribeParams params) => mtrBaseClusterPushAVStreamTransport -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPushAVStreamTransport  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport => mtrBaseClusterPushAVStreamTransport -> IO (Id MTRBaseClusterPushAVStreamTransport)
init_ mtrBaseClusterPushAVStreamTransport  =
    sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterPushAVStreamTransport)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterPushAVStreamTransport"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterPushAVStreamTransport mtrBaseClusterPushAVStreamTransport, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterPushAVStreamTransport -> device -> endpointID -> queue -> IO (Id MTRBaseClusterPushAVStreamTransport)
initWithDevice_endpointID_queue mtrBaseClusterPushAVStreamTransport  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterPushAVStreamTransport (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allocatePushTransportWithParams:completion:@
allocatePushTransportWithParams_completionSelector :: Selector
allocatePushTransportWithParams_completionSelector = mkSelector "allocatePushTransportWithParams:completion:"

-- | @Selector@ for @deallocatePushTransportWithParams:completion:@
deallocatePushTransportWithParams_completionSelector :: Selector
deallocatePushTransportWithParams_completionSelector = mkSelector "deallocatePushTransportWithParams:completion:"

-- | @Selector@ for @modifyPushTransportWithParams:completion:@
modifyPushTransportWithParams_completionSelector :: Selector
modifyPushTransportWithParams_completionSelector = mkSelector "modifyPushTransportWithParams:completion:"

-- | @Selector@ for @setTransportStatusWithParams:completion:@
setTransportStatusWithParams_completionSelector :: Selector
setTransportStatusWithParams_completionSelector = mkSelector "setTransportStatusWithParams:completion:"

-- | @Selector@ for @manuallyTriggerTransportWithParams:completion:@
manuallyTriggerTransportWithParams_completionSelector :: Selector
manuallyTriggerTransportWithParams_completionSelector = mkSelector "manuallyTriggerTransportWithParams:completion:"

-- | @Selector@ for @findTransportWithParams:completion:@
findTransportWithParams_completionSelector :: Selector
findTransportWithParams_completionSelector = mkSelector "findTransportWithParams:completion:"

-- | @Selector@ for @readAttributeSupportedFormatsWithCompletion:@
readAttributeSupportedFormatsWithCompletionSelector :: Selector
readAttributeSupportedFormatsWithCompletionSelector = mkSelector "readAttributeSupportedFormatsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedFormatsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedFormatsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedFormatsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedFormatsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentConnectionsWithParams:completion:@
readAttributeCurrentConnectionsWithParams_completionSelector :: Selector
readAttributeCurrentConnectionsWithParams_completionSelector = mkSelector "readAttributeCurrentConnectionsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentConnectionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentConnectionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentConnectionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentConnectionsWithClusterStateCache:endpoint:queue:completion:"

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

