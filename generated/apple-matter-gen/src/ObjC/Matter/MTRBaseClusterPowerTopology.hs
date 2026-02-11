{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Topology
--
-- The Power Topology Cluster provides a mechanism for expressing how power is flowing between endpoints.
--
-- Generated bindings for @MTRBaseClusterPowerTopology@.
module ObjC.Matter.MTRBaseClusterPowerTopology
  ( MTRBaseClusterPowerTopology
  , IsMTRBaseClusterPowerTopology(..)
  , readAttributeAvailableEndpointsWithCompletion
  , subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandler
  , readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveEndpointsWithCompletion
  , subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completion
  , readAttributeElectricalCircuitNodesWithParams_completion
  , writeAttributeElectricalCircuitNodesWithValue_completion
  , writeAttributeElectricalCircuitNodesWithValue_params_completion
  , subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandler
  , readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeAvailableEndpointsWithCompletionSelector
  , subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveEndpointsWithCompletionSelector
  , subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeElectricalCircuitNodesWithParams_completionSelector
  , writeAttributeElectricalCircuitNodesWithValue_completionSelector
  , writeAttributeElectricalCircuitNodesWithValue_params_completionSelector
  , subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeAvailableEndpointsWithCompletion:@
readAttributeAvailableEndpointsWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeAvailableEndpointsWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeAvailableEndpointsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveEndpointsWithCompletion:@
readAttributeActiveEndpointsWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeActiveEndpointsWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeActiveEndpointsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeElectricalCircuitNodesWithParams:completion:@
readAttributeElectricalCircuitNodesWithParams_completion :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRReadParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> IO ()
readAttributeElectricalCircuitNodesWithParams_completion mtrBaseClusterPowerTopology  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeElectricalCircuitNodesWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeElectricalCircuitNodesWithValue:completion:@
writeAttributeElectricalCircuitNodesWithValue_completion :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsNSArray value) => mtrBaseClusterPowerTopology -> value -> Ptr () -> IO ()
writeAttributeElectricalCircuitNodesWithValue_completion mtrBaseClusterPowerTopology  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "writeAttributeElectricalCircuitNodesWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeElectricalCircuitNodesWithValue:params:completion:@
writeAttributeElectricalCircuitNodesWithValue_params_completion :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsNSArray value, IsMTRWriteParams params) => mtrBaseClusterPowerTopology -> value -> params -> Ptr () -> IO ()
writeAttributeElectricalCircuitNodesWithValue_params_completion mtrBaseClusterPowerTopology  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterPowerTopology (mkSelector "writeAttributeElectricalCircuitNodesWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:@
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterPowerTopology  completion =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRSubscribeParams params) => mtrBaseClusterPowerTopology -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterPowerTopology  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterPowerTopology (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology => mtrBaseClusterPowerTopology -> IO (Id MTRBaseClusterPowerTopology)
init_ mtrBaseClusterPowerTopology  =
    sendMsg mtrBaseClusterPowerTopology (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterPowerTopology)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterPowerTopology"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterPowerTopology mtrBaseClusterPowerTopology, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterPowerTopology -> device -> endpointID -> queue -> IO (Id MTRBaseClusterPowerTopology)
initWithDevice_endpointID_queue mtrBaseClusterPowerTopology  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterPowerTopology (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAvailableEndpointsWithCompletion:@
readAttributeAvailableEndpointsWithCompletionSelector :: Selector
readAttributeAvailableEndpointsWithCompletionSelector = mkSelector "readAttributeAvailableEndpointsWithCompletion:"

-- | @Selector@ for @subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAvailableEndpointsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAvailableEndpointsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAvailableEndpointsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAvailableEndpointsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveEndpointsWithCompletion:@
readAttributeActiveEndpointsWithCompletionSelector :: Selector
readAttributeActiveEndpointsWithCompletionSelector = mkSelector "readAttributeActiveEndpointsWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveEndpointsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveEndpointsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveEndpointsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveEndpointsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeElectricalCircuitNodesWithParams:completion:@
readAttributeElectricalCircuitNodesWithParams_completionSelector :: Selector
readAttributeElectricalCircuitNodesWithParams_completionSelector = mkSelector "readAttributeElectricalCircuitNodesWithParams:completion:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:completion:@
writeAttributeElectricalCircuitNodesWithValue_completionSelector :: Selector
writeAttributeElectricalCircuitNodesWithValue_completionSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:completion:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:params:completion:@
writeAttributeElectricalCircuitNodesWithValue_params_completionSelector :: Selector
writeAttributeElectricalCircuitNodesWithValue_params_completionSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeElectricalCircuitNodesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeElectricalCircuitNodesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:@
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeElectricalCircuitNodesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeElectricalCircuitNodesWithClusterStateCache:endpoint:queue:completion:"

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

