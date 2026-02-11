{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Network Directory
--
-- Manages the names and credentials of Thread networks visible to the user.
--
-- Generated bindings for @MTRBaseClusterThreadNetworkDirectory@.
module ObjC.Matter.MTRBaseClusterThreadNetworkDirectory
  ( MTRBaseClusterThreadNetworkDirectory
  , IsMTRBaseClusterThreadNetworkDirectory(..)
  , addNetworkWithParams_completion
  , removeNetworkWithParams_completion
  , getOperationalDatasetWithParams_completion
  , readAttributePreferredExtendedPanIDWithCompletion
  , writeAttributePreferredExtendedPanIDWithValue_completion
  , writeAttributePreferredExtendedPanIDWithValue_params_completion
  , subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandler
  , readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadNetworksWithCompletion
  , subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completion
  , readAttributeThreadNetworkTableSizeWithCompletion
  , subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completion
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
  , addNetworkWithParams_completionSelector
  , removeNetworkWithParams_completionSelector
  , getOperationalDatasetWithParams_completionSelector
  , readAttributePreferredExtendedPanIDWithCompletionSelector
  , writeAttributePreferredExtendedPanIDWithValue_completionSelector
  , writeAttributePreferredExtendedPanIDWithValue_params_completionSelector
  , subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadNetworksWithCompletionSelector
  , subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeThreadNetworkTableSizeWithCompletionSelector
  , subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command AddNetwork
--
-- Adds an entry to the ThreadNetworks attribute with the specified Thread Operational Dataset.
--
-- ObjC selector: @- addNetworkWithParams:completion:@
addNetworkWithParams_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterAddNetworkParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> IO ()
addNetworkWithParams_completion mtrBaseClusterThreadNetworkDirectory  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "addNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command RemoveNetwork
--
-- Removes the network with the given Extended PAN ID from the ThreadNetworks attribute.
--
-- ObjC selector: @- removeNetworkWithParams:completion:@
removeNetworkWithParams_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> IO ()
removeNetworkWithParams_completion mtrBaseClusterThreadNetworkDirectory  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "removeNetworkWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command GetOperationalDataset
--
-- Retrieves the Thread Operational Dataset with the given Extended PAN ID.
--
-- ObjC selector: @- getOperationalDatasetWithParams:completion:@
getOperationalDatasetWithParams_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterGetOperationalDatasetParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> IO ()
getOperationalDatasetWithParams_completion mtrBaseClusterThreadNetworkDirectory  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "getOperationalDatasetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePreferredExtendedPanIDWithCompletion:@
readAttributePreferredExtendedPanIDWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributePreferredExtendedPanIDWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributePreferredExtendedPanIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributePreferredExtendedPanIDWithValue:completion:@
writeAttributePreferredExtendedPanIDWithValue_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsNSData value) => mtrBaseClusterThreadNetworkDirectory -> value -> Ptr () -> IO ()
writeAttributePreferredExtendedPanIDWithValue_completion mtrBaseClusterThreadNetworkDirectory  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "writeAttributePreferredExtendedPanIDWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributePreferredExtendedPanIDWithValue:params:completion:@
writeAttributePreferredExtendedPanIDWithValue_params_completion :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsNSData value, IsMTRWriteParams params) => mtrBaseClusterThreadNetworkDirectory -> value -> params -> Ptr () -> IO ()
writeAttributePreferredExtendedPanIDWithValue_params_completion mtrBaseClusterThreadNetworkDirectory  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "writeAttributePreferredExtendedPanIDWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:@
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeThreadNetworksWithCompletion:@
readAttributeThreadNetworksWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeThreadNetworksWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeThreadNetworksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeThreadNetworkTableSizeWithCompletion:@
readAttributeThreadNetworkTableSizeWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeThreadNetworkTableSizeWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeThreadNetworkTableSizeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterThreadNetworkDirectory  completion =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRSubscribeParams params) => mtrBaseClusterThreadNetworkDirectory -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterThreadNetworkDirectory  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory => mtrBaseClusterThreadNetworkDirectory -> IO (Id MTRBaseClusterThreadNetworkDirectory)
init_ mtrBaseClusterThreadNetworkDirectory  =
    sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterThreadNetworkDirectory)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterThreadNetworkDirectory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterThreadNetworkDirectory mtrBaseClusterThreadNetworkDirectory, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterThreadNetworkDirectory -> device -> endpointID -> queue -> IO (Id MTRBaseClusterThreadNetworkDirectory)
initWithDevice_endpointID_queue mtrBaseClusterThreadNetworkDirectory  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterThreadNetworkDirectory (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addNetworkWithParams:completion:@
addNetworkWithParams_completionSelector :: Selector
addNetworkWithParams_completionSelector = mkSelector "addNetworkWithParams:completion:"

-- | @Selector@ for @removeNetworkWithParams:completion:@
removeNetworkWithParams_completionSelector :: Selector
removeNetworkWithParams_completionSelector = mkSelector "removeNetworkWithParams:completion:"

-- | @Selector@ for @getOperationalDatasetWithParams:completion:@
getOperationalDatasetWithParams_completionSelector :: Selector
getOperationalDatasetWithParams_completionSelector = mkSelector "getOperationalDatasetWithParams:completion:"

-- | @Selector@ for @readAttributePreferredExtendedPanIDWithCompletion:@
readAttributePreferredExtendedPanIDWithCompletionSelector :: Selector
readAttributePreferredExtendedPanIDWithCompletionSelector = mkSelector "readAttributePreferredExtendedPanIDWithCompletion:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:completion:@
writeAttributePreferredExtendedPanIDWithValue_completionSelector :: Selector
writeAttributePreferredExtendedPanIDWithValue_completionSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:completion:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:params:completion:@
writeAttributePreferredExtendedPanIDWithValue_params_completionSelector :: Selector
writeAttributePreferredExtendedPanIDWithValue_params_completionSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePreferredExtendedPanIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePreferredExtendedPanIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:@
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePreferredExtendedPanIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePreferredExtendedPanIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadNetworksWithCompletion:@
readAttributeThreadNetworksWithCompletionSelector :: Selector
readAttributeThreadNetworksWithCompletionSelector = mkSelector "readAttributeThreadNetworksWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeThreadNetworksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadNetworksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeThreadNetworksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadNetworksWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeThreadNetworkTableSizeWithCompletion:@
readAttributeThreadNetworkTableSizeWithCompletionSelector :: Selector
readAttributeThreadNetworkTableSizeWithCompletionSelector = mkSelector "readAttributeThreadNetworkTableSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeThreadNetworkTableSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadNetworkTableSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeThreadNetworkTableSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadNetworkTableSizeWithClusterStateCache:endpoint:queue:completion:"

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

