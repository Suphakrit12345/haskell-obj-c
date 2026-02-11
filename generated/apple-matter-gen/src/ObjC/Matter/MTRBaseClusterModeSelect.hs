{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Mode Select
--
-- Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRBaseClusterModeSelect@.
module ObjC.Matter.MTRBaseClusterModeSelect
  ( MTRBaseClusterModeSelect
  , IsMTRBaseClusterModeSelect(..)
  , changeToModeWithParams_completion
  , readAttributeDescriptionWithCompletion
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion
  , readAttributeStandardNamespaceWithCompletion
  , subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandler
  , readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedModesWithCompletion
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentModeWithCompletion
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartUpModeWithCompletion
  , writeAttributeStartUpModeWithValue_completion
  , writeAttributeStartUpModeWithValue_params_completion
  , subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnModeWithCompletion
  , writeAttributeOnModeWithValue_completion
  , writeAttributeOnModeWithValue_params_completion
  , subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnModeWithClusterStateCache_endpoint_queue_completion
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
  , changeToModeWithParams_completionHandler
  , readAttributeDescriptionWithCompletionHandler
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStandardNamespaceWithCompletionHandler
  , subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedModesWithCompletionHandler
  , subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentModeWithCompletionHandler
  , subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartUpModeWithCompletionHandler
  , writeAttributeStartUpModeWithValue_completionHandler
  , writeAttributeStartUpModeWithValue_params_completionHandler
  , subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeOnModeWithCompletionHandler
  , writeAttributeOnModeWithValue_completionHandler
  , writeAttributeOnModeWithValue_params_completionHandler
  , subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandler
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
  , changeToModeWithParams_completionSelector
  , readAttributeDescriptionWithCompletionSelector
  , subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStandardNamespaceWithCompletionSelector
  , subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedModesWithCompletionSelector
  , subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentModeWithCompletionSelector
  , subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartUpModeWithCompletionSelector
  , writeAttributeStartUpModeWithValue_completionSelector
  , writeAttributeStartUpModeWithValue_params_completionSelector
  , subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnModeWithCompletionSelector
  , writeAttributeOnModeWithValue_completionSelector
  , writeAttributeOnModeWithValue_params_completionSelector
  , subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector
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
  , changeToModeWithParams_completionHandlerSelector
  , readAttributeDescriptionWithCompletionHandlerSelector
  , subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStandardNamespaceWithCompletionHandlerSelector
  , subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedModesWithCompletionHandlerSelector
  , subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentModeWithCompletionHandlerSelector
  , subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartUpModeWithCompletionHandlerSelector
  , writeAttributeStartUpModeWithValue_completionHandlerSelector
  , writeAttributeStartUpModeWithValue_params_completionHandlerSelector
  , subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeOnModeWithCompletionHandlerSelector
  , writeAttributeOnModeWithValue_completionHandlerSelector
  , writeAttributeOnModeWithValue_params_completionHandlerSelector
  , subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command ChangeToMode
--
-- On receipt of this command, if the NewMode field indicates a valid mode transition within the supported list, the server SHALL set the CurrentMode attribute to the NewMode value, otherwise, the server SHALL respond with an INVALID_COMMAND status response.
--
-- ObjC selector: @- changeToModeWithParams:completion:@
changeToModeWithParams_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> IO ()
changeToModeWithParams_completion mtrBaseClusterModeSelect  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "changeToModeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeDescriptionWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeDescriptionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStandardNamespaceWithCompletion:@
readAttributeStandardNamespaceWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStandardNamespaceWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeStandardNamespaceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:@
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeSupportedModesWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeSupportedModesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeCurrentModeWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeCurrentModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStartUpModeWithCompletion:@
readAttributeStartUpModeWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStartUpModeWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeStartUpModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeStartUpModeWithValue:completion:@
writeAttributeStartUpModeWithValue_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_completion mtrBaseClusterModeSelect  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeStartUpModeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeStartUpModeWithValue:params:completion:@
writeAttributeStartUpModeWithValue_params_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_params_completion mtrBaseClusterModeSelect  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeStartUpModeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnModeWithCompletion:@
readAttributeOnModeWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeOnModeWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeOnModeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnModeWithValue:completion:@
writeAttributeOnModeWithValue_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeOnModeWithValue_completion mtrBaseClusterModeSelect  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeOnModeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeOnModeWithValue:params:completion:@
writeAttributeOnModeWithValue_params_completion :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeOnModeWithValue_params_completion mtrBaseClusterModeSelect  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeOnModeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnModeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnModeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterModeSelect  completion =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> IO (Id MTRBaseClusterModeSelect)
init_ mtrBaseClusterModeSelect  =
    sendMsg mtrBaseClusterModeSelect (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterModeSelect)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterModeSelect -> device -> CUShort -> queue -> IO (Id MTRBaseClusterModeSelect)
initWithDevice_endpoint_queue mtrBaseClusterModeSelect  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterModeSelect (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- changeToModeWithParams:completionHandler:@
changeToModeWithParams_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params) => mtrBaseClusterModeSelect -> params -> Ptr () -> IO ()
changeToModeWithParams_completionHandler mtrBaseClusterModeSelect  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "changeToModeWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeDescriptionWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeDescriptionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStandardNamespaceWithCompletionHandler:@
readAttributeStandardNamespaceWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStandardNamespaceWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeStandardNamespaceWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSupportedModesWithCompletionHandler:@
readAttributeSupportedModesWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeSupportedModesWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeSupportedModesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentModeWithCompletionHandler:@
readAttributeCurrentModeWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeCurrentModeWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeCurrentModeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStartUpModeWithCompletionHandler:@
readAttributeStartUpModeWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeStartUpModeWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeStartUpModeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeStartUpModeWithValue:completionHandler:@
writeAttributeStartUpModeWithValue_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_completionHandler mtrBaseClusterModeSelect  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeStartUpModeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeStartUpModeWithValue:params:completionHandler:@
writeAttributeStartUpModeWithValue_params_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeStartUpModeWithValue_params_completionHandler mtrBaseClusterModeSelect  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeStartUpModeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeOnModeWithCompletionHandler:@
readAttributeOnModeWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeOnModeWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeOnModeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnModeWithValue:completionHandler:@
writeAttributeOnModeWithValue_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value) => mtrBaseClusterModeSelect -> value -> Ptr () -> IO ()
writeAttributeOnModeWithValue_completionHandler mtrBaseClusterModeSelect  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeOnModeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeOnModeWithValue:params:completionHandler:@
writeAttributeOnModeWithValue_params_completionHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterModeSelect -> value -> params -> Ptr () -> IO ()
writeAttributeOnModeWithValue_params_completionHandler mtrBaseClusterModeSelect  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterModeSelect (mkSelector "writeAttributeOnModeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect => mtrBaseClusterModeSelect -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterModeSelect  completionHandler =
    sendMsg mtrBaseClusterModeSelect (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterModeSelect -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterModeSelect  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterModeSelect"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterModeSelect mtrBaseClusterModeSelect, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterModeSelect -> device -> endpointID -> queue -> IO (Id MTRBaseClusterModeSelect)
initWithDevice_endpointID_queue mtrBaseClusterModeSelect  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterModeSelect (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:completion:@
changeToModeWithParams_completionSelector :: Selector
changeToModeWithParams_completionSelector = mkSelector "changeToModeWithParams:completion:"

-- | @Selector@ for @readAttributeDescriptionWithCompletion:@
readAttributeDescriptionWithCompletionSelector :: Selector
readAttributeDescriptionWithCompletionSelector = mkSelector "readAttributeDescriptionWithCompletion:"

-- | @Selector@ for @subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDescriptionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDescriptionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDescriptionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStandardNamespaceWithCompletion:@
readAttributeStandardNamespaceWithCompletionSelector :: Selector
readAttributeStandardNamespaceWithCompletionSelector = mkSelector "readAttributeStandardNamespaceWithCompletion:"

-- | @Selector@ for @subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStandardNamespaceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStandardNamespaceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:@
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStandardNamespaceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStandardNamespaceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithCompletion:@
readAttributeSupportedModesWithCompletionSelector :: Selector
readAttributeSupportedModesWithCompletionSelector = mkSelector "readAttributeSupportedModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedModesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentModeWithCompletion:@
readAttributeCurrentModeWithCompletionSelector :: Selector
readAttributeCurrentModeWithCompletionSelector = mkSelector "readAttributeCurrentModeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartUpModeWithCompletion:@
readAttributeStartUpModeWithCompletionSelector :: Selector
readAttributeStartUpModeWithCompletionSelector = mkSelector "readAttributeStartUpModeWithCompletion:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:completion:@
writeAttributeStartUpModeWithValue_completionSelector :: Selector
writeAttributeStartUpModeWithValue_completionSelector = mkSelector "writeAttributeStartUpModeWithValue:completion:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:params:completion:@
writeAttributeStartUpModeWithValue_params_completionSelector :: Selector
writeAttributeStartUpModeWithValue_params_completionSelector = mkSelector "writeAttributeStartUpModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartUpModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStartUpModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartUpModeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnModeWithCompletion:@
readAttributeOnModeWithCompletionSelector :: Selector
readAttributeOnModeWithCompletionSelector = mkSelector "readAttributeOnModeWithCompletion:"

-- | @Selector@ for @writeAttributeOnModeWithValue:completion:@
writeAttributeOnModeWithValue_completionSelector :: Selector
writeAttributeOnModeWithValue_completionSelector = mkSelector "writeAttributeOnModeWithValue:completion:"

-- | @Selector@ for @writeAttributeOnModeWithValue:params:completion:@
writeAttributeOnModeWithValue_params_completionSelector :: Selector
writeAttributeOnModeWithValue_params_completionSelector = mkSelector "writeAttributeOnModeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnModeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnModeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnModeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnModeWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @changeToModeWithParams:completionHandler:@
changeToModeWithParams_completionHandlerSelector :: Selector
changeToModeWithParams_completionHandlerSelector = mkSelector "changeToModeWithParams:completionHandler:"

-- | @Selector@ for @readAttributeDescriptionWithCompletionHandler:@
readAttributeDescriptionWithCompletionHandlerSelector :: Selector
readAttributeDescriptionWithCompletionHandlerSelector = mkSelector "readAttributeDescriptionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDescriptionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDescriptionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeDescriptionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDescriptionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStandardNamespaceWithCompletionHandler:@
readAttributeStandardNamespaceWithCompletionHandlerSelector :: Selector
readAttributeStandardNamespaceWithCompletionHandlerSelector = mkSelector "readAttributeStandardNamespaceWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStandardNamespaceWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStandardNamespaceWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStandardNamespaceWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStandardNamespaceWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithCompletionHandler:@
readAttributeSupportedModesWithCompletionHandlerSelector :: Selector
readAttributeSupportedModesWithCompletionHandlerSelector = mkSelector "readAttributeSupportedModesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedModesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedModesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSupportedModesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedModesWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithCompletionHandler:@
readAttributeCurrentModeWithCompletionHandlerSelector :: Selector
readAttributeCurrentModeWithCompletionHandlerSelector = mkSelector "readAttributeCurrentModeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartUpModeWithCompletionHandler:@
readAttributeStartUpModeWithCompletionHandlerSelector :: Selector
readAttributeStartUpModeWithCompletionHandlerSelector = mkSelector "readAttributeStartUpModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:completionHandler:@
writeAttributeStartUpModeWithValue_completionHandlerSelector :: Selector
writeAttributeStartUpModeWithValue_completionHandlerSelector = mkSelector "writeAttributeStartUpModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:params:completionHandler:@
writeAttributeStartUpModeWithValue_params_completionHandlerSelector :: Selector
writeAttributeStartUpModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeStartUpModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartUpModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartUpModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStartUpModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartUpModeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeOnModeWithCompletionHandler:@
readAttributeOnModeWithCompletionHandlerSelector :: Selector
readAttributeOnModeWithCompletionHandlerSelector = mkSelector "readAttributeOnModeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeOnModeWithValue:completionHandler:@
writeAttributeOnModeWithValue_completionHandlerSelector :: Selector
writeAttributeOnModeWithValue_completionHandlerSelector = mkSelector "writeAttributeOnModeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeOnModeWithValue:params:completionHandler:@
writeAttributeOnModeWithValue_params_completionHandlerSelector :: Selector
writeAttributeOnModeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeOnModeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnModeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnModeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeOnModeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeOnModeWithAttributeCache:endpoint:queue:completionHandler:"

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

