{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Software Diagnostics
--
-- The Software Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRBaseClusterSoftwareDiagnostics@.
module ObjC.Matter.MTRBaseClusterSoftwareDiagnostics
  ( MTRBaseClusterSoftwareDiagnostics
  , IsMTRBaseClusterSoftwareDiagnostics(..)
  , resetWatermarksWithParams_completion
  , resetWatermarksWithCompletion
  , readAttributeThreadMetricsWithCompletion
  , subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentHeapFreeWithCompletion
  , subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentHeapUsedWithCompletion
  , subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentHeapHighWatermarkWithCompletion
  , subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completion
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
  , resetWatermarksWithParams_completionHandler
  , resetWatermarksWithCompletionHandler
  , readAttributeThreadMetricsWithCompletionHandler
  , subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentHeapFreeWithCompletionHandler
  , subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentHeapUsedWithCompletionHandler
  , subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentHeapHighWatermarkWithCompletionHandler
  , subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandler
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
  , resetWatermarksWithParams_completionSelector
  , resetWatermarksWithCompletionSelector
  , readAttributeThreadMetricsWithCompletionSelector
  , subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentHeapFreeWithCompletionSelector
  , subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentHeapUsedWithCompletionSelector
  , subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentHeapHighWatermarkWithCompletionSelector
  , subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector
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
  , resetWatermarksWithParams_completionHandlerSelector
  , resetWatermarksWithCompletionHandlerSelector
  , readAttributeThreadMetricsWithCompletionHandlerSelector
  , subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentHeapFreeWithCompletionHandlerSelector
  , subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentHeapUsedWithCompletionHandlerSelector
  , subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector
  , subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command ResetWatermarks
--
-- This command is used to reset the high watermarks for heap and stack memory.
--
-- ObjC selector: @- resetWatermarksWithParams:completion:@
resetWatermarksWithParams_completion :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSoftwareDiagnosticsClusterResetWatermarksParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> IO ()
resetWatermarksWithParams_completion mtrBaseClusterSoftwareDiagnostics  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetWatermarksWithCompletion:@
resetWatermarksWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
resetWatermarksWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeThreadMetricsWithCompletion:@
readAttributeThreadMetricsWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeThreadMetricsWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeThreadMetricsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentHeapFreeWithCompletion:@
readAttributeCurrentHeapFreeWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapFreeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentHeapUsedWithCompletion:@
readAttributeCurrentHeapUsedWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapUsedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentHeapHighWatermarkWithCompletion:@
readAttributeCurrentHeapHighWatermarkWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapHighWatermarkWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterSoftwareDiagnostics  completion =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> IO (Id MTRBaseClusterSoftwareDiagnostics)
init_ mtrBaseClusterSoftwareDiagnostics  =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterSoftwareDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterSoftwareDiagnostics -> device -> CUShort -> queue -> IO (Id MTRBaseClusterSoftwareDiagnostics)
initWithDevice_endpoint_queue mtrBaseClusterSoftwareDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetWatermarksWithParams:completionHandler:@
resetWatermarksWithParams_completionHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRSoftwareDiagnosticsClusterResetWatermarksParams params) => mtrBaseClusterSoftwareDiagnostics -> params -> Ptr () -> IO ()
resetWatermarksWithParams_completionHandler mtrBaseClusterSoftwareDiagnostics  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetWatermarksWithCompletionHandler:@
resetWatermarksWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
resetWatermarksWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "resetWatermarksWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeThreadMetricsWithCompletionHandler:@
readAttributeThreadMetricsWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeThreadMetricsWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeThreadMetricsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentHeapFreeWithCompletionHandler:@
readAttributeCurrentHeapFreeWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapFreeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentHeapUsedWithCompletionHandler:@
readAttributeCurrentHeapUsedWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapUsedWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentHeapHighWatermarkWithCompletionHandler:@
readAttributeCurrentHeapHighWatermarkWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeCurrentHeapHighWatermarkWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics => mtrBaseClusterSoftwareDiagnostics -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterSoftwareDiagnostics  completionHandler =
    sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterSoftwareDiagnostics -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterSoftwareDiagnostics  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterSoftwareDiagnostics"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterSoftwareDiagnostics mtrBaseClusterSoftwareDiagnostics, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterSoftwareDiagnostics -> device -> endpointID -> queue -> IO (Id MTRBaseClusterSoftwareDiagnostics)
initWithDevice_endpointID_queue mtrBaseClusterSoftwareDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterSoftwareDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetWatermarksWithParams:completion:@
resetWatermarksWithParams_completionSelector :: Selector
resetWatermarksWithParams_completionSelector = mkSelector "resetWatermarksWithParams:completion:"

-- | @Selector@ for @resetWatermarksWithCompletion:@
resetWatermarksWithCompletionSelector :: Selector
resetWatermarksWithCompletionSelector = mkSelector "resetWatermarksWithCompletion:"

-- | @Selector@ for @readAttributeThreadMetricsWithCompletion:@
readAttributeThreadMetricsWithCompletionSelector :: Selector
readAttributeThreadMetricsWithCompletionSelector = mkSelector "readAttributeThreadMetricsWithCompletion:"

-- | @Selector@ for @subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeThreadMetricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadMetricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeThreadMetricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeThreadMetricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithCompletion:@
readAttributeCurrentHeapFreeWithCompletionSelector :: Selector
readAttributeCurrentHeapFreeWithCompletionSelector = mkSelector "readAttributeCurrentHeapFreeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentHeapFreeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapFreeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentHeapFreeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentHeapFreeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithCompletion:@
readAttributeCurrentHeapUsedWithCompletionSelector :: Selector
readAttributeCurrentHeapUsedWithCompletionSelector = mkSelector "readAttributeCurrentHeapUsedWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentHeapUsedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapUsedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentHeapUsedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentHeapUsedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithCompletion:@
readAttributeCurrentHeapHighWatermarkWithCompletionSelector :: Selector
readAttributeCurrentHeapHighWatermarkWithCompletionSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentHeapHighWatermarkWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapHighWatermarkWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentHeapHighWatermarkWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @resetWatermarksWithParams:completionHandler:@
resetWatermarksWithParams_completionHandlerSelector :: Selector
resetWatermarksWithParams_completionHandlerSelector = mkSelector "resetWatermarksWithParams:completionHandler:"

-- | @Selector@ for @resetWatermarksWithCompletionHandler:@
resetWatermarksWithCompletionHandlerSelector :: Selector
resetWatermarksWithCompletionHandlerSelector = mkSelector "resetWatermarksWithCompletionHandler:"

-- | @Selector@ for @readAttributeThreadMetricsWithCompletionHandler:@
readAttributeThreadMetricsWithCompletionHandlerSelector :: Selector
readAttributeThreadMetricsWithCompletionHandlerSelector = mkSelector "readAttributeThreadMetricsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeThreadMetricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeThreadMetricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeThreadMetricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeThreadMetricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithCompletionHandler:@
readAttributeCurrentHeapFreeWithCompletionHandlerSelector :: Selector
readAttributeCurrentHeapFreeWithCompletionHandlerSelector = mkSelector "readAttributeCurrentHeapFreeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentHeapFreeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapFreeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentHeapFreeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentHeapFreeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithCompletionHandler:@
readAttributeCurrentHeapUsedWithCompletionHandlerSelector :: Selector
readAttributeCurrentHeapUsedWithCompletionHandlerSelector = mkSelector "readAttributeCurrentHeapUsedWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentHeapUsedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapUsedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentHeapUsedWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentHeapUsedWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithCompletionHandler:@
readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector :: Selector
readAttributeCurrentHeapHighWatermarkWithCompletionHandlerSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentHeapHighWatermarkWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentHeapHighWatermarkWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentHeapHighWatermarkWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentHeapHighWatermarkWithAttributeCache:endpoint:queue:completionHandler:"

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

