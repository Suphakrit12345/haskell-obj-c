{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Basic
--
-- This cluster provides information about an application running on a TV or media player device which is represented as an endpoint.
--
-- Generated bindings for @MTRBaseClusterApplicationBasic@.
module ObjC.Matter.MTRBaseClusterApplicationBasic
  ( MTRBaseClusterApplicationBasic
  , IsMTRBaseClusterApplicationBasic(..)
  , readAttributeVendorNameWithCompletion
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeVendorIDWithCompletion
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationNameWithCompletion
  , subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductIDWithCompletion
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationWithCompletion
  , subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationWithClusterStateCache_endpoint_queue_completion
  , readAttributeStatusWithCompletion
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeApplicationVersionWithCompletion
  , subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeAllowedVendorListWithCompletion
  , subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeVendorNameWithCompletionHandler
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeVendorIDWithCompletionHandler
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationNameWithCompletionHandler
  , subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductIDWithCompletionHandler
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationWithCompletionHandler
  , subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStatusWithCompletionHandler
  , subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeApplicationVersionWithCompletionHandler
  , subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAllowedVendorListWithCompletionHandler
  , subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeVendorNameWithCompletionSelector
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorIDWithCompletionSelector
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationNameWithCompletionSelector
  , subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductIDWithCompletionSelector
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationWithCompletionSelector
  , subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStatusWithCompletionSelector
  , subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeApplicationVersionWithCompletionSelector
  , subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAllowedVendorListWithCompletionSelector
  , subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeVendorNameWithCompletionHandlerSelector
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorIDWithCompletionHandlerSelector
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationNameWithCompletionHandlerSelector
  , subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductIDWithCompletionHandlerSelector
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationWithCompletionHandlerSelector
  , subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStatusWithCompletionHandlerSelector
  , subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeApplicationVersionWithCompletionHandlerSelector
  , subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAllowedVendorListWithCompletionHandlerSelector
  , subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorNameWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeVendorNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorIDWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeVendorIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApplicationNameWithCompletion:@
readAttributeApplicationNameWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationNameWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeApplicationNameWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeProductIDWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeProductIDWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApplicationWithCompletion:@
readAttributeApplicationWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeApplicationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeStatusWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeStatusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStatusWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeApplicationVersionWithCompletion:@
readAttributeApplicationVersionWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationVersionWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeApplicationVersionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAllowedVendorListWithCompletion:@
readAttributeAllowedVendorListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAllowedVendorListWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeAllowedVendorListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterApplicationBasic  completion =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> IO (Id MTRBaseClusterApplicationBasic)
init_ mtrBaseClusterApplicationBasic  =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterApplicationBasic)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterApplicationBasic -> device -> CUShort -> queue -> IO (Id MTRBaseClusterApplicationBasic)
initWithDevice_endpoint_queue mtrBaseClusterApplicationBasic  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterApplicationBasic (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorNameWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeVendorNameWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeVendorIDWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeVendorIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeApplicationNameWithCompletionHandler:@
readAttributeApplicationNameWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationNameWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeApplicationNameWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeProductIDWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeProductIDWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeApplicationWithCompletionHandler:@
readAttributeApplicationWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeApplicationWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStatusWithCompletionHandler:@
readAttributeStatusWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeStatusWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeStatusWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeApplicationVersionWithCompletionHandler:@
readAttributeApplicationVersionWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeApplicationVersionWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeApplicationVersionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAllowedVendorListWithCompletionHandler:@
readAttributeAllowedVendorListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAllowedVendorListWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeAllowedVendorListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic => mtrBaseClusterApplicationBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterApplicationBasic  completionHandler =
    sendMsg mtrBaseClusterApplicationBasic (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationBasic  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationBasic"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterApplicationBasic mtrBaseClusterApplicationBasic, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterApplicationBasic -> device -> endpointID -> queue -> IO (Id MTRBaseClusterApplicationBasic)
initWithDevice_endpointID_queue mtrBaseClusterApplicationBasic  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterApplicationBasic (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletionSelector :: Selector
readAttributeVendorNameWithCompletionSelector = mkSelector "readAttributeVendorNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletionSelector :: Selector
readAttributeVendorIDWithCompletionSelector = mkSelector "readAttributeVendorIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationNameWithCompletion:@
readAttributeApplicationNameWithCompletionSelector :: Selector
readAttributeApplicationNameWithCompletionSelector = mkSelector "readAttributeApplicationNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApplicationNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletionSelector :: Selector
readAttributeProductIDWithCompletionSelector = mkSelector "readAttributeProductIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationWithCompletion:@
readAttributeApplicationWithCompletionSelector :: Selector
readAttributeApplicationWithCompletionSelector = mkSelector "readAttributeApplicationWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApplicationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStatusWithCompletion:@
readAttributeStatusWithCompletionSelector :: Selector
readAttributeStatusWithCompletionSelector = mkSelector "readAttributeStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeApplicationVersionWithCompletion:@
readAttributeApplicationVersionWithCompletionSelector :: Selector
readAttributeApplicationVersionWithCompletionSelector = mkSelector "readAttributeApplicationVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeApplicationVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeApplicationVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAllowedVendorListWithCompletion:@
readAttributeAllowedVendorListWithCompletionSelector :: Selector
readAttributeAllowedVendorListWithCompletionSelector = mkSelector "readAttributeAllowedVendorListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAllowedVendorListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAllowedVendorListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAllowedVendorListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAllowedVendorListWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandlerSelector :: Selector
readAttributeVendorNameWithCompletionHandlerSelector = mkSelector "readAttributeVendorNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandlerSelector :: Selector
readAttributeVendorIDWithCompletionHandlerSelector = mkSelector "readAttributeVendorIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationNameWithCompletionHandler:@
readAttributeApplicationNameWithCompletionHandlerSelector :: Selector
readAttributeApplicationNameWithCompletionHandlerSelector = mkSelector "readAttributeApplicationNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeApplicationNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandlerSelector :: Selector
readAttributeProductIDWithCompletionHandlerSelector = mkSelector "readAttributeProductIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationWithCompletionHandler:@
readAttributeApplicationWithCompletionHandlerSelector :: Selector
readAttributeApplicationWithCompletionHandlerSelector = mkSelector "readAttributeApplicationWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeApplicationWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStatusWithCompletionHandler:@
readAttributeStatusWithCompletionHandlerSelector :: Selector
readAttributeStatusWithCompletionHandlerSelector = mkSelector "readAttributeStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeApplicationVersionWithCompletionHandler:@
readAttributeApplicationVersionWithCompletionHandlerSelector :: Selector
readAttributeApplicationVersionWithCompletionHandlerSelector = mkSelector "readAttributeApplicationVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeApplicationVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeApplicationVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeApplicationVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeApplicationVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAllowedVendorListWithCompletionHandler:@
readAttributeAllowedVendorListWithCompletionHandlerSelector :: Selector
readAttributeAllowedVendorListWithCompletionHandlerSelector = mkSelector "readAttributeAllowedVendorListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAllowedVendorListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAllowedVendorListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeAllowedVendorListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAllowedVendorListWithAttributeCache:endpoint:queue:completionHandler:"

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

