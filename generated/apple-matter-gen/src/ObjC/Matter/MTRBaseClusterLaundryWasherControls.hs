{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Washer Controls
--
-- This cluster supports remotely monitoring and controlling the different types of functionality available to a washing device, such as a washing machine.
--
-- Generated bindings for @MTRBaseClusterLaundryWasherControls@.
module ObjC.Matter.MTRBaseClusterLaundryWasherControls
  ( MTRBaseClusterLaundryWasherControls
  , IsMTRBaseClusterLaundryWasherControls(..)
  , readAttributeSpinSpeedsWithCompletion
  , subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpinSpeedCurrentWithCompletion
  , writeAttributeSpinSpeedCurrentWithValue_completion
  , writeAttributeSpinSpeedCurrentWithValue_params_completion
  , subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completion
  , readAttributeNumberOfRinsesWithCompletion
  , writeAttributeNumberOfRinsesWithValue_completion
  , writeAttributeNumberOfRinsesWithValue_params_completion
  , subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandler
  , readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedRinsesWithCompletion
  , subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeSpinSpeedsWithCompletionSelector
  , subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpinSpeedCurrentWithCompletionSelector
  , writeAttributeSpinSpeedCurrentWithValue_completionSelector
  , writeAttributeSpinSpeedCurrentWithValue_params_completionSelector
  , subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNumberOfRinsesWithCompletionSelector
  , writeAttributeNumberOfRinsesWithValue_completionSelector
  , writeAttributeNumberOfRinsesWithValue_params_completionSelector
  , subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedRinsesWithCompletionSelector
  , subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeSpinSpeedsWithCompletion:@
readAttributeSpinSpeedsWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeSpinSpeedsWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeSpinSpeedsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSpinSpeedCurrentWithCompletion:@
readAttributeSpinSpeedCurrentWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeSpinSpeedCurrentWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeSpinSpeedCurrentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSpinSpeedCurrentWithValue:completion:@
writeAttributeSpinSpeedCurrentWithValue_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value) => mtrBaseClusterLaundryWasherControls -> value -> Ptr () -> IO ()
writeAttributeSpinSpeedCurrentWithValue_completion mtrBaseClusterLaundryWasherControls  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "writeAttributeSpinSpeedCurrentWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSpinSpeedCurrentWithValue:params:completion:@
writeAttributeSpinSpeedCurrentWithValue_params_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLaundryWasherControls -> value -> params -> Ptr () -> IO ()
writeAttributeSpinSpeedCurrentWithValue_params_completion mtrBaseClusterLaundryWasherControls  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "writeAttributeSpinSpeedCurrentWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNumberOfRinsesWithCompletion:@
readAttributeNumberOfRinsesWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeNumberOfRinsesWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeNumberOfRinsesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeNumberOfRinsesWithValue:completion:@
writeAttributeNumberOfRinsesWithValue_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value) => mtrBaseClusterLaundryWasherControls -> value -> Ptr () -> IO ()
writeAttributeNumberOfRinsesWithValue_completion mtrBaseClusterLaundryWasherControls  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "writeAttributeNumberOfRinsesWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeNumberOfRinsesWithValue:params:completion:@
writeAttributeNumberOfRinsesWithValue_params_completion :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLaundryWasherControls -> value -> params -> Ptr () -> IO ()
writeAttributeNumberOfRinsesWithValue_params_completion mtrBaseClusterLaundryWasherControls  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "writeAttributeNumberOfRinsesWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedRinsesWithCompletion:@
readAttributeSupportedRinsesWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeSupportedRinsesWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeSupportedRinsesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterLaundryWasherControls  completion =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryWasherControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryWasherControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls => mtrBaseClusterLaundryWasherControls -> IO (Id MTRBaseClusterLaundryWasherControls)
init_ mtrBaseClusterLaundryWasherControls  =
    sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterLaundryWasherControls)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryWasherControls"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterLaundryWasherControls mtrBaseClusterLaundryWasherControls, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterLaundryWasherControls -> device -> endpointID -> queue -> IO (Id MTRBaseClusterLaundryWasherControls)
initWithDevice_endpointID_queue mtrBaseClusterLaundryWasherControls  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterLaundryWasherControls (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSpinSpeedsWithCompletion:@
readAttributeSpinSpeedsWithCompletionSelector :: Selector
readAttributeSpinSpeedsWithCompletionSelector = mkSelector "readAttributeSpinSpeedsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpinSpeedsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpinSpeedsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSpinSpeedsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpinSpeedsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpinSpeedCurrentWithCompletion:@
readAttributeSpinSpeedCurrentWithCompletionSelector :: Selector
readAttributeSpinSpeedCurrentWithCompletionSelector = mkSelector "readAttributeSpinSpeedCurrentWithCompletion:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:completion:@
writeAttributeSpinSpeedCurrentWithValue_completionSelector :: Selector
writeAttributeSpinSpeedCurrentWithValue_completionSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:completion:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:params:completion:@
writeAttributeSpinSpeedCurrentWithValue_params_completionSelector :: Selector
writeAttributeSpinSpeedCurrentWithValue_params_completionSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSpinSpeedCurrentWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpinSpeedCurrentWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSpinSpeedCurrentWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpinSpeedCurrentWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNumberOfRinsesWithCompletion:@
readAttributeNumberOfRinsesWithCompletionSelector :: Selector
readAttributeNumberOfRinsesWithCompletionSelector = mkSelector "readAttributeNumberOfRinsesWithCompletion:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:completion:@
writeAttributeNumberOfRinsesWithValue_completionSelector :: Selector
writeAttributeNumberOfRinsesWithValue_completionSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:completion:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:params:completion:@
writeAttributeNumberOfRinsesWithValue_params_completionSelector :: Selector
writeAttributeNumberOfRinsesWithValue_params_completionSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNumberOfRinsesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNumberOfRinsesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNumberOfRinsesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNumberOfRinsesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedRinsesWithCompletion:@
readAttributeSupportedRinsesWithCompletionSelector :: Selector
readAttributeSupportedRinsesWithCompletionSelector = mkSelector "readAttributeSupportedRinsesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedRinsesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedRinsesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedRinsesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedRinsesWithClusterStateCache:endpoint:queue:completion:"

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

