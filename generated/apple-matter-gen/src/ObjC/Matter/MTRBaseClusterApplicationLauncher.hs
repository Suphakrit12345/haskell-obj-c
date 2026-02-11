{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Launcher
--
-- This cluster provides an interface for launching content on a media player device such as a TV or Speaker.
--
-- Generated bindings for @MTRBaseClusterApplicationLauncher@.
module ObjC.Matter.MTRBaseClusterApplicationLauncher
  ( MTRBaseClusterApplicationLauncher
  , IsMTRBaseClusterApplicationLauncher(..)
  , launchAppWithParams_completion
  , launchAppWithCompletion
  , stopAppWithParams_completion
  , stopAppWithCompletion
  , hideAppWithParams_completion
  , hideAppWithCompletion
  , readAttributeCatalogListWithCompletion
  , subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandler
  , readAttributeCatalogListWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentAppWithCompletion
  , writeAttributeCurrentAppWithValue_completion
  , writeAttributeCurrentAppWithValue_params_completion
  , subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completion
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
  , launchAppWithParams_completionHandler
  , stopAppWithParams_completionHandler
  , hideAppWithParams_completionHandler
  , readAttributeCatalogListWithCompletionHandler
  , subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentAppWithCompletionHandler
  , writeAttributeCurrentAppWithValue_completionHandler
  , writeAttributeCurrentAppWithValue_params_completionHandler
  , subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandler
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
  , launchAppWithParams_completionSelector
  , launchAppWithCompletionSelector
  , stopAppWithParams_completionSelector
  , stopAppWithCompletionSelector
  , hideAppWithParams_completionSelector
  , hideAppWithCompletionSelector
  , readAttributeCatalogListWithCompletionSelector
  , subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentAppWithCompletionSelector
  , writeAttributeCurrentAppWithValue_completionSelector
  , writeAttributeCurrentAppWithValue_params_completionSelector
  , subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector
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
  , launchAppWithParams_completionHandlerSelector
  , stopAppWithParams_completionHandlerSelector
  , hideAppWithParams_completionHandlerSelector
  , readAttributeCatalogListWithCompletionHandlerSelector
  , subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentAppWithCompletionHandlerSelector
  , writeAttributeCurrentAppWithValue_completionHandlerSelector
  , writeAttributeCurrentAppWithValue_params_completionHandlerSelector
  , subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command LaunchApp
--
-- Upon receipt, this SHALL launch the specified app with optional data. The TV Device SHALL launch and bring to foreground the identified application in the command if the application is not already launched and in foreground. The TV Device SHALL update state attribute on the Application Basic cluster of the Endpoint corresponding to the launched application. This command returns a Launch Response.
--
-- ObjC selector: @- launchAppWithParams:completion:@
launchAppWithParams_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
launchAppWithParams_completion mtrBaseClusterApplicationLauncher  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "launchAppWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- launchAppWithCompletion:@
launchAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
launchAppWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "launchAppWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command StopApp
--
-- Upon receipt on a Video Player endpoint this SHALL stop the specified application if it is running.
--
-- ObjC selector: @- stopAppWithParams:completion:@
stopAppWithParams_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
stopAppWithParams_completion mtrBaseClusterApplicationLauncher  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "stopAppWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopAppWithCompletion:@
stopAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
stopAppWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "stopAppWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command HideApp
--
-- Upon receipt on a Video Player endpoint this SHALL hide the specified application if it is running and visible.
--
-- ObjC selector: @- hideAppWithParams:completion:@
hideAppWithParams_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
hideAppWithParams_completion mtrBaseClusterApplicationLauncher  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "hideAppWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- hideAppWithCompletion:@
hideAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
hideAppWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "hideAppWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCatalogListWithCompletion:@
readAttributeCatalogListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCatalogListWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeCatalogListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentAppWithCompletion:@
readAttributeCurrentAppWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCurrentAppWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeCurrentAppWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentAppWithValue:completion:@
writeAttributeCurrentAppWithValue_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEPStruct value) => mtrBaseClusterApplicationLauncher -> value -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_completion mtrBaseClusterApplicationLauncher  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "writeAttributeCurrentAppWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeCurrentAppWithValue:params:completion:@
writeAttributeCurrentAppWithValue_params_completion :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEPStruct value, IsMTRWriteParams params) => mtrBaseClusterApplicationLauncher -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_params_completion mtrBaseClusterApplicationLauncher  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterApplicationLauncher (mkSelector "writeAttributeCurrentAppWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterApplicationLauncher  completion =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> IO (Id MTRBaseClusterApplicationLauncher)
init_ mtrBaseClusterApplicationLauncher  =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterApplicationLauncher)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterApplicationLauncher -> device -> CUShort -> queue -> IO (Id MTRBaseClusterApplicationLauncher)
initWithDevice_endpoint_queue mtrBaseClusterApplicationLauncher  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterApplicationLauncher (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- launchAppWithParams:completionHandler:@
launchAppWithParams_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterLaunchAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
launchAppWithParams_completionHandler mtrBaseClusterApplicationLauncher  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "launchAppWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopAppWithParams:completionHandler:@
stopAppWithParams_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterStopAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
stopAppWithParams_completionHandler mtrBaseClusterApplicationLauncher  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "stopAppWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- hideAppWithParams:completionHandler:@
hideAppWithParams_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterHideAppParams params) => mtrBaseClusterApplicationLauncher -> params -> Ptr () -> IO ()
hideAppWithParams_completionHandler mtrBaseClusterApplicationLauncher  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "hideAppWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCatalogListWithCompletionHandler:@
readAttributeCatalogListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCatalogListWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeCatalogListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentAppWithCompletionHandler:@
readAttributeCurrentAppWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeCurrentAppWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeCurrentAppWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeCurrentAppWithValue:completionHandler:@
writeAttributeCurrentAppWithValue_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEP value) => mtrBaseClusterApplicationLauncher -> value -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_completionHandler mtrBaseClusterApplicationLauncher  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterApplicationLauncher (mkSelector "writeAttributeCurrentAppWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeCurrentAppWithValue:params:completionHandler:@
writeAttributeCurrentAppWithValue_params_completionHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRApplicationLauncherClusterApplicationEP value, IsMTRWriteParams params) => mtrBaseClusterApplicationLauncher -> value -> params -> Ptr () -> IO ()
writeAttributeCurrentAppWithValue_params_completionHandler mtrBaseClusterApplicationLauncher  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterApplicationLauncher (mkSelector "writeAttributeCurrentAppWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher => mtrBaseClusterApplicationLauncher -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterApplicationLauncher  completionHandler =
    sendMsg mtrBaseClusterApplicationLauncher (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterApplicationLauncher -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterApplicationLauncher  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterApplicationLauncher"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterApplicationLauncher mtrBaseClusterApplicationLauncher, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterApplicationLauncher -> device -> endpointID -> queue -> IO (Id MTRBaseClusterApplicationLauncher)
initWithDevice_endpointID_queue mtrBaseClusterApplicationLauncher  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterApplicationLauncher (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @launchAppWithParams:completion:@
launchAppWithParams_completionSelector :: Selector
launchAppWithParams_completionSelector = mkSelector "launchAppWithParams:completion:"

-- | @Selector@ for @launchAppWithCompletion:@
launchAppWithCompletionSelector :: Selector
launchAppWithCompletionSelector = mkSelector "launchAppWithCompletion:"

-- | @Selector@ for @stopAppWithParams:completion:@
stopAppWithParams_completionSelector :: Selector
stopAppWithParams_completionSelector = mkSelector "stopAppWithParams:completion:"

-- | @Selector@ for @stopAppWithCompletion:@
stopAppWithCompletionSelector :: Selector
stopAppWithCompletionSelector = mkSelector "stopAppWithCompletion:"

-- | @Selector@ for @hideAppWithParams:completion:@
hideAppWithParams_completionSelector :: Selector
hideAppWithParams_completionSelector = mkSelector "hideAppWithParams:completion:"

-- | @Selector@ for @hideAppWithCompletion:@
hideAppWithCompletionSelector :: Selector
hideAppWithCompletionSelector = mkSelector "hideAppWithCompletion:"

-- | @Selector@ for @readAttributeCatalogListWithCompletion:@
readAttributeCatalogListWithCompletionSelector :: Selector
readAttributeCatalogListWithCompletionSelector = mkSelector "readAttributeCatalogListWithCompletion:"

-- | @Selector@ for @subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCatalogListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCatalogListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCatalogListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCatalogListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentAppWithCompletion:@
readAttributeCurrentAppWithCompletionSelector :: Selector
readAttributeCurrentAppWithCompletionSelector = mkSelector "readAttributeCurrentAppWithCompletion:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:completion:@
writeAttributeCurrentAppWithValue_completionSelector :: Selector
writeAttributeCurrentAppWithValue_completionSelector = mkSelector "writeAttributeCurrentAppWithValue:completion:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:params:completion:@
writeAttributeCurrentAppWithValue_params_completionSelector :: Selector
writeAttributeCurrentAppWithValue_params_completionSelector = mkSelector "writeAttributeCurrentAppWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentAppWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentAppWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentAppWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentAppWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @launchAppWithParams:completionHandler:@
launchAppWithParams_completionHandlerSelector :: Selector
launchAppWithParams_completionHandlerSelector = mkSelector "launchAppWithParams:completionHandler:"

-- | @Selector@ for @stopAppWithParams:completionHandler:@
stopAppWithParams_completionHandlerSelector :: Selector
stopAppWithParams_completionHandlerSelector = mkSelector "stopAppWithParams:completionHandler:"

-- | @Selector@ for @hideAppWithParams:completionHandler:@
hideAppWithParams_completionHandlerSelector :: Selector
hideAppWithParams_completionHandlerSelector = mkSelector "hideAppWithParams:completionHandler:"

-- | @Selector@ for @readAttributeCatalogListWithCompletionHandler:@
readAttributeCatalogListWithCompletionHandlerSelector :: Selector
readAttributeCatalogListWithCompletionHandlerSelector = mkSelector "readAttributeCatalogListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCatalogListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCatalogListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCatalogListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCatalogListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentAppWithCompletionHandler:@
readAttributeCurrentAppWithCompletionHandlerSelector :: Selector
readAttributeCurrentAppWithCompletionHandlerSelector = mkSelector "readAttributeCurrentAppWithCompletionHandler:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:completionHandler:@
writeAttributeCurrentAppWithValue_completionHandlerSelector :: Selector
writeAttributeCurrentAppWithValue_completionHandlerSelector = mkSelector "writeAttributeCurrentAppWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeCurrentAppWithValue:params:completionHandler:@
writeAttributeCurrentAppWithValue_params_completionHandlerSelector :: Selector
writeAttributeCurrentAppWithValue_params_completionHandlerSelector = mkSelector "writeAttributeCurrentAppWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentAppWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentAppWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentAppWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentAppWithAttributeCache:endpoint:queue:completionHandler:"

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

