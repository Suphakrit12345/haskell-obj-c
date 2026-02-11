{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator Alarm
--
-- Attributes and commands for configuring the Refrigerator alarm.
--
-- Generated bindings for @MTRBaseClusterRefrigeratorAlarm@.
module ObjC.Matter.MTRBaseClusterRefrigeratorAlarm
  ( MTRBaseClusterRefrigeratorAlarm
  , IsMTRBaseClusterRefrigeratorAlarm(..)
  , readAttributeMaskWithCompletion
  , subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaskWithClusterStateCache_endpoint_queue_completion
  , readAttributeStateWithCompletion
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedWithCompletion
  , subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeMaskWithCompletionSelector
  , subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStateWithCompletionSelector
  , subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedWithCompletionSelector
  , subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeMaskWithCompletion:@
readAttributeMaskWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeMaskWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeMaskWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaskWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaskWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaskWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaskWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStateWithCompletion:@
readAttributeStateWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeStateWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedWithCompletion:@
readAttributeSupportedWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeSupportedWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeSupportedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterRefrigeratorAlarm  completion =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRSubscribeParams params) => mtrBaseClusterRefrigeratorAlarm -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRefrigeratorAlarm  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm => mtrBaseClusterRefrigeratorAlarm -> IO (Id MTRBaseClusterRefrigeratorAlarm)
init_ mtrBaseClusterRefrigeratorAlarm  =
    sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterRefrigeratorAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterRefrigeratorAlarm"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterRefrigeratorAlarm mtrBaseClusterRefrigeratorAlarm, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterRefrigeratorAlarm -> device -> endpointID -> queue -> IO (Id MTRBaseClusterRefrigeratorAlarm)
initWithDevice_endpointID_queue mtrBaseClusterRefrigeratorAlarm  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterRefrigeratorAlarm (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMaskWithCompletion:@
readAttributeMaskWithCompletionSelector :: Selector
readAttributeMaskWithCompletionSelector = mkSelector "readAttributeMaskWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaskWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaskWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaskWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaskWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaskWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStateWithCompletion:@
readAttributeStateWithCompletionSelector :: Selector
readAttributeStateWithCompletionSelector = mkSelector "readAttributeStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedWithCompletion:@
readAttributeSupportedWithCompletionSelector :: Selector
readAttributeSupportedWithCompletionSelector = mkSelector "readAttributeSupportedWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedWithClusterStateCache:endpoint:queue:completion:"

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

