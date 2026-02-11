{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Timer
--
-- This cluster supports creating a simple timer functionality.
--
-- Generated bindings for @MTRBaseClusterTimer@.
module ObjC.Matter.MTRBaseClusterTimer
  ( MTRBaseClusterTimer
  , IsMTRBaseClusterTimer(..)
  , setTimerWithParams_completion
  , resetTimerWithParams_completion
  , resetTimerWithCompletion
  , addTimeWithParams_completion
  , reduceTimeWithParams_completion
  , readAttributeSetTimeWithCompletion
  , subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSetTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeRemainingWithCompletion
  , subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimerStateWithCompletion
  , subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimerStateWithClusterStateCache_endpoint_queue_completion
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
  , setTimerWithParams_completionSelector
  , resetTimerWithParams_completionSelector
  , resetTimerWithCompletionSelector
  , addTimeWithParams_completionSelector
  , reduceTimeWithParams_completionSelector
  , readAttributeSetTimeWithCompletionSelector
  , subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeRemainingWithCompletionSelector
  , subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimerStateWithCompletionSelector
  , subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SetTimer
--
-- This command is used to set the timer.
--
-- ObjC selector: @- setTimerWithParams:completion:@
setTimerWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterSetTimerParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
setTimerWithParams_completion mtrBaseClusterTimer  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "setTimerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ResetTimer
--
-- This command is used to reset the timer to the original value.
--
-- ObjC selector: @- resetTimerWithParams:completion:@
resetTimerWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterResetTimerParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
resetTimerWithParams_completion mtrBaseClusterTimer  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "resetTimerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetTimerWithCompletion:@
resetTimerWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
resetTimerWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "resetTimerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command AddTime
--
-- This command is used to add time to the existing timer.
--
-- ObjC selector: @- addTimeWithParams:completion:@
addTimeWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterAddTimeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
addTimeWithParams_completion mtrBaseClusterTimer  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "addTimeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ReduceTime
--
-- This command is used to reduce time on the existing timer.
--
-- ObjC selector: @- reduceTimeWithParams:completion:@
reduceTimeWithParams_completion :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRTimerClusterReduceTimeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> IO ()
reduceTimeWithParams_completion mtrBaseClusterTimer  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "reduceTimeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSetTimeWithCompletion:@
readAttributeSetTimeWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeSetTimeWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeSetTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimeRemainingWithCompletion:@
readAttributeTimeRemainingWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeTimeRemainingWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeTimeRemainingWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimerStateWithCompletion:@
readAttributeTimerStateWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeTimerStateWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeTimerStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTimer  completion =
    sendMsg mtrBaseClusterTimer (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRSubscribeParams params) => mtrBaseClusterTimer -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimer  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimer (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterTimer mtrBaseClusterTimer => mtrBaseClusterTimer -> IO (Id MTRBaseClusterTimer)
init_ mtrBaseClusterTimer  =
    sendMsg mtrBaseClusterTimer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterTimer)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTimer mtrBaseClusterTimer, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTimer -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTimer)
initWithDevice_endpointID_queue mtrBaseClusterTimer  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterTimer (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTimerWithParams:completion:@
setTimerWithParams_completionSelector :: Selector
setTimerWithParams_completionSelector = mkSelector "setTimerWithParams:completion:"

-- | @Selector@ for @resetTimerWithParams:completion:@
resetTimerWithParams_completionSelector :: Selector
resetTimerWithParams_completionSelector = mkSelector "resetTimerWithParams:completion:"

-- | @Selector@ for @resetTimerWithCompletion:@
resetTimerWithCompletionSelector :: Selector
resetTimerWithCompletionSelector = mkSelector "resetTimerWithCompletion:"

-- | @Selector@ for @addTimeWithParams:completion:@
addTimeWithParams_completionSelector :: Selector
addTimeWithParams_completionSelector = mkSelector "addTimeWithParams:completion:"

-- | @Selector@ for @reduceTimeWithParams:completion:@
reduceTimeWithParams_completionSelector :: Selector
reduceTimeWithParams_completionSelector = mkSelector "reduceTimeWithParams:completion:"

-- | @Selector@ for @readAttributeSetTimeWithCompletion:@
readAttributeSetTimeWithCompletionSelector :: Selector
readAttributeSetTimeWithCompletionSelector = mkSelector "readAttributeSetTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSetTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSetTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSetTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSetTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeRemainingWithCompletion:@
readAttributeTimeRemainingWithCompletionSelector :: Selector
readAttributeTimeRemainingWithCompletionSelector = mkSelector "readAttributeTimeRemainingWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeRemainingWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeRemainingWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimeRemainingWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeRemainingWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimerStateWithCompletion:@
readAttributeTimerStateWithCompletionSelector :: Selector
readAttributeTimerStateWithCompletionSelector = mkSelector "readAttributeTimerStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimerStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimerStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimerStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimerStateWithClusterStateCache:endpoint:queue:completion:"

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

