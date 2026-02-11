{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster RVC Operational State
--
-- This cluster supports remotely monitoring and, where supported, changing the operational state of a Robotic Vacuum.
--
-- Generated bindings for @MTRBaseClusterRVCOperationalState@.
module ObjC.Matter.MTRBaseClusterRVCOperationalState
  ( MTRBaseClusterRVCOperationalState
  , IsMTRBaseClusterRVCOperationalState(..)
  , pauseWithParams_completion
  , pauseWithCompletion
  , resumeWithParams_completion
  , resumeWithCompletion
  , goHomeWithParams_completion
  , goHomeWithCompletion
  , readAttributePhaseListWithCompletion
  , subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler
  , readAttributePhaseListWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentPhaseWithCompletion
  , subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion
  , readAttributeCountdownTimeWithCompletion
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperationalStateListWithCompletion
  , subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperationalStateWithCompletion
  , subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeOperationalErrorWithCompletion
  , subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler
  , readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion
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
  , pauseWithParams_completionSelector
  , pauseWithCompletionSelector
  , resumeWithParams_completionSelector
  , resumeWithCompletionSelector
  , goHomeWithParams_completionSelector
  , goHomeWithCompletionSelector
  , readAttributePhaseListWithCompletionSelector
  , subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentPhaseWithCompletionSelector
  , subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCountdownTimeWithCompletionSelector
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperationalStateListWithCompletionSelector
  , subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperationalStateWithCompletionSelector
  , subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOperationalErrorWithCompletionSelector
  , subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command Pause
--
-- Upon receipt, the device SHALL pause its operation if it is possible based on the current function of the server.
--
-- ObjC selector: @- pauseWithParams:completion:@
pauseWithParams_completion :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRRVCOperationalStateClusterPauseParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> IO ()
pauseWithParams_completion mtrBaseClusterRVCOperationalState  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "pauseWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseWithCompletion:@
pauseWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
pauseWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "pauseWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Resume
--
-- Upon receipt, the device SHALL resume its operation from the point it was at when it received the Pause command, or from the point when it was paused by means outside of this cluster (for example by manual button press).
--
-- ObjC selector: @- resumeWithParams:completion:@
resumeWithParams_completion :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRRVCOperationalStateClusterResumeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> IO ()
resumeWithParams_completion mtrBaseClusterRVCOperationalState  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "resumeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resumeWithCompletion:@
resumeWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
resumeWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "resumeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command GoHome
--
-- On receipt of this command, the device SHALL start seeking the charging dock, if possible in the current state of the device.
--
-- ObjC selector: @- goHomeWithParams:completion:@
goHomeWithParams_completion :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRRVCOperationalStateClusterGoHomeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> IO ()
goHomeWithParams_completion mtrBaseClusterRVCOperationalState  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "goHomeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- goHomeWithCompletion:@
goHomeWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
goHomeWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "goHomeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePhaseListWithCompletion:@
readAttributePhaseListWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributePhaseListWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributePhaseListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:@
readAttributePhaseListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePhaseListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentPhaseWithCompletion:@
readAttributeCurrentPhaseWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeCurrentPhaseWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeCurrentPhaseWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeCountdownTimeWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeCountdownTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOperationalStateListWithCompletion:@
readAttributeOperationalStateListWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeOperationalStateListWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeOperationalStateListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOperationalStateWithCompletion:@
readAttributeOperationalStateWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeOperationalStateWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeOperationalStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOperationalErrorWithCompletion:@
readAttributeOperationalErrorWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeOperationalErrorWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeOperationalErrorWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterRVCOperationalState  completion =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRSubscribeParams params) => mtrBaseClusterRVCOperationalState -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterRVCOperationalState  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterRVCOperationalState (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState => mtrBaseClusterRVCOperationalState -> IO (Id MTRBaseClusterRVCOperationalState)
init_ mtrBaseClusterRVCOperationalState  =
    sendMsg mtrBaseClusterRVCOperationalState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterRVCOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterRVCOperationalState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterRVCOperationalState mtrBaseClusterRVCOperationalState, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterRVCOperationalState -> device -> endpointID -> queue -> IO (Id MTRBaseClusterRVCOperationalState)
initWithDevice_endpointID_queue mtrBaseClusterRVCOperationalState  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterRVCOperationalState (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pauseWithParams:completion:@
pauseWithParams_completionSelector :: Selector
pauseWithParams_completionSelector = mkSelector "pauseWithParams:completion:"

-- | @Selector@ for @pauseWithCompletion:@
pauseWithCompletionSelector :: Selector
pauseWithCompletionSelector = mkSelector "pauseWithCompletion:"

-- | @Selector@ for @resumeWithParams:completion:@
resumeWithParams_completionSelector :: Selector
resumeWithParams_completionSelector = mkSelector "resumeWithParams:completion:"

-- | @Selector@ for @resumeWithCompletion:@
resumeWithCompletionSelector :: Selector
resumeWithCompletionSelector = mkSelector "resumeWithCompletion:"

-- | @Selector@ for @goHomeWithParams:completion:@
goHomeWithParams_completionSelector :: Selector
goHomeWithParams_completionSelector = mkSelector "goHomeWithParams:completion:"

-- | @Selector@ for @goHomeWithCompletion:@
goHomeWithCompletionSelector :: Selector
goHomeWithCompletionSelector = mkSelector "goHomeWithCompletion:"

-- | @Selector@ for @readAttributePhaseListWithCompletion:@
readAttributePhaseListWithCompletionSelector :: Selector
readAttributePhaseListWithCompletionSelector = mkSelector "readAttributePhaseListWithCompletion:"

-- | @Selector@ for @subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePhaseListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePhaseListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:@
readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePhaseListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePhaseListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentPhaseWithCompletion:@
readAttributeCurrentPhaseWithCompletionSelector :: Selector
readAttributeCurrentPhaseWithCompletionSelector = mkSelector "readAttributeCurrentPhaseWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentPhaseWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentPhaseWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentPhaseWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentPhaseWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletionSelector :: Selector
readAttributeCountdownTimeWithCompletionSelector = mkSelector "readAttributeCountdownTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperationalStateListWithCompletion:@
readAttributeOperationalStateListWithCompletionSelector :: Selector
readAttributeOperationalStateListWithCompletionSelector = mkSelector "readAttributeOperationalStateListWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOperationalStateListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperationalStateListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOperationalStateListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperationalStateListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperationalStateWithCompletion:@
readAttributeOperationalStateWithCompletionSelector :: Selector
readAttributeOperationalStateWithCompletionSelector = mkSelector "readAttributeOperationalStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOperationalStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperationalStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOperationalStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperationalStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOperationalErrorWithCompletion:@
readAttributeOperationalErrorWithCompletionSelector :: Selector
readAttributeOperationalErrorWithCompletionSelector = mkSelector "readAttributeOperationalErrorWithCompletion:"

-- | @Selector@ for @subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOperationalErrorWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOperationalErrorWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:@
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOperationalErrorWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOperationalErrorWithClusterStateCache:endpoint:queue:completion:"

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

