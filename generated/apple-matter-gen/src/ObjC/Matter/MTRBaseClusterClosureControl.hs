{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Control
--
-- This cluster provides an interface for controlling a Closure.
--
-- Generated bindings for @MTRBaseClusterClosureControl@.
module ObjC.Matter.MTRBaseClusterClosureControl
  ( MTRBaseClusterClosureControl
  , IsMTRBaseClusterClosureControl(..)
  , stopWithParams_completion
  , stopWithCompletion
  , moveToWithParams_completion
  , moveToWithCompletion
  , calibrateWithParams_completion
  , calibrateWithCompletion
  , readAttributeCountdownTimeWithCompletion
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeMainStateWithCompletion
  , subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeMainStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentErrorListWithCompletion
  , subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverallCurrentStateWithCompletion
  , subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeOverallTargetStateWithCompletion
  , subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeLatchControlModesWithCompletion
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion
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
  , stopWithParams_completionSelector
  , stopWithCompletionSelector
  , moveToWithParams_completionSelector
  , moveToWithCompletionSelector
  , calibrateWithParams_completionSelector
  , calibrateWithCompletionSelector
  , readAttributeCountdownTimeWithCompletionSelector
  , subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMainStateWithCompletionSelector
  , subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentErrorListWithCompletionSelector
  , subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverallCurrentStateWithCompletionSelector
  , subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOverallTargetStateWithCompletionSelector
  , subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLatchControlModesWithCompletionSelector
  , subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command Stop
--
-- On receipt of this command, the closure SHALL stop its movement as fast as the closure is able too.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRClosureControlClusterStopParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterClosureControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "stopWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithCompletion:@
stopWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
stopWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "stopWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command MoveTo
--
-- On receipt of this command, the closure SHALL operate to update its position, latch state and/or motion speed.
--
-- ObjC selector: @- moveToWithParams:completion:@
moveToWithParams_completion :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRClosureControlClusterMoveToParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> IO ()
moveToWithParams_completion mtrBaseClusterClosureControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "moveToWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- moveToWithCompletion:@
moveToWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
moveToWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "moveToWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Calibrate
--
-- This command is used to trigger a calibration of the closure.
--
-- ObjC selector: @- calibrateWithParams:completion:@
calibrateWithParams_completion :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRClosureControlClusterCalibrateParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> IO ()
calibrateWithParams_completion mtrBaseClusterClosureControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "calibrateWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- calibrateWithCompletion:@
calibrateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
calibrateWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "calibrateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeCountdownTimeWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeCountdownTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMainStateWithCompletion:@
readAttributeMainStateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeMainStateWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeMainStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMainStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMainStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentErrorListWithCompletion:@
readAttributeCurrentErrorListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeCurrentErrorListWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeCurrentErrorListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOverallCurrentStateWithCompletion:@
readAttributeOverallCurrentStateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeOverallCurrentStateWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeOverallCurrentStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOverallTargetStateWithCompletion:@
readAttributeOverallTargetStateWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeOverallTargetStateWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeOverallTargetStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeLatchControlModesWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeLatchControlModesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterClosureControl  completion =
    sendMsg mtrBaseClusterClosureControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRSubscribeParams params) => mtrBaseClusterClosureControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterClosureControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterClosureControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl => mtrBaseClusterClosureControl -> IO (Id MTRBaseClusterClosureControl)
init_ mtrBaseClusterClosureControl  =
    sendMsg mtrBaseClusterClosureControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterClosureControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterClosureControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterClosureControl mtrBaseClusterClosureControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterClosureControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterClosureControl)
initWithDevice_endpointID_queue mtrBaseClusterClosureControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterClosureControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @stopWithCompletion:@
stopWithCompletionSelector :: Selector
stopWithCompletionSelector = mkSelector "stopWithCompletion:"

-- | @Selector@ for @moveToWithParams:completion:@
moveToWithParams_completionSelector :: Selector
moveToWithParams_completionSelector = mkSelector "moveToWithParams:completion:"

-- | @Selector@ for @moveToWithCompletion:@
moveToWithCompletionSelector :: Selector
moveToWithCompletionSelector = mkSelector "moveToWithCompletion:"

-- | @Selector@ for @calibrateWithParams:completion:@
calibrateWithParams_completionSelector :: Selector
calibrateWithParams_completionSelector = mkSelector "calibrateWithParams:completion:"

-- | @Selector@ for @calibrateWithCompletion:@
calibrateWithCompletionSelector :: Selector
calibrateWithCompletionSelector = mkSelector "calibrateWithCompletion:"

-- | @Selector@ for @readAttributeCountdownTimeWithCompletion:@
readAttributeCountdownTimeWithCompletionSelector :: Selector
readAttributeCountdownTimeWithCompletionSelector = mkSelector "readAttributeCountdownTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCountdownTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCountdownTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCountdownTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCountdownTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMainStateWithCompletion:@
readAttributeMainStateWithCompletionSelector :: Selector
readAttributeMainStateWithCompletionSelector = mkSelector "readAttributeMainStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMainStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMainStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMainStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMainStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentErrorListWithCompletion:@
readAttributeCurrentErrorListWithCompletionSelector :: Selector
readAttributeCurrentErrorListWithCompletionSelector = mkSelector "readAttributeCurrentErrorListWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentErrorListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentErrorListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentErrorListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentErrorListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverallCurrentStateWithCompletion:@
readAttributeOverallCurrentStateWithCompletionSelector :: Selector
readAttributeOverallCurrentStateWithCompletionSelector = mkSelector "readAttributeOverallCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverallCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverallCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOverallCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverallCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOverallTargetStateWithCompletion:@
readAttributeOverallTargetStateWithCompletionSelector :: Selector
readAttributeOverallTargetStateWithCompletionSelector = mkSelector "readAttributeOverallTargetStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOverallTargetStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOverallTargetStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOverallTargetStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOverallTargetStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLatchControlModesWithCompletion:@
readAttributeLatchControlModesWithCompletionSelector :: Selector
readAttributeLatchControlModesWithCompletionSelector = mkSelector "readAttributeLatchControlModesWithCompletion:"

-- | @Selector@ for @subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLatchControlModesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLatchControlModesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:@
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLatchControlModesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLatchControlModesWithClusterStateCache:endpoint:queue:completion:"

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

