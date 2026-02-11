{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Actions
--
-- This cluster provides a standardized way for a Node (typically a Bridge, but could be any Node) to expose action information.
--
-- Generated bindings for @MTRBaseClusterActions@.
module ObjC.Matter.MTRBaseClusterActions
  ( MTRBaseClusterActions
  , IsMTRBaseClusterActions(..)
  , instantActionWithParams_completion
  , instantActionWithTransitionWithParams_completion
  , startActionWithParams_completion
  , startActionWithDurationWithParams_completion
  , stopActionWithParams_completion
  , pauseActionWithParams_completion
  , pauseActionWithDurationWithParams_completion
  , resumeActionWithParams_completion
  , enableActionWithParams_completion
  , enableActionWithDurationWithParams_completion
  , disableActionWithParams_completion
  , disableActionWithDurationWithParams_completion
  , readAttributeActionListWithCompletion
  , subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandler
  , readAttributeActionListWithClusterStateCache_endpoint_queue_completion
  , readAttributeEndpointListsWithCompletion
  , subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandler
  , readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSetupURLWithCompletion
  , subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandler
  , readAttributeSetupURLWithClusterStateCache_endpoint_queue_completion
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
  , instantActionWithParams_completionHandler
  , instantActionWithTransitionWithParams_completionHandler
  , startActionWithParams_completionHandler
  , startActionWithDurationWithParams_completionHandler
  , stopActionWithParams_completionHandler
  , pauseActionWithParams_completionHandler
  , pauseActionWithDurationWithParams_completionHandler
  , resumeActionWithParams_completionHandler
  , enableActionWithParams_completionHandler
  , enableActionWithDurationWithParams_completionHandler
  , disableActionWithParams_completionHandler
  , disableActionWithDurationWithParams_completionHandler
  , readAttributeActionListWithCompletionHandler
  , subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActionListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeEndpointListsWithCompletionHandler
  , subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSetupURLWithCompletionHandler
  , subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandler
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
  , instantActionWithParams_completionSelector
  , instantActionWithTransitionWithParams_completionSelector
  , startActionWithParams_completionSelector
  , startActionWithDurationWithParams_completionSelector
  , stopActionWithParams_completionSelector
  , pauseActionWithParams_completionSelector
  , pauseActionWithDurationWithParams_completionSelector
  , resumeActionWithParams_completionSelector
  , enableActionWithParams_completionSelector
  , enableActionWithDurationWithParams_completionSelector
  , disableActionWithParams_completionSelector
  , disableActionWithDurationWithParams_completionSelector
  , readAttributeActionListWithCompletionSelector
  , subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEndpointListsWithCompletionSelector
  , subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSetupURLWithCompletionSelector
  , subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector
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
  , instantActionWithParams_completionHandlerSelector
  , instantActionWithTransitionWithParams_completionHandlerSelector
  , startActionWithParams_completionHandlerSelector
  , startActionWithDurationWithParams_completionHandlerSelector
  , stopActionWithParams_completionHandlerSelector
  , pauseActionWithParams_completionHandlerSelector
  , pauseActionWithDurationWithParams_completionHandlerSelector
  , resumeActionWithParams_completionHandlerSelector
  , enableActionWithParams_completionHandlerSelector
  , enableActionWithDurationWithParams_completionHandlerSelector
  , disableActionWithParams_completionHandlerSelector
  , disableActionWithDurationWithParams_completionHandlerSelector
  , readAttributeActionListWithCompletionHandlerSelector
  , subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeEndpointListsWithCompletionHandlerSelector
  , subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSetupURLWithCompletionHandlerSelector
  , subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command InstantAction
--
-- This command is used to trigger an instantaneous action.
--
-- ObjC selector: @- instantActionWithParams:completion:@
instantActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "instantActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command InstantActionWithTransition
--
-- This command is used to trigger an instantaneous action with a transition over a given time.
--
-- ObjC selector: @- instantActionWithTransitionWithParams:completion:@
instantActionWithTransitionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionWithTransitionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithTransitionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "instantActionWithTransitionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StartAction
--
-- This command is used to trigger the commencement of an action.
--
-- ObjC selector: @- startActionWithParams:completion:@
startActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "startActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StartActionWithDuration
--
-- This command is used to trigger the commencement of an action with a duration.
--
-- ObjC selector: @- startActionWithDurationWithParams:completion:@
startActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithDurationWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "startActionWithDurationWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command StopAction
--
-- This command is used to stop an action.
--
-- ObjC selector: @- stopActionWithParams:completion:@
stopActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStopActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
stopActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "stopActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command PauseAction
--
-- This command is used to pause an action.
--
-- ObjC selector: @- pauseActionWithParams:completion:@
pauseActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "pauseActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command PauseActionWithDuration
--
-- This command is used to pause an action with a duration.
--
-- ObjC selector: @- pauseActionWithDurationWithParams:completion:@
pauseActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithDurationWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "pauseActionWithDurationWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ResumeAction
--
-- This command is used to resume an action.
--
-- ObjC selector: @- resumeActionWithParams:completion:@
resumeActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterResumeActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
resumeActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "resumeActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command EnableAction
--
-- This command is used to enable an action.
--
-- ObjC selector: @- enableActionWithParams:completion:@
enableActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "enableActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command EnableActionWithDuration
--
-- This command is used to enable an action with a duration.
--
-- ObjC selector: @- enableActionWithDurationWithParams:completion:@
enableActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithDurationWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "enableActionWithDurationWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command DisableAction
--
-- This command is used to disable an action.
--
-- ObjC selector: @- disableActionWithParams:completion:@
disableActionWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "disableActionWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command DisableActionWithDuration
--
-- This command is used to disable an action with a duration.
--
-- ObjC selector: @- disableActionWithDurationWithParams:completion:@
disableActionWithDurationWithParams_completion :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithDurationWithParams_completion mtrBaseClusterActions  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "disableActionWithDurationWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActionListWithCompletion:@
readAttributeActionListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeActionListWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeActionListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActionListWithClusterStateCache:endpoint:queue:completion:@
readAttributeActionListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActionListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActionListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEndpointListsWithCompletion:@
readAttributeEndpointListsWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeEndpointListsWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeEndpointListsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSetupURLWithCompletion:@
readAttributeSetupURLWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeSetupURLWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeSetupURLWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterActions  completion =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRSubscribeParams params) => mtrBaseClusterActions -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterActions  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> IO (Id MTRBaseClusterActions)
init_ mtrBaseClusterActions  =
    sendMsg mtrBaseClusterActions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterActions)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterActions -> device -> CUShort -> queue -> IO (Id MTRBaseClusterActions)
initWithDevice_endpoint_queue mtrBaseClusterActions  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterActions (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- instantActionWithParams:completionHandler:@
instantActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "instantActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- instantActionWithTransitionWithParams:completionHandler:@
instantActionWithTransitionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterInstantActionWithTransitionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
instantActionWithTransitionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "instantActionWithTransitionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startActionWithParams:completionHandler:@
startActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "startActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startActionWithDurationWithParams:completionHandler:@
startActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStartActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
startActionWithDurationWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "startActionWithDurationWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopActionWithParams:completionHandler:@
stopActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterStopActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
stopActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "stopActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseActionWithParams:completionHandler:@
pauseActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "pauseActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseActionWithDurationWithParams:completionHandler:@
pauseActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterPauseActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
pauseActionWithDurationWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "pauseActionWithDurationWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resumeActionWithParams:completionHandler:@
resumeActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterResumeActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
resumeActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "resumeActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enableActionWithParams:completionHandler:@
enableActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "enableActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- enableActionWithDurationWithParams:completionHandler:@
enableActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterEnableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
enableActionWithDurationWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "enableActionWithDurationWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- disableActionWithParams:completionHandler:@
disableActionWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "disableActionWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- disableActionWithDurationWithParams:completionHandler:@
disableActionWithDurationWithParams_completionHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRActionsClusterDisableActionWithDurationParams params) => mtrBaseClusterActions -> params -> Ptr () -> IO ()
disableActionWithDurationWithParams_completionHandler mtrBaseClusterActions  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterActions (mkSelector "disableActionWithDurationWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeActionListWithCompletionHandler:@
readAttributeActionListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeActionListWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeActionListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeEndpointListsWithCompletionHandler:@
readAttributeEndpointListsWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeEndpointListsWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeEndpointListsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSetupURLWithCompletionHandler:@
readAttributeSetupURLWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeSetupURLWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeSetupURLWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterActions mtrBaseClusterActions => mtrBaseClusterActions -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterActions  completionHandler =
    sendMsg mtrBaseClusterActions (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterActions -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterActions  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterActions (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterActions"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterActions mtrBaseClusterActions, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterActions -> device -> endpointID -> queue -> IO (Id MTRBaseClusterActions)
initWithDevice_endpointID_queue mtrBaseClusterActions  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterActions (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instantActionWithParams:completion:@
instantActionWithParams_completionSelector :: Selector
instantActionWithParams_completionSelector = mkSelector "instantActionWithParams:completion:"

-- | @Selector@ for @instantActionWithTransitionWithParams:completion:@
instantActionWithTransitionWithParams_completionSelector :: Selector
instantActionWithTransitionWithParams_completionSelector = mkSelector "instantActionWithTransitionWithParams:completion:"

-- | @Selector@ for @startActionWithParams:completion:@
startActionWithParams_completionSelector :: Selector
startActionWithParams_completionSelector = mkSelector "startActionWithParams:completion:"

-- | @Selector@ for @startActionWithDurationWithParams:completion:@
startActionWithDurationWithParams_completionSelector :: Selector
startActionWithDurationWithParams_completionSelector = mkSelector "startActionWithDurationWithParams:completion:"

-- | @Selector@ for @stopActionWithParams:completion:@
stopActionWithParams_completionSelector :: Selector
stopActionWithParams_completionSelector = mkSelector "stopActionWithParams:completion:"

-- | @Selector@ for @pauseActionWithParams:completion:@
pauseActionWithParams_completionSelector :: Selector
pauseActionWithParams_completionSelector = mkSelector "pauseActionWithParams:completion:"

-- | @Selector@ for @pauseActionWithDurationWithParams:completion:@
pauseActionWithDurationWithParams_completionSelector :: Selector
pauseActionWithDurationWithParams_completionSelector = mkSelector "pauseActionWithDurationWithParams:completion:"

-- | @Selector@ for @resumeActionWithParams:completion:@
resumeActionWithParams_completionSelector :: Selector
resumeActionWithParams_completionSelector = mkSelector "resumeActionWithParams:completion:"

-- | @Selector@ for @enableActionWithParams:completion:@
enableActionWithParams_completionSelector :: Selector
enableActionWithParams_completionSelector = mkSelector "enableActionWithParams:completion:"

-- | @Selector@ for @enableActionWithDurationWithParams:completion:@
enableActionWithDurationWithParams_completionSelector :: Selector
enableActionWithDurationWithParams_completionSelector = mkSelector "enableActionWithDurationWithParams:completion:"

-- | @Selector@ for @disableActionWithParams:completion:@
disableActionWithParams_completionSelector :: Selector
disableActionWithParams_completionSelector = mkSelector "disableActionWithParams:completion:"

-- | @Selector@ for @disableActionWithDurationWithParams:completion:@
disableActionWithDurationWithParams_completionSelector :: Selector
disableActionWithDurationWithParams_completionSelector = mkSelector "disableActionWithDurationWithParams:completion:"

-- | @Selector@ for @readAttributeActionListWithCompletion:@
readAttributeActionListWithCompletionSelector :: Selector
readAttributeActionListWithCompletionSelector = mkSelector "readAttributeActionListWithCompletion:"

-- | @Selector@ for @subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActionListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActionListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActionListWithClusterStateCache:endpoint:queue:completion:@
readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActionListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActionListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEndpointListsWithCompletion:@
readAttributeEndpointListsWithCompletionSelector :: Selector
readAttributeEndpointListsWithCompletionSelector = mkSelector "readAttributeEndpointListsWithCompletion:"

-- | @Selector@ for @subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEndpointListsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointListsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:@
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEndpointListsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEndpointListsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSetupURLWithCompletion:@
readAttributeSetupURLWithCompletionSelector :: Selector
readAttributeSetupURLWithCompletionSelector = mkSelector "readAttributeSetupURLWithCompletion:"

-- | @Selector@ for @subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSetupURLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSetupURLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSetupURLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSetupURLWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @instantActionWithParams:completionHandler:@
instantActionWithParams_completionHandlerSelector :: Selector
instantActionWithParams_completionHandlerSelector = mkSelector "instantActionWithParams:completionHandler:"

-- | @Selector@ for @instantActionWithTransitionWithParams:completionHandler:@
instantActionWithTransitionWithParams_completionHandlerSelector :: Selector
instantActionWithTransitionWithParams_completionHandlerSelector = mkSelector "instantActionWithTransitionWithParams:completionHandler:"

-- | @Selector@ for @startActionWithParams:completionHandler:@
startActionWithParams_completionHandlerSelector :: Selector
startActionWithParams_completionHandlerSelector = mkSelector "startActionWithParams:completionHandler:"

-- | @Selector@ for @startActionWithDurationWithParams:completionHandler:@
startActionWithDurationWithParams_completionHandlerSelector :: Selector
startActionWithDurationWithParams_completionHandlerSelector = mkSelector "startActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @stopActionWithParams:completionHandler:@
stopActionWithParams_completionHandlerSelector :: Selector
stopActionWithParams_completionHandlerSelector = mkSelector "stopActionWithParams:completionHandler:"

-- | @Selector@ for @pauseActionWithParams:completionHandler:@
pauseActionWithParams_completionHandlerSelector :: Selector
pauseActionWithParams_completionHandlerSelector = mkSelector "pauseActionWithParams:completionHandler:"

-- | @Selector@ for @pauseActionWithDurationWithParams:completionHandler:@
pauseActionWithDurationWithParams_completionHandlerSelector :: Selector
pauseActionWithDurationWithParams_completionHandlerSelector = mkSelector "pauseActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @resumeActionWithParams:completionHandler:@
resumeActionWithParams_completionHandlerSelector :: Selector
resumeActionWithParams_completionHandlerSelector = mkSelector "resumeActionWithParams:completionHandler:"

-- | @Selector@ for @enableActionWithParams:completionHandler:@
enableActionWithParams_completionHandlerSelector :: Selector
enableActionWithParams_completionHandlerSelector = mkSelector "enableActionWithParams:completionHandler:"

-- | @Selector@ for @enableActionWithDurationWithParams:completionHandler:@
enableActionWithDurationWithParams_completionHandlerSelector :: Selector
enableActionWithDurationWithParams_completionHandlerSelector = mkSelector "enableActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @disableActionWithParams:completionHandler:@
disableActionWithParams_completionHandlerSelector :: Selector
disableActionWithParams_completionHandlerSelector = mkSelector "disableActionWithParams:completionHandler:"

-- | @Selector@ for @disableActionWithDurationWithParams:completionHandler:@
disableActionWithDurationWithParams_completionHandlerSelector :: Selector
disableActionWithDurationWithParams_completionHandlerSelector = mkSelector "disableActionWithDurationWithParams:completionHandler:"

-- | @Selector@ for @readAttributeActionListWithCompletionHandler:@
readAttributeActionListWithCompletionHandlerSelector :: Selector
readAttributeActionListWithCompletionHandlerSelector = mkSelector "readAttributeActionListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActionListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActionListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeActionListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActionListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeEndpointListsWithCompletionHandler:@
readAttributeEndpointListsWithCompletionHandlerSelector :: Selector
readAttributeEndpointListsWithCompletionHandlerSelector = mkSelector "readAttributeEndpointListsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEndpointListsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEndpointListsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeEndpointListsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeEndpointListsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSetupURLWithCompletionHandler:@
readAttributeSetupURLWithCompletionHandlerSelector :: Selector
readAttributeSetupURLWithCompletionHandlerSelector = mkSelector "readAttributeSetupURLWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSetupURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSetupURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSetupURLWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSetupURLWithAttributeCache:endpoint:queue:completionHandler:"

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

