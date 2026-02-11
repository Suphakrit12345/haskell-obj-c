{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Valve Configuration and Control
--
-- This cluster is used to configure a valve.
--
-- Generated bindings for @MTRBaseClusterValveConfigurationAndControl@.
module ObjC.Matter.MTRBaseClusterValveConfigurationAndControl
  ( MTRBaseClusterValveConfigurationAndControl
  , IsMTRBaseClusterValveConfigurationAndControl(..)
  , openWithParams_completion
  , openWithCompletion
  , closeWithParams_completion
  , closeWithCompletion
  , readAttributeOpenDurationWithCompletion
  , subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultOpenDurationWithCompletion
  , writeAttributeDefaultOpenDurationWithValue_completion
  , writeAttributeDefaultOpenDurationWithValue_params_completion
  , subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeAutoCloseTimeWithCompletion
  , subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeRemainingDurationWithCompletion
  , subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentStateWithCompletion
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetStateWithCompletion
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentLevelWithCompletion
  , subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeTargetLevelWithCompletion
  , subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultOpenLevelWithCompletion
  , writeAttributeDefaultOpenLevelWithValue_completion
  , writeAttributeDefaultOpenLevelWithValue_params_completion
  , subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completion
  , readAttributeValveFaultWithCompletion
  , subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandler
  , readAttributeValveFaultWithClusterStateCache_endpoint_queue_completion
  , readAttributeLevelStepWithCompletion
  , subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandler
  , readAttributeLevelStepWithClusterStateCache_endpoint_queue_completion
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
  , openWithParams_completionSelector
  , openWithCompletionSelector
  , closeWithParams_completionSelector
  , closeWithCompletionSelector
  , readAttributeOpenDurationWithCompletionSelector
  , subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultOpenDurationWithCompletionSelector
  , writeAttributeDefaultOpenDurationWithValue_completionSelector
  , writeAttributeDefaultOpenDurationWithValue_params_completionSelector
  , subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAutoCloseTimeWithCompletionSelector
  , subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRemainingDurationWithCompletionSelector
  , subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentStateWithCompletionSelector
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetStateWithCompletionSelector
  , subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentLevelWithCompletionSelector
  , subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTargetLevelWithCompletionSelector
  , subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultOpenLevelWithCompletionSelector
  , writeAttributeDefaultOpenLevelWithValue_completionSelector
  , writeAttributeDefaultOpenLevelWithValue_params_completionSelector
  , subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeValveFaultWithCompletionSelector
  , subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLevelStepWithCompletionSelector
  , subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command Open
--
-- This command is used to set the valve to its open position.
--
-- ObjC selector: @- openWithParams:completion:@
openWithParams_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterOpenParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> IO ()
openWithParams_completion mtrBaseClusterValveConfigurationAndControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "openWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- openWithCompletion:@
openWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
openWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "openWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Close
--
-- This command is used to set the valve to its closed position.
--
-- ObjC selector: @- closeWithParams:completion:@
closeWithParams_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterCloseParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> IO ()
closeWithParams_completion mtrBaseClusterValveConfigurationAndControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "closeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- closeWithCompletion:@
closeWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
closeWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "closeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOpenDurationWithCompletion:@
readAttributeOpenDurationWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeOpenDurationWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeOpenDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultOpenDurationWithCompletion:@
readAttributeDefaultOpenDurationWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeDefaultOpenDurationWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeDefaultOpenDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDefaultOpenDurationWithValue:completion:@
writeAttributeDefaultOpenDurationWithValue_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value) => mtrBaseClusterValveConfigurationAndControl -> value -> Ptr () -> IO ()
writeAttributeDefaultOpenDurationWithValue_completion mtrBaseClusterValveConfigurationAndControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenDurationWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDefaultOpenDurationWithValue:params:completion:@
writeAttributeDefaultOpenDurationWithValue_params_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterValveConfigurationAndControl -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultOpenDurationWithValue_params_completion mtrBaseClusterValveConfigurationAndControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenDurationWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAutoCloseTimeWithCompletion:@
readAttributeAutoCloseTimeWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeAutoCloseTimeWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeAutoCloseTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRemainingDurationWithCompletion:@
readAttributeRemainingDurationWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeRemainingDurationWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeRemainingDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeCurrentStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeTargetStateWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeTargetStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentLevelWithCompletion:@
readAttributeCurrentLevelWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeCurrentLevelWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeCurrentLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTargetLevelWithCompletion:@
readAttributeTargetLevelWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeTargetLevelWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeTargetLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultOpenLevelWithCompletion:@
readAttributeDefaultOpenLevelWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeDefaultOpenLevelWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeDefaultOpenLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDefaultOpenLevelWithValue:completion:@
writeAttributeDefaultOpenLevelWithValue_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value) => mtrBaseClusterValveConfigurationAndControl -> value -> Ptr () -> IO ()
writeAttributeDefaultOpenLevelWithValue_completion mtrBaseClusterValveConfigurationAndControl  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeDefaultOpenLevelWithValue:params:completion:@
writeAttributeDefaultOpenLevelWithValue_params_completion :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterValveConfigurationAndControl -> value -> params -> Ptr () -> IO ()
writeAttributeDefaultOpenLevelWithValue_params_completion mtrBaseClusterValveConfigurationAndControl  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "writeAttributeDefaultOpenLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeValveFaultWithCompletion:@
readAttributeValveFaultWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeValveFaultWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeValveFaultWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLevelStepWithCompletion:@
readAttributeLevelStepWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeLevelStepWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeLevelStepWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterValveConfigurationAndControl  completion =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRSubscribeParams params) => mtrBaseClusterValveConfigurationAndControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterValveConfigurationAndControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl => mtrBaseClusterValveConfigurationAndControl -> IO (Id MTRBaseClusterValveConfigurationAndControl)
init_ mtrBaseClusterValveConfigurationAndControl  =
    sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterValveConfigurationAndControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterValveConfigurationAndControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterValveConfigurationAndControl mtrBaseClusterValveConfigurationAndControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterValveConfigurationAndControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterValveConfigurationAndControl)
initWithDevice_endpointID_queue mtrBaseClusterValveConfigurationAndControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterValveConfigurationAndControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openWithParams:completion:@
openWithParams_completionSelector :: Selector
openWithParams_completionSelector = mkSelector "openWithParams:completion:"

-- | @Selector@ for @openWithCompletion:@
openWithCompletionSelector :: Selector
openWithCompletionSelector = mkSelector "openWithCompletion:"

-- | @Selector@ for @closeWithParams:completion:@
closeWithParams_completionSelector :: Selector
closeWithParams_completionSelector = mkSelector "closeWithParams:completion:"

-- | @Selector@ for @closeWithCompletion:@
closeWithCompletionSelector :: Selector
closeWithCompletionSelector = mkSelector "closeWithCompletion:"

-- | @Selector@ for @readAttributeOpenDurationWithCompletion:@
readAttributeOpenDurationWithCompletionSelector :: Selector
readAttributeOpenDurationWithCompletionSelector = mkSelector "readAttributeOpenDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOpenDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOpenDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOpenDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultOpenDurationWithCompletion:@
readAttributeDefaultOpenDurationWithCompletionSelector :: Selector
readAttributeDefaultOpenDurationWithCompletionSelector = mkSelector "readAttributeDefaultOpenDurationWithCompletion:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:completion:@
writeAttributeDefaultOpenDurationWithValue_completionSelector :: Selector
writeAttributeDefaultOpenDurationWithValue_completionSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:completion:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:params:completion:@
writeAttributeDefaultOpenDurationWithValue_params_completionSelector :: Selector
writeAttributeDefaultOpenDurationWithValue_params_completionSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultOpenDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultOpenDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultOpenDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultOpenDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAutoCloseTimeWithCompletion:@
readAttributeAutoCloseTimeWithCompletionSelector :: Selector
readAttributeAutoCloseTimeWithCompletionSelector = mkSelector "readAttributeAutoCloseTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAutoCloseTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAutoCloseTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAutoCloseTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAutoCloseTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRemainingDurationWithCompletion:@
readAttributeRemainingDurationWithCompletionSelector :: Selector
readAttributeRemainingDurationWithCompletionSelector = mkSelector "readAttributeRemainingDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRemainingDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRemainingDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRemainingDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRemainingDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletionSelector :: Selector
readAttributeCurrentStateWithCompletionSelector = mkSelector "readAttributeCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetStateWithCompletion:@
readAttributeTargetStateWithCompletionSelector :: Selector
readAttributeTargetStateWithCompletionSelector = mkSelector "readAttributeTargetStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTargetStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTargetStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentLevelWithCompletion:@
readAttributeCurrentLevelWithCompletionSelector :: Selector
readAttributeCurrentLevelWithCompletionSelector = mkSelector "readAttributeCurrentLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTargetLevelWithCompletion:@
readAttributeTargetLevelWithCompletionSelector :: Selector
readAttributeTargetLevelWithCompletionSelector = mkSelector "readAttributeTargetLevelWithCompletion:"

-- | @Selector@ for @subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTargetLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTargetLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTargetLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTargetLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultOpenLevelWithCompletion:@
readAttributeDefaultOpenLevelWithCompletionSelector :: Selector
readAttributeDefaultOpenLevelWithCompletionSelector = mkSelector "readAttributeDefaultOpenLevelWithCompletion:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:completion:@
writeAttributeDefaultOpenLevelWithValue_completionSelector :: Selector
writeAttributeDefaultOpenLevelWithValue_completionSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:params:completion:@
writeAttributeDefaultOpenLevelWithValue_params_completionSelector :: Selector
writeAttributeDefaultOpenLevelWithValue_params_completionSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultOpenLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultOpenLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultOpenLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultOpenLevelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeValveFaultWithCompletion:@
readAttributeValveFaultWithCompletionSelector :: Selector
readAttributeValveFaultWithCompletionSelector = mkSelector "readAttributeValveFaultWithCompletion:"

-- | @Selector@ for @subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeValveFaultWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeValveFaultWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:@
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeValveFaultWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeValveFaultWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLevelStepWithCompletion:@
readAttributeLevelStepWithCompletionSelector :: Selector
readAttributeLevelStepWithCompletionSelector = mkSelector "readAttributeLevelStepWithCompletion:"

-- | @Selector@ for @subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLevelStepWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLevelStepWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:@
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLevelStepWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLevelStepWithClusterStateCache:endpoint:queue:completion:"

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

