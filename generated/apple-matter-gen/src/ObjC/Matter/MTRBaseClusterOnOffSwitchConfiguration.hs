{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/off Switch Configuration
--
-- Attributes and commands for configuring On/Off switching devices.
--
-- Generated bindings for @MTRBaseClusterOnOffSwitchConfiguration@.
module ObjC.Matter.MTRBaseClusterOnOffSwitchConfiguration
  ( MTRBaseClusterOnOffSwitchConfiguration
  , IsMTRBaseClusterOnOffSwitchConfiguration(..)
  , readAttributeSwitchTypeWithCompletion
  , subscribeAttributeSwitchTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSwitchTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeSwitchActionsWithCompletion
  , writeAttributeSwitchActionsWithValue_completion
  , writeAttributeSwitchActionsWithValue_params_completion
  , subscribeAttributeSwitchActionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSwitchActionsWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeSwitchTypeWithCompletionHandler
  , subscribeAttributeSwitchTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSwitchTypeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSwitchActionsWithCompletionHandler
  , writeAttributeSwitchActionsWithValue_completionHandler
  , writeAttributeSwitchActionsWithValue_params_completionHandler
  , subscribeAttributeSwitchActionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSwitchActionsWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeSwitchTypeWithCompletionSelector
  , subscribeAttributeSwitchTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSwitchTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSwitchActionsWithCompletionSelector
  , writeAttributeSwitchActionsWithValue_completionSelector
  , writeAttributeSwitchActionsWithValue_params_completionSelector
  , subscribeAttributeSwitchActionsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSwitchActionsWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeSwitchTypeWithCompletionHandlerSelector
  , subscribeAttributeSwitchTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSwitchTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSwitchActionsWithCompletionHandlerSelector
  , writeAttributeSwitchActionsWithValue_completionHandlerSelector
  , writeAttributeSwitchActionsWithValue_params_completionHandlerSelector
  , subscribeAttributeSwitchActionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSwitchActionsWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributeSwitchTypeWithCompletion:@
readAttributeSwitchTypeWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeSwitchTypeWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeSwitchTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSwitchTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSwitchTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeSwitchTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSwitchTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSwitchTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSwitchTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSwitchTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSwitchActionsWithCompletion:@
readAttributeSwitchActionsWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeSwitchActionsWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeSwitchActionsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSwitchActionsWithValue:completion:@
writeAttributeSwitchActionsWithValue_completion :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber value) => mtrBaseClusterOnOffSwitchConfiguration -> value -> Ptr () -> IO ()
writeAttributeSwitchActionsWithValue_completion mtrBaseClusterOnOffSwitchConfiguration  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "writeAttributeSwitchActionsWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSwitchActionsWithValue:params:completion:@
writeAttributeSwitchActionsWithValue_params_completion :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOffSwitchConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeSwitchActionsWithValue_params_completion mtrBaseClusterOnOffSwitchConfiguration  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "writeAttributeSwitchActionsWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSwitchActionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchActionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSwitchActionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeSwitchActionsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSwitchActionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSwitchActionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSwitchActionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSwitchActionsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOnOffSwitchConfiguration  completion =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> IO (Id MTRBaseClusterOnOffSwitchConfiguration)
init_ mtrBaseClusterOnOffSwitchConfiguration  =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterOnOffSwitchConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterOnOffSwitchConfiguration -> device -> CUShort -> queue -> IO (Id MTRBaseClusterOnOffSwitchConfiguration)
initWithDevice_endpoint_queue mtrBaseClusterOnOffSwitchConfiguration  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeSwitchTypeWithCompletionHandler:@
readAttributeSwitchTypeWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeSwitchTypeWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeSwitchTypeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSwitchTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSwitchTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeSwitchTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSwitchTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSwitchTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSwitchTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSwitchTypeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSwitchActionsWithCompletionHandler:@
readAttributeSwitchActionsWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeSwitchActionsWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeSwitchActionsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeSwitchActionsWithValue:completionHandler:@
writeAttributeSwitchActionsWithValue_completionHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber value) => mtrBaseClusterOnOffSwitchConfiguration -> value -> Ptr () -> IO ()
writeAttributeSwitchActionsWithValue_completionHandler mtrBaseClusterOnOffSwitchConfiguration  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "writeAttributeSwitchActionsWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeSwitchActionsWithValue:params:completionHandler:@
writeAttributeSwitchActionsWithValue_params_completionHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterOnOffSwitchConfiguration -> value -> params -> Ptr () -> IO ()
writeAttributeSwitchActionsWithValue_params_completionHandler mtrBaseClusterOnOffSwitchConfiguration  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "writeAttributeSwitchActionsWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSwitchActionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchActionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSwitchActionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeSwitchActionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSwitchActionsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSwitchActionsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSwitchActionsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSwitchActionsWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration => mtrBaseClusterOnOffSwitchConfiguration -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterOnOffSwitchConfiguration  completionHandler =
    sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOnOffSwitchConfiguration -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOnOffSwitchConfiguration  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOnOffSwitchConfiguration"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOnOffSwitchConfiguration mtrBaseClusterOnOffSwitchConfiguration, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOnOffSwitchConfiguration -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOnOffSwitchConfiguration)
initWithDevice_endpointID_queue mtrBaseClusterOnOffSwitchConfiguration  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterOnOffSwitchConfiguration (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSwitchTypeWithCompletion:@
readAttributeSwitchTypeWithCompletionSelector :: Selector
readAttributeSwitchTypeWithCompletionSelector = mkSelector "readAttributeSwitchTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeSwitchTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSwitchTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSwitchTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSwitchTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSwitchTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSwitchTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSwitchTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSwitchActionsWithCompletion:@
readAttributeSwitchActionsWithCompletionSelector :: Selector
readAttributeSwitchActionsWithCompletionSelector = mkSelector "readAttributeSwitchActionsWithCompletion:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:completion:@
writeAttributeSwitchActionsWithValue_completionSelector :: Selector
writeAttributeSwitchActionsWithValue_completionSelector = mkSelector "writeAttributeSwitchActionsWithValue:completion:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:params:completion:@
writeAttributeSwitchActionsWithValue_params_completionSelector :: Selector
writeAttributeSwitchActionsWithValue_params_completionSelector = mkSelector "writeAttributeSwitchActionsWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSwitchActionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchActionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSwitchActionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSwitchActionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSwitchActionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSwitchActionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSwitchActionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSwitchActionsWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeSwitchTypeWithCompletionHandler:@
readAttributeSwitchTypeWithCompletionHandlerSelector :: Selector
readAttributeSwitchTypeWithCompletionHandlerSelector = mkSelector "readAttributeSwitchTypeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSwitchTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSwitchTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSwitchTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSwitchTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSwitchTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSwitchTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSwitchTypeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSwitchActionsWithCompletionHandler:@
readAttributeSwitchActionsWithCompletionHandlerSelector :: Selector
readAttributeSwitchActionsWithCompletionHandlerSelector = mkSelector "readAttributeSwitchActionsWithCompletionHandler:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:completionHandler:@
writeAttributeSwitchActionsWithValue_completionHandlerSelector :: Selector
writeAttributeSwitchActionsWithValue_completionHandlerSelector = mkSelector "writeAttributeSwitchActionsWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:params:completionHandler:@
writeAttributeSwitchActionsWithValue_params_completionHandlerSelector :: Selector
writeAttributeSwitchActionsWithValue_params_completionHandlerSelector = mkSelector "writeAttributeSwitchActionsWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeSwitchActionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSwitchActionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSwitchActionsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSwitchActionsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSwitchActionsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSwitchActionsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSwitchActionsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSwitchActionsWithAttributeCache:endpoint:queue:completionHandler:"

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

