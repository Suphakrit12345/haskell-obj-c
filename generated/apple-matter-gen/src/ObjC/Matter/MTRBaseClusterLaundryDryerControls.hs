{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Dryer Controls
--
-- This cluster provides a way to access options associated with the operation of            a laundry dryer device type.
--
-- Generated bindings for @MTRBaseClusterLaundryDryerControls@.
module ObjC.Matter.MTRBaseClusterLaundryDryerControls
  ( MTRBaseClusterLaundryDryerControls
  , IsMTRBaseClusterLaundryDryerControls(..)
  , readAttributeSupportedDrynessLevelsWithCompletion
  , subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedDrynessLevelWithCompletion
  , writeAttributeSelectedDrynessLevelWithValue_completion
  , writeAttributeSelectedDrynessLevelWithValue_params_completion
  , subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeSupportedDrynessLevelsWithCompletionSelector
  , subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedDrynessLevelWithCompletionSelector
  , writeAttributeSelectedDrynessLevelWithValue_completionSelector
  , writeAttributeSelectedDrynessLevelWithValue_params_completionSelector
  , subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector
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

-- | @- readAttributeSupportedDrynessLevelsWithCompletion:@
readAttributeSupportedDrynessLevelsWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeSupportedDrynessLevelsWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeSupportedDrynessLevelsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSelectedDrynessLevelWithCompletion:@
readAttributeSelectedDrynessLevelWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeSelectedDrynessLevelWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeSelectedDrynessLevelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSelectedDrynessLevelWithValue:completion:@
writeAttributeSelectedDrynessLevelWithValue_completion :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsNSNumber value) => mtrBaseClusterLaundryDryerControls -> value -> Ptr () -> IO ()
writeAttributeSelectedDrynessLevelWithValue_completion mtrBaseClusterLaundryDryerControls  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "writeAttributeSelectedDrynessLevelWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSelectedDrynessLevelWithValue:params:completion:@
writeAttributeSelectedDrynessLevelWithValue_params_completion :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterLaundryDryerControls -> value -> params -> Ptr () -> IO ()
writeAttributeSelectedDrynessLevelWithValue_params_completion mtrBaseClusterLaundryDryerControls  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "writeAttributeSelectedDrynessLevelWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterLaundryDryerControls  completion =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRSubscribeParams params) => mtrBaseClusterLaundryDryerControls -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterLaundryDryerControls  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls => mtrBaseClusterLaundryDryerControls -> IO (Id MTRBaseClusterLaundryDryerControls)
init_ mtrBaseClusterLaundryDryerControls  =
    sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterLaundryDryerControls)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterLaundryDryerControls"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterLaundryDryerControls mtrBaseClusterLaundryDryerControls, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterLaundryDryerControls -> device -> endpointID -> queue -> IO (Id MTRBaseClusterLaundryDryerControls)
initWithDevice_endpointID_queue mtrBaseClusterLaundryDryerControls  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterLaundryDryerControls (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportedDrynessLevelsWithCompletion:@
readAttributeSupportedDrynessLevelsWithCompletionSelector :: Selector
readAttributeSupportedDrynessLevelsWithCompletionSelector = mkSelector "readAttributeSupportedDrynessLevelsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedDrynessLevelsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedDrynessLevelsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedDrynessLevelsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedDrynessLevelsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedDrynessLevelWithCompletion:@
readAttributeSelectedDrynessLevelWithCompletionSelector :: Selector
readAttributeSelectedDrynessLevelWithCompletionSelector = mkSelector "readAttributeSelectedDrynessLevelWithCompletion:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:completion:@
writeAttributeSelectedDrynessLevelWithValue_completionSelector :: Selector
writeAttributeSelectedDrynessLevelWithValue_completionSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:completion:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:params:completion:@
writeAttributeSelectedDrynessLevelWithValue_params_completionSelector :: Selector
writeAttributeSelectedDrynessLevelWithValue_params_completionSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSelectedDrynessLevelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedDrynessLevelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSelectedDrynessLevelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedDrynessLevelWithClusterStateCache:endpoint:queue:completion:"

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

