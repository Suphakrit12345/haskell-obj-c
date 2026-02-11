{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Chime
--
-- This cluster provides facilities to configure and play Chime sounds, such as those used in a doorbell.
--
-- Generated bindings for @MTRBaseClusterChime@.
module ObjC.Matter.MTRBaseClusterChime
  ( MTRBaseClusterChime
  , IsMTRBaseClusterChime(..)
  , playChimeSoundWithParams_completion
  , playChimeSoundWithCompletion
  , readAttributeInstalledChimeSoundsWithCompletion
  , subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandler
  , readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSelectedChimeWithCompletion
  , writeAttributeSelectedChimeWithValue_completion
  , writeAttributeSelectedChimeWithValue_params_completion
  , subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeEnabledWithCompletion
  , writeAttributeEnabledWithValue_completion
  , writeAttributeEnabledWithValue_params_completion
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completion
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
  , playChimeSoundWithParams_completionSelector
  , playChimeSoundWithCompletionSelector
  , readAttributeInstalledChimeSoundsWithCompletionSelector
  , subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSelectedChimeWithCompletionSelector
  , writeAttributeSelectedChimeWithValue_completionSelector
  , writeAttributeSelectedChimeWithValue_params_completionSelector
  , subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeEnabledWithCompletionSelector
  , writeAttributeEnabledWithValue_completionSelector
  , writeAttributeEnabledWithValue_params_completionSelector
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command PlayChimeSound
--
-- ObjC selector: @- playChimeSoundWithParams:completion:@
playChimeSoundWithParams_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRChimeClusterPlayChimeSoundParams params) => mtrBaseClusterChime -> params -> Ptr () -> IO ()
playChimeSoundWithParams_completion mtrBaseClusterChime  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "playChimeSoundWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- playChimeSoundWithCompletion:@
playChimeSoundWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
playChimeSoundWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "playChimeSoundWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInstalledChimeSoundsWithCompletion:@
readAttributeInstalledChimeSoundsWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeInstalledChimeSoundsWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeInstalledChimeSoundsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:@
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSelectedChimeWithCompletion:@
readAttributeSelectedChimeWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeSelectedChimeWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeSelectedChimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSelectedChimeWithValue:completion:@
writeAttributeSelectedChimeWithValue_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value) => mtrBaseClusterChime -> value -> Ptr () -> IO ()
writeAttributeSelectedChimeWithValue_completion mtrBaseClusterChime  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterChime (mkSelector "writeAttributeSelectedChimeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeSelectedChimeWithValue:params:completion:@
writeAttributeSelectedChimeWithValue_params_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterChime -> value -> params -> Ptr () -> IO ()
writeAttributeSelectedChimeWithValue_params_completion mtrBaseClusterChime  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterChime (mkSelector "writeAttributeSelectedChimeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeEnabledWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeEnabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeEnabledWithValue:completion:@
writeAttributeEnabledWithValue_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value) => mtrBaseClusterChime -> value -> Ptr () -> IO ()
writeAttributeEnabledWithValue_completion mtrBaseClusterChime  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterChime (mkSelector "writeAttributeEnabledWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeEnabledWithValue:params:completion:@
writeAttributeEnabledWithValue_params_completion :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterChime -> value -> params -> Ptr () -> IO ()
writeAttributeEnabledWithValue_params_completion mtrBaseClusterChime  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterChime (mkSelector "writeAttributeEnabledWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterChime  completion =
    sendMsg mtrBaseClusterChime (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRSubscribeParams params) => mtrBaseClusterChime -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChime  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChime (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterChime mtrBaseClusterChime => mtrBaseClusterChime -> IO (Id MTRBaseClusterChime)
init_ mtrBaseClusterChime  =
    sendMsg mtrBaseClusterChime (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterChime)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterChime"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterChime mtrBaseClusterChime, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterChime -> device -> endpointID -> queue -> IO (Id MTRBaseClusterChime)
initWithDevice_endpointID_queue mtrBaseClusterChime  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterChime (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playChimeSoundWithParams:completion:@
playChimeSoundWithParams_completionSelector :: Selector
playChimeSoundWithParams_completionSelector = mkSelector "playChimeSoundWithParams:completion:"

-- | @Selector@ for @playChimeSoundWithCompletion:@
playChimeSoundWithCompletionSelector :: Selector
playChimeSoundWithCompletionSelector = mkSelector "playChimeSoundWithCompletion:"

-- | @Selector@ for @readAttributeInstalledChimeSoundsWithCompletion:@
readAttributeInstalledChimeSoundsWithCompletionSelector :: Selector
readAttributeInstalledChimeSoundsWithCompletionSelector = mkSelector "readAttributeInstalledChimeSoundsWithCompletion:"

-- | @Selector@ for @subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeInstalledChimeSoundsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeInstalledChimeSoundsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:@
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeInstalledChimeSoundsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeInstalledChimeSoundsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSelectedChimeWithCompletion:@
readAttributeSelectedChimeWithCompletionSelector :: Selector
readAttributeSelectedChimeWithCompletionSelector = mkSelector "readAttributeSelectedChimeWithCompletion:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:completion:@
writeAttributeSelectedChimeWithValue_completionSelector :: Selector
writeAttributeSelectedChimeWithValue_completionSelector = mkSelector "writeAttributeSelectedChimeWithValue:completion:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:params:completion:@
writeAttributeSelectedChimeWithValue_params_completionSelector :: Selector
writeAttributeSelectedChimeWithValue_params_completionSelector = mkSelector "writeAttributeSelectedChimeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSelectedChimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSelectedChimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSelectedChimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSelectedChimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletionSelector :: Selector
readAttributeEnabledWithCompletionSelector = mkSelector "readAttributeEnabledWithCompletion:"

-- | @Selector@ for @writeAttributeEnabledWithValue:completion:@
writeAttributeEnabledWithValue_completionSelector :: Selector
writeAttributeEnabledWithValue_completionSelector = mkSelector "writeAttributeEnabledWithValue:completion:"

-- | @Selector@ for @writeAttributeEnabledWithValue:params:completion:@
writeAttributeEnabledWithValue_params_completionSelector :: Selector
writeAttributeEnabledWithValue_params_completionSelector = mkSelector "writeAttributeEnabledWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:"

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

