{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Channel
--
-- This cluster provides an interface for controlling the current Channel on a device.
--
-- Generated bindings for @MTRBaseClusterChannel@.
module ObjC.Matter.MTRBaseClusterChannel
  ( MTRBaseClusterChannel
  , IsMTRBaseClusterChannel(..)
  , changeChannelWithParams_completion
  , changeChannelByNumberWithParams_completion
  , skipChannelWithParams_completion
  , getProgramGuideWithParams_completion
  , getProgramGuideWithCompletion
  , recordProgramWithParams_completion
  , cancelRecordProgramWithParams_completion
  , readAttributeChannelListWithCompletion
  , subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandler
  , readAttributeChannelListWithClusterStateCache_endpoint_queue_completion
  , readAttributeLineupWithCompletion
  , subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandler
  , readAttributeLineupWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentChannelWithCompletion
  , subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completion
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
  , changeChannelWithParams_completionHandler
  , changeChannelByNumberWithParams_completionHandler
  , skipChannelWithParams_completionHandler
  , readAttributeChannelListWithCompletionHandler
  , subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLineupWithCompletionHandler
  , subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLineupWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentChannelWithCompletionHandler
  , subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandler
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
  , changeChannelWithParams_completionSelector
  , changeChannelByNumberWithParams_completionSelector
  , skipChannelWithParams_completionSelector
  , getProgramGuideWithParams_completionSelector
  , getProgramGuideWithCompletionSelector
  , recordProgramWithParams_completionSelector
  , cancelRecordProgramWithParams_completionSelector
  , readAttributeChannelListWithCompletionSelector
  , subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLineupWithCompletionSelector
  , subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentChannelWithCompletionSelector
  , subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector
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
  , changeChannelWithParams_completionHandlerSelector
  , changeChannelByNumberWithParams_completionHandlerSelector
  , skipChannelWithParams_completionHandlerSelector
  , readAttributeChannelListWithCompletionHandlerSelector
  , subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLineupWithCompletionHandlerSelector
  , subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentChannelWithCompletionHandlerSelector
  , subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command ChangeChannel
--
-- Change the channel on the media player to the channel case-insensitive exact matching the value passed as an argument.
--
-- ObjC selector: @- changeChannelWithParams:completion:@
changeChannelWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelWithParams_completion mtrBaseClusterChannel  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "changeChannelWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ChangeChannelByNumber
--
-- Change the channel on the media plaeyer to the channel with the given Number in the ChannelList attribute.
--
-- ObjC selector: @- changeChannelByNumberWithParams:completion:@
changeChannelByNumberWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelByNumberWithParams_completion mtrBaseClusterChannel  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "changeChannelByNumberWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SkipChannel
--
-- This command provides channel up and channel down functionality, but allows channel index jumps of size Count. When the value of the increase or decrease is larger than the number of channels remaining in the given direction, then the behavior SHALL be to return to the beginning (or end) of the channel list and continue. For example, if the current channel is at index 0 and count value of -1 is given, then the current channel should change to the last channel.
--
-- ObjC selector: @- skipChannelWithParams:completion:@
skipChannelWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterSkipChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
skipChannelWithParams_completion mtrBaseClusterChannel  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "skipChannelWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command GetProgramGuide
--
-- This command retrieves the program guide. It accepts several filter parameters to return specific schedule and program information from a content app. The command shall receive in response a ProgramGuideResponse.
--
-- ObjC selector: @- getProgramGuideWithParams:completion:@
getProgramGuideWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterGetProgramGuideParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
getProgramGuideWithParams_completion mtrBaseClusterChannel  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "getProgramGuideWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- getProgramGuideWithCompletion:@
getProgramGuideWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
getProgramGuideWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "getProgramGuideWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command RecordProgram
--
-- Record a specific program or series when it goes live. This functionality enables DVR recording features.
--
-- ObjC selector: @- recordProgramWithParams:completion:@
recordProgramWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterRecordProgramParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
recordProgramWithParams_completion mtrBaseClusterChannel  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "recordProgramWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command CancelRecordProgram
--
-- Cancel recording for a specific program or series.
--
-- ObjC selector: @- cancelRecordProgramWithParams:completion:@
cancelRecordProgramWithParams_completion :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterCancelRecordProgramParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
cancelRecordProgramWithParams_completion mtrBaseClusterChannel  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "cancelRecordProgramWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeChannelListWithCompletion:@
readAttributeChannelListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeChannelListWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeChannelListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:@
readAttributeChannelListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChannelListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLineupWithCompletion:@
readAttributeLineupWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeLineupWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeLineupWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLineupWithClusterStateCache:endpoint:queue:completion:@
readAttributeLineupWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLineupWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLineupWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentChannelWithCompletion:@
readAttributeCurrentChannelWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeCurrentChannelWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeCurrentChannelWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterChannel  completion =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterChannel  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> IO (Id MTRBaseClusterChannel)
init_ mtrBaseClusterChannel  =
    sendMsg mtrBaseClusterChannel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterChannel)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterChannel -> device -> CUShort -> queue -> IO (Id MTRBaseClusterChannel)
initWithDevice_endpoint_queue mtrBaseClusterChannel  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterChannel (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- changeChannelWithParams:completionHandler:@
changeChannelWithParams_completionHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelWithParams_completionHandler mtrBaseClusterChannel  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "changeChannelWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- changeChannelByNumberWithParams:completionHandler:@
changeChannelByNumberWithParams_completionHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterChangeChannelByNumberParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
changeChannelByNumberWithParams_completionHandler mtrBaseClusterChannel  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "changeChannelByNumberWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipChannelWithParams:completionHandler:@
skipChannelWithParams_completionHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRChannelClusterSkipChannelParams params) => mtrBaseClusterChannel -> params -> Ptr () -> IO ()
skipChannelWithParams_completionHandler mtrBaseClusterChannel  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterChannel (mkSelector "skipChannelWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeChannelListWithCompletionHandler:@
readAttributeChannelListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeChannelListWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeChannelListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeLineupWithCompletionHandler:@
readAttributeLineupWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeLineupWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeLineupWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentChannelWithCompletionHandler:@
readAttributeCurrentChannelWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeCurrentChannelWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeCurrentChannelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterChannel mtrBaseClusterChannel => mtrBaseClusterChannel -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterChannel  completionHandler =
    sendMsg mtrBaseClusterChannel (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterChannel -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterChannel  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterChannel (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterChannel"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterChannel mtrBaseClusterChannel, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterChannel -> device -> endpointID -> queue -> IO (Id MTRBaseClusterChannel)
initWithDevice_endpointID_queue mtrBaseClusterChannel  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterChannel (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeChannelWithParams:completion:@
changeChannelWithParams_completionSelector :: Selector
changeChannelWithParams_completionSelector = mkSelector "changeChannelWithParams:completion:"

-- | @Selector@ for @changeChannelByNumberWithParams:completion:@
changeChannelByNumberWithParams_completionSelector :: Selector
changeChannelByNumberWithParams_completionSelector = mkSelector "changeChannelByNumberWithParams:completion:"

-- | @Selector@ for @skipChannelWithParams:completion:@
skipChannelWithParams_completionSelector :: Selector
skipChannelWithParams_completionSelector = mkSelector "skipChannelWithParams:completion:"

-- | @Selector@ for @getProgramGuideWithParams:completion:@
getProgramGuideWithParams_completionSelector :: Selector
getProgramGuideWithParams_completionSelector = mkSelector "getProgramGuideWithParams:completion:"

-- | @Selector@ for @getProgramGuideWithCompletion:@
getProgramGuideWithCompletionSelector :: Selector
getProgramGuideWithCompletionSelector = mkSelector "getProgramGuideWithCompletion:"

-- | @Selector@ for @recordProgramWithParams:completion:@
recordProgramWithParams_completionSelector :: Selector
recordProgramWithParams_completionSelector = mkSelector "recordProgramWithParams:completion:"

-- | @Selector@ for @cancelRecordProgramWithParams:completion:@
cancelRecordProgramWithParams_completionSelector :: Selector
cancelRecordProgramWithParams_completionSelector = mkSelector "cancelRecordProgramWithParams:completion:"

-- | @Selector@ for @readAttributeChannelListWithCompletion:@
readAttributeChannelListWithCompletionSelector :: Selector
readAttributeChannelListWithCompletionSelector = mkSelector "readAttributeChannelListWithCompletion:"

-- | @Selector@ for @subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeChannelListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChannelListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:@
readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeChannelListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeChannelListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLineupWithCompletion:@
readAttributeLineupWithCompletionSelector :: Selector
readAttributeLineupWithCompletionSelector = mkSelector "readAttributeLineupWithCompletion:"

-- | @Selector@ for @subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLineupWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLineupWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLineupWithClusterStateCache:endpoint:queue:completion:@
readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLineupWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLineupWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentChannelWithCompletion:@
readAttributeCurrentChannelWithCompletionSelector :: Selector
readAttributeCurrentChannelWithCompletionSelector = mkSelector "readAttributeCurrentChannelWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentChannelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentChannelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentChannelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentChannelWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @changeChannelWithParams:completionHandler:@
changeChannelWithParams_completionHandlerSelector :: Selector
changeChannelWithParams_completionHandlerSelector = mkSelector "changeChannelWithParams:completionHandler:"

-- | @Selector@ for @changeChannelByNumberWithParams:completionHandler:@
changeChannelByNumberWithParams_completionHandlerSelector :: Selector
changeChannelByNumberWithParams_completionHandlerSelector = mkSelector "changeChannelByNumberWithParams:completionHandler:"

-- | @Selector@ for @skipChannelWithParams:completionHandler:@
skipChannelWithParams_completionHandlerSelector :: Selector
skipChannelWithParams_completionHandlerSelector = mkSelector "skipChannelWithParams:completionHandler:"

-- | @Selector@ for @readAttributeChannelListWithCompletionHandler:@
readAttributeChannelListWithCompletionHandlerSelector :: Selector
readAttributeChannelListWithCompletionHandlerSelector = mkSelector "readAttributeChannelListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeChannelListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeChannelListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeChannelListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeChannelListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLineupWithCompletionHandler:@
readAttributeLineupWithCompletionHandlerSelector :: Selector
readAttributeLineupWithCompletionHandlerSelector = mkSelector "readAttributeLineupWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLineupWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLineupWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeLineupWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLineupWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentChannelWithCompletionHandler:@
readAttributeCurrentChannelWithCompletionHandlerSelector :: Selector
readAttributeCurrentChannelWithCompletionHandlerSelector = mkSelector "readAttributeCurrentChannelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentChannelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentChannelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentChannelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentChannelWithAttributeCache:endpoint:queue:completionHandler:"

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

