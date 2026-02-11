{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Format Localization
--
-- Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing preferences for how dates and times are conveyed. As such, Nodes that visually      or audibly convey time information need a mechanism by which they can be configured to use a      user’s preferred format.
--
-- Generated bindings for @MTRBaseClusterTimeFormatLocalization@.
module ObjC.Matter.MTRBaseClusterTimeFormatLocalization
  ( MTRBaseClusterTimeFormatLocalization
  , IsMTRBaseClusterTimeFormatLocalization(..)
  , readAttributeHourFormatWithCompletion
  , writeAttributeHourFormatWithValue_completion
  , writeAttributeHourFormatWithValue_params_completion
  , subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandler
  , readAttributeHourFormatWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveCalendarTypeWithCompletion
  , writeAttributeActiveCalendarTypeWithValue_completion
  , writeAttributeActiveCalendarTypeWithValue_params_completion
  , subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedCalendarTypesWithCompletion
  , subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeHourFormatWithCompletionHandler
  , writeAttributeHourFormatWithValue_completionHandler
  , writeAttributeHourFormatWithValue_params_completionHandler
  , subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeActiveCalendarTypeWithCompletionHandler
  , writeAttributeActiveCalendarTypeWithValue_completionHandler
  , writeAttributeActiveCalendarTypeWithValue_params_completionHandler
  , subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedCalendarTypesWithCompletionHandler
  , subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandler
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
  , readAttributeHourFormatWithCompletionSelector
  , writeAttributeHourFormatWithValue_completionSelector
  , writeAttributeHourFormatWithValue_params_completionSelector
  , subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveCalendarTypeWithCompletionSelector
  , writeAttributeActiveCalendarTypeWithValue_completionSelector
  , writeAttributeActiveCalendarTypeWithValue_params_completionSelector
  , subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedCalendarTypesWithCompletionSelector
  , subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector
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
  , readAttributeHourFormatWithCompletionHandlerSelector
  , writeAttributeHourFormatWithValue_completionHandlerSelector
  , writeAttributeHourFormatWithValue_params_completionHandlerSelector
  , subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeActiveCalendarTypeWithCompletionHandlerSelector
  , writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector
  , writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector
  , subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedCalendarTypesWithCompletionHandlerSelector
  , subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | @- readAttributeHourFormatWithCompletion:@
readAttributeHourFormatWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeHourFormatWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeHourFormatWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeHourFormatWithValue:completion:@
writeAttributeHourFormatWithValue_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_completion mtrBaseClusterTimeFormatLocalization  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeHourFormatWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeHourFormatWithValue:params:completion:@
writeAttributeHourFormatWithValue_params_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_params_completion mtrBaseClusterTimeFormatLocalization  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeHourFormatWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:@
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveCalendarTypeWithCompletion:@
readAttributeActiveCalendarTypeWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeActiveCalendarTypeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeActiveCalendarTypeWithValue:completion:@
writeAttributeActiveCalendarTypeWithValue_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_completion mtrBaseClusterTimeFormatLocalization  value completion =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeActiveCalendarTypeWithValue:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- writeAttributeActiveCalendarTypeWithValue:params:completion:@
writeAttributeActiveCalendarTypeWithValue_params_completion :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_params_completion mtrBaseClusterTimeFormatLocalization  value params completion =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeActiveCalendarTypeWithValue:params:completion:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportedCalendarTypesWithCompletion:@
readAttributeSupportedCalendarTypesWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeSupportedCalendarTypesWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTimeFormatLocalization  completion =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> IO (Id MTRBaseClusterTimeFormatLocalization)
init_ mtrBaseClusterTimeFormatLocalization  =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterTimeFormatLocalization)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterTimeFormatLocalization -> device -> CUShort -> queue -> IO (Id MTRBaseClusterTimeFormatLocalization)
initWithDevice_endpoint_queue mtrBaseClusterTimeFormatLocalization  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- readAttributeHourFormatWithCompletionHandler:@
readAttributeHourFormatWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeHourFormatWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeHourFormatWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeHourFormatWithValue:completionHandler:@
writeAttributeHourFormatWithValue_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_completionHandler mtrBaseClusterTimeFormatLocalization  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeHourFormatWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeHourFormatWithValue:params:completionHandler:@
writeAttributeHourFormatWithValue_params_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeHourFormatWithValue_params_completionHandler mtrBaseClusterTimeFormatLocalization  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeHourFormatWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeActiveCalendarTypeWithCompletionHandler:@
readAttributeActiveCalendarTypeWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeActiveCalendarTypeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeActiveCalendarTypeWithValue:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value) => mtrBaseClusterTimeFormatLocalization -> value -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_completionHandler mtrBaseClusterTimeFormatLocalization  value completionHandler =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeActiveCalendarTypeWithValue:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- writeAttributeActiveCalendarTypeWithValue:params:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_params_completionHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterTimeFormatLocalization -> value -> params -> Ptr () -> IO ()
writeAttributeActiveCalendarTypeWithValue_params_completionHandler mtrBaseClusterTimeFormatLocalization  value params completionHandler =
  withObjCPtr value $ \raw_value ->
    withObjCPtr params $ \raw_params ->
        sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "writeAttributeActiveCalendarTypeWithValue:params:completionHandler:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSupportedCalendarTypesWithCompletionHandler:@
readAttributeSupportedCalendarTypesWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeSupportedCalendarTypesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization => mtrBaseClusterTimeFormatLocalization -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterTimeFormatLocalization  completionHandler =
    sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterTimeFormatLocalization -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterTimeFormatLocalization  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeFormatLocalization"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTimeFormatLocalization mtrBaseClusterTimeFormatLocalization, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTimeFormatLocalization -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTimeFormatLocalization)
initWithDevice_endpointID_queue mtrBaseClusterTimeFormatLocalization  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterTimeFormatLocalization (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeHourFormatWithCompletion:@
readAttributeHourFormatWithCompletionSelector :: Selector
readAttributeHourFormatWithCompletionSelector = mkSelector "readAttributeHourFormatWithCompletion:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:completion:@
writeAttributeHourFormatWithValue_completionSelector :: Selector
writeAttributeHourFormatWithValue_completionSelector = mkSelector "writeAttributeHourFormatWithValue:completion:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:params:completion:@
writeAttributeHourFormatWithValue_params_completionSelector :: Selector
writeAttributeHourFormatWithValue_params_completionSelector = mkSelector "writeAttributeHourFormatWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHourFormatWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHourFormatWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:@
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeHourFormatWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHourFormatWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithCompletion:@
readAttributeActiveCalendarTypeWithCompletionSelector :: Selector
readAttributeActiveCalendarTypeWithCompletionSelector = mkSelector "readAttributeActiveCalendarTypeWithCompletion:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:completion:@
writeAttributeActiveCalendarTypeWithValue_completionSelector :: Selector
writeAttributeActiveCalendarTypeWithValue_completionSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:completion:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:params:completion:@
writeAttributeActiveCalendarTypeWithValue_params_completionSelector :: Selector
writeAttributeActiveCalendarTypeWithValue_params_completionSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveCalendarTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveCalendarTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveCalendarTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveCalendarTypeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithCompletion:@
readAttributeSupportedCalendarTypesWithCompletionSelector :: Selector
readAttributeSupportedCalendarTypesWithCompletionSelector = mkSelector "readAttributeSupportedCalendarTypesWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedCalendarTypesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedCalendarTypesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportedCalendarTypesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedCalendarTypesWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @readAttributeHourFormatWithCompletionHandler:@
readAttributeHourFormatWithCompletionHandlerSelector :: Selector
readAttributeHourFormatWithCompletionHandlerSelector = mkSelector "readAttributeHourFormatWithCompletionHandler:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:completionHandler:@
writeAttributeHourFormatWithValue_completionHandlerSelector :: Selector
writeAttributeHourFormatWithValue_completionHandlerSelector = mkSelector "writeAttributeHourFormatWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:params:completionHandler:@
writeAttributeHourFormatWithValue_params_completionHandlerSelector :: Selector
writeAttributeHourFormatWithValue_params_completionHandlerSelector = mkSelector "writeAttributeHourFormatWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeHourFormatWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHourFormatWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeHourFormatWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeHourFormatWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithCompletionHandler:@
readAttributeActiveCalendarTypeWithCompletionHandlerSelector :: Selector
readAttributeActiveCalendarTypeWithCompletionHandlerSelector = mkSelector "readAttributeActiveCalendarTypeWithCompletionHandler:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector :: Selector
writeAttributeActiveCalendarTypeWithValue_completionHandlerSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:params:completionHandler:@
writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector :: Selector
writeAttributeActiveCalendarTypeWithValue_params_completionHandlerSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveCalendarTypeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveCalendarTypeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeActiveCalendarTypeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeActiveCalendarTypeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithCompletionHandler:@
readAttributeSupportedCalendarTypesWithCompletionHandlerSelector :: Selector
readAttributeSupportedCalendarTypesWithCompletionHandlerSelector = mkSelector "readAttributeSupportedCalendarTypesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportedCalendarTypesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedCalendarTypesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSupportedCalendarTypesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedCalendarTypesWithAttributeCache:endpoint:queue:completionHandler:"

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

