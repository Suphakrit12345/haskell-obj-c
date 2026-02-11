{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Synchronization
--
-- Accurate time is required for a number of reasons, including scheduling, display and validating security materials.
--
-- Generated bindings for @MTRBaseClusterTimeSynchronization@.
module ObjC.Matter.MTRBaseClusterTimeSynchronization
  ( MTRBaseClusterTimeSynchronization
  , IsMTRBaseClusterTimeSynchronization(..)
  , setUTCTimeWithParams_completion
  , setTrustedTimeSourceWithParams_completion
  , setTimeZoneWithParams_completion
  , setDSTOffsetWithParams_completion
  , setDefaultNTPWithParams_completion
  , readAttributeUTCTimeWithCompletion
  , subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeGranularityWithCompletion
  , subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandler
  , readAttributeGranularityWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeSourceWithCompletion
  , subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completion
  , readAttributeTrustedTimeSourceWithCompletion
  , subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandler
  , readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultNTPWithCompletion
  , subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeZoneWithCompletion
  , subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completion
  , readAttributeDSTOffsetWithCompletion
  , subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandler
  , readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocalTimeWithCompletion
  , subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeZoneDatabaseWithCompletion
  , subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completion
  , readAttributeNTPServerAvailableWithCompletion
  , subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandler
  , readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeZoneListMaxSizeWithCompletion
  , subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completion
  , readAttributeDSTOffsetListMaxSizeWithCompletion
  , subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportsDNSResolveWithCompletion
  , subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completion
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
  , setUTCTimeWithParams_completionSelector
  , setTrustedTimeSourceWithParams_completionSelector
  , setTimeZoneWithParams_completionSelector
  , setDSTOffsetWithParams_completionSelector
  , setDefaultNTPWithParams_completionSelector
  , readAttributeUTCTimeWithCompletionSelector
  , subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGranularityWithCompletionSelector
  , subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeSourceWithCompletionSelector
  , subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTrustedTimeSourceWithCompletionSelector
  , subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultNTPWithCompletionSelector
  , subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeZoneWithCompletionSelector
  , subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDSTOffsetWithCompletionSelector
  , subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocalTimeWithCompletionSelector
  , subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeZoneDatabaseWithCompletionSelector
  , subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNTPServerAvailableWithCompletionSelector
  , subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeZoneListMaxSizeWithCompletionSelector
  , subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDSTOffsetListMaxSizeWithCompletionSelector
  , subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportsDNSResolveWithCompletionSelector
  , subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command SetUTCTime
--
-- This command is used to set the UTC time of the node.
--
-- ObjC selector: @- setUTCTimeWithParams:completion:@
setUTCTimeWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetUTCTimeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setUTCTimeWithParams_completion mtrBaseClusterTimeSynchronization  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "setUTCTimeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetTrustedTimeSource
--
-- This command is used to set the TrustedTimeSource attribute.
--
-- ObjC selector: @- setTrustedTimeSourceWithParams:completion:@
setTrustedTimeSourceWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTrustedTimeSourceParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setTrustedTimeSourceWithParams_completion mtrBaseClusterTimeSynchronization  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "setTrustedTimeSourceWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetTimeZone
--
-- This command is used to set the time zone of the node.
--
-- ObjC selector: @- setTimeZoneWithParams:completion:@
setTimeZoneWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTimeZoneParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setTimeZoneWithParams_completion mtrBaseClusterTimeSynchronization  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "setTimeZoneWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetDSTOffset
--
-- This command is used to set the DST offsets for a node.
--
-- ObjC selector: @- setDSTOffsetWithParams:completion:@
setDSTOffsetWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDSTOffsetParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setDSTOffsetWithParams_completion mtrBaseClusterTimeSynchronization  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "setDSTOffsetWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetDefaultNTP
--
-- This command is used to set the DefaultNTP attribute.
--
-- ObjC selector: @- setDefaultNTPWithParams:completion:@
setDefaultNTPWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDefaultNTPParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setDefaultNTPWithParams_completion mtrBaseClusterTimeSynchronization  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "setDefaultNTPWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUTCTimeWithCompletion:@
readAttributeUTCTimeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeUTCTimeWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeUTCTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGranularityWithCompletion:@
readAttributeGranularityWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeGranularityWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeGranularityWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:@
readAttributeGranularityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGranularityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimeSourceWithCompletion:@
readAttributeTimeSourceWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeSourceWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeTimeSourceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTrustedTimeSourceWithCompletion:@
readAttributeTrustedTimeSourceWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTrustedTimeSourceWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeTrustedTimeSourceWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDefaultNTPWithCompletion:@
readAttributeDefaultNTPWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeDefaultNTPWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeDefaultNTPWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimeZoneWithCompletion:@
readAttributeTimeZoneWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeZoneWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeTimeZoneWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDSTOffsetWithCompletion:@
readAttributeDSTOffsetWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeDSTOffsetWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeDSTOffsetWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeLocalTimeWithCompletion:@
readAttributeLocalTimeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeLocalTimeWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeLocalTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimeZoneDatabaseWithCompletion:@
readAttributeTimeZoneDatabaseWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeZoneDatabaseWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeTimeZoneDatabaseWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeNTPServerAvailableWithCompletion:@
readAttributeNTPServerAvailableWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeNTPServerAvailableWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeNTPServerAvailableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeTimeZoneListMaxSizeWithCompletion:@
readAttributeTimeZoneListMaxSizeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeZoneListMaxSizeWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeTimeZoneListMaxSizeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDSTOffsetListMaxSizeWithCompletion:@
readAttributeDSTOffsetListMaxSizeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeDSTOffsetListMaxSizeWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeDSTOffsetListMaxSizeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSupportsDNSResolveWithCompletion:@
readAttributeSupportsDNSResolveWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeSupportsDNSResolveWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeSupportsDNSResolveWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTimeSynchronization  completion =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterTimeSynchronization (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> IO (Id MTRBaseClusterTimeSynchronization)
init_ mtrBaseClusterTimeSynchronization  =
    sendMsg mtrBaseClusterTimeSynchronization (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterTimeSynchronization)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTimeSynchronization -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTimeSynchronization)
initWithDevice_endpointID_queue mtrBaseClusterTimeSynchronization  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterTimeSynchronization (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setUTCTimeWithParams:completion:@
setUTCTimeWithParams_completionSelector :: Selector
setUTCTimeWithParams_completionSelector = mkSelector "setUTCTimeWithParams:completion:"

-- | @Selector@ for @setTrustedTimeSourceWithParams:completion:@
setTrustedTimeSourceWithParams_completionSelector :: Selector
setTrustedTimeSourceWithParams_completionSelector = mkSelector "setTrustedTimeSourceWithParams:completion:"

-- | @Selector@ for @setTimeZoneWithParams:completion:@
setTimeZoneWithParams_completionSelector :: Selector
setTimeZoneWithParams_completionSelector = mkSelector "setTimeZoneWithParams:completion:"

-- | @Selector@ for @setDSTOffsetWithParams:completion:@
setDSTOffsetWithParams_completionSelector :: Selector
setDSTOffsetWithParams_completionSelector = mkSelector "setDSTOffsetWithParams:completion:"

-- | @Selector@ for @setDefaultNTPWithParams:completion:@
setDefaultNTPWithParams_completionSelector :: Selector
setDefaultNTPWithParams_completionSelector = mkSelector "setDefaultNTPWithParams:completion:"

-- | @Selector@ for @readAttributeUTCTimeWithCompletion:@
readAttributeUTCTimeWithCompletionSelector :: Selector
readAttributeUTCTimeWithCompletionSelector = mkSelector "readAttributeUTCTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGranularityWithCompletion:@
readAttributeGranularityWithCompletionSelector :: Selector
readAttributeGranularityWithCompletionSelector = mkSelector "readAttributeGranularityWithCompletion:"

-- | @Selector@ for @subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:@
readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeSourceWithCompletion:@
readAttributeTimeSourceWithCompletionSelector :: Selector
readAttributeTimeSourceWithCompletionSelector = mkSelector "readAttributeTimeSourceWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTrustedTimeSourceWithCompletion:@
readAttributeTrustedTimeSourceWithCompletionSelector :: Selector
readAttributeTrustedTimeSourceWithCompletionSelector = mkSelector "readAttributeTrustedTimeSourceWithCompletion:"

-- | @Selector@ for @subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultNTPWithCompletion:@
readAttributeDefaultNTPWithCompletionSelector :: Selector
readAttributeDefaultNTPWithCompletionSelector = mkSelector "readAttributeDefaultNTPWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeZoneWithCompletion:@
readAttributeTimeZoneWithCompletionSelector :: Selector
readAttributeTimeZoneWithCompletionSelector = mkSelector "readAttributeTimeZoneWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDSTOffsetWithCompletion:@
readAttributeDSTOffsetWithCompletionSelector :: Selector
readAttributeDSTOffsetWithCompletionSelector = mkSelector "readAttributeDSTOffsetWithCompletion:"

-- | @Selector@ for @subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocalTimeWithCompletion:@
readAttributeLocalTimeWithCompletionSelector :: Selector
readAttributeLocalTimeWithCompletionSelector = mkSelector "readAttributeLocalTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeZoneDatabaseWithCompletion:@
readAttributeTimeZoneDatabaseWithCompletionSelector :: Selector
readAttributeTimeZoneDatabaseWithCompletionSelector = mkSelector "readAttributeTimeZoneDatabaseWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNTPServerAvailableWithCompletion:@
readAttributeNTPServerAvailableWithCompletionSelector :: Selector
readAttributeNTPServerAvailableWithCompletionSelector = mkSelector "readAttributeNTPServerAvailableWithCompletion:"

-- | @Selector@ for @subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeZoneListMaxSizeWithCompletion:@
readAttributeTimeZoneListMaxSizeWithCompletionSelector :: Selector
readAttributeTimeZoneListMaxSizeWithCompletionSelector = mkSelector "readAttributeTimeZoneListMaxSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDSTOffsetListMaxSizeWithCompletion:@
readAttributeDSTOffsetListMaxSizeWithCompletionSelector :: Selector
readAttributeDSTOffsetListMaxSizeWithCompletionSelector = mkSelector "readAttributeDSTOffsetListMaxSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportsDNSResolveWithCompletion:@
readAttributeSupportsDNSResolveWithCompletionSelector :: Selector
readAttributeSupportsDNSResolveWithCompletionSelector = mkSelector "readAttributeSupportsDNSResolveWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:"

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

