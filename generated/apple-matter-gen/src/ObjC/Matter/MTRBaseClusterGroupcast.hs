{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Groupcast
--
-- The Groupcast cluster manages the content of the node-wide multicast Group membership that is part of the underlying interaction layer.
--
-- Generated bindings for @MTRBaseClusterGroupcast@.
module ObjC.Matter.MTRBaseClusterGroupcast
  ( MTRBaseClusterGroupcast
  , IsMTRBaseClusterGroupcast(..)
  , joinGroupWithParams_completion
  , leaveGroupWithParams_completion
  , updateGroupKeyWithParams_completion
  , expireGracePeriodWithParams_completion
  , configureAuxiliaryACLWithParams_completion
  , readAttributeMembershipWithParams_completion
  , subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandler
  , readAttributeMembershipWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxMembershipCountWithCompletion
  , subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completion
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
  , joinGroupWithParams_completionSelector
  , leaveGroupWithParams_completionSelector
  , updateGroupKeyWithParams_completionSelector
  , expireGracePeriodWithParams_completionSelector
  , configureAuxiliaryACLWithParams_completionSelector
  , readAttributeMembershipWithParams_completionSelector
  , subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxMembershipCountWithCompletionSelector
  , subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command JoinGroup
--
-- This command SHALL be used to instruct the server to join a multicast group.
--
-- ObjC selector: @- joinGroupWithParams:completion:@
joinGroupWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterJoinGroupParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
joinGroupWithParams_completion mtrBaseClusterGroupcast  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "joinGroupWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command LeaveGroup
--
-- This command SHALL allow a maintainer to request that the server withdraws itself or specific endpoints from a specific group or from all groups of this client's fabric.
--
-- ObjC selector: @- leaveGroupWithParams:completion:@
leaveGroupWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterLeaveGroupParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
leaveGroupWithParams_completion mtrBaseClusterGroupcast  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "leaveGroupWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command UpdateGroupKey
--
-- This command SHALL allow a fabric maintainer to update the OperationalGroupKey for an existing group ID that the server is a member of.
--
-- ObjC selector: @- updateGroupKeyWithParams:completion:@
updateGroupKeyWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterUpdateGroupKeyParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
updateGroupKeyWithParams_completion mtrBaseClusterGroupcast  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "updateGroupKeyWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ExpireGracePeriod
--
-- This command SHALL allow a fabric maintainer to expire the grace period of the previous key for an existing group ID that the server is a member of.
--
-- ObjC selector: @- expireGracePeriodWithParams:completion:@
expireGracePeriodWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterExpireGracePeriodParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
expireGracePeriodWithParams_completion mtrBaseClusterGroupcast  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "expireGracePeriodWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ConfigureAuxiliaryACL
--
-- This command SHALL allow an Administrator to enable or disable the generation of AuxiliaryACL entries in the Access Control Cluster based on the groups joined (see Groupcast Auxiliary ACL Handling).
--
-- ObjC selector: @- configureAuxiliaryACLWithParams:completion:@
configureAuxiliaryACLWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRGroupcastClusterConfigureAuxiliaryACLParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
configureAuxiliaryACLWithParams_completion mtrBaseClusterGroupcast  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "configureAuxiliaryACLWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMembershipWithParams:completion:@
readAttributeMembershipWithParams_completion :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRReadParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> IO ()
readAttributeMembershipWithParams_completion mtrBaseClusterGroupcast  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeMembershipWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:@
readAttributeMembershipWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMembershipWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxMembershipCountWithCompletion:@
readAttributeMaxMembershipCountWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeMaxMembershipCountWithCompletion mtrBaseClusterGroupcast  completion =
    sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeMaxMembershipCountWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterGroupcast  completion =
    sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterGroupcast  completion =
    sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterGroupcast  completion =
    sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterGroupcast  completion =
    sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterGroupcast  completion =
    sendMsg mtrBaseClusterGroupcast (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRSubscribeParams params) => mtrBaseClusterGroupcast -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterGroupcast  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterGroupcast (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast => mtrBaseClusterGroupcast -> IO (Id MTRBaseClusterGroupcast)
init_ mtrBaseClusterGroupcast  =
    sendMsg mtrBaseClusterGroupcast (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterGroupcast)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterGroupcast"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterGroupcast mtrBaseClusterGroupcast, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterGroupcast -> device -> endpointID -> queue -> IO (Id MTRBaseClusterGroupcast)
initWithDevice_endpointID_queue mtrBaseClusterGroupcast  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterGroupcast (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @joinGroupWithParams:completion:@
joinGroupWithParams_completionSelector :: Selector
joinGroupWithParams_completionSelector = mkSelector "joinGroupWithParams:completion:"

-- | @Selector@ for @leaveGroupWithParams:completion:@
leaveGroupWithParams_completionSelector :: Selector
leaveGroupWithParams_completionSelector = mkSelector "leaveGroupWithParams:completion:"

-- | @Selector@ for @updateGroupKeyWithParams:completion:@
updateGroupKeyWithParams_completionSelector :: Selector
updateGroupKeyWithParams_completionSelector = mkSelector "updateGroupKeyWithParams:completion:"

-- | @Selector@ for @expireGracePeriodWithParams:completion:@
expireGracePeriodWithParams_completionSelector :: Selector
expireGracePeriodWithParams_completionSelector = mkSelector "expireGracePeriodWithParams:completion:"

-- | @Selector@ for @configureAuxiliaryACLWithParams:completion:@
configureAuxiliaryACLWithParams_completionSelector :: Selector
configureAuxiliaryACLWithParams_completionSelector = mkSelector "configureAuxiliaryACLWithParams:completion:"

-- | @Selector@ for @readAttributeMembershipWithParams:completion:@
readAttributeMembershipWithParams_completionSelector :: Selector
readAttributeMembershipWithParams_completionSelector = mkSelector "readAttributeMembershipWithParams:completion:"

-- | @Selector@ for @subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMembershipWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMembershipWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:@
readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMembershipWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMembershipWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxMembershipCountWithCompletion:@
readAttributeMaxMembershipCountWithCompletionSelector :: Selector
readAttributeMaxMembershipCountWithCompletionSelector = mkSelector "readAttributeMaxMembershipCountWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeMaxMembershipCountWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxMembershipCountWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeMaxMembershipCountWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxMembershipCountWithClusterStateCache:endpoint:queue:completion:"

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

