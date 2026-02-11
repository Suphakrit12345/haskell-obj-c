{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster WebRTC Transport Requestor
--
-- The WebRTC transport requestor cluster provides a way for stream consumers (e.g. Matter Stream Viewer) to establish a WebRTC connection with a stream provider.
--
-- Generated bindings for @MTRBaseClusterWebRTCTransportRequestor@.
module ObjC.Matter.MTRBaseClusterWebRTCTransportRequestor
  ( MTRBaseClusterWebRTCTransportRequestor
  , IsMTRBaseClusterWebRTCTransportRequestor(..)
  , offerWithParams_completion
  , answerWithParams_completion
  , iceCandidatesWithParams_completion
  , endWithParams_completion
  , readAttributeCurrentSessionsWithParams_completion
  , subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion
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
  , offerWithParams_completionSelector
  , answerWithParams_completionSelector
  , iceCandidatesWithParams_completionSelector
  , endWithParams_completionSelector
  , readAttributeCurrentSessionsWithParams_completionSelector
  , subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command Offer
--
-- This command provides the stream requestor with WebRTC session details.
--
-- ObjC selector: @- offerWithParams:completion:@
offerWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterOfferParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
offerWithParams_completion mtrBaseClusterWebRTCTransportRequestor  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "offerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command Answer
--
-- This command provides the stream requestor with the WebRTC session details (i.e. Session ID and SDP answer), It is the next command in the Offer/Answer flow to the ProvideOffer command.
--
-- ObjC selector: @- answerWithParams:completion:@
answerWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterAnswerParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
answerWithParams_completion mtrBaseClusterWebRTCTransportRequestor  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "answerWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ICECandidates
--
-- This command allows for the object based ICE candidates generated after the initial Offer / Answer exchange, via a JSEP onicecandidate event, a DOM rtcpeerconnectioniceevent event, or other WebRTC compliant implementations, to be added to a session during the gathering phase.
--
-- ObjC selector: @- ICECandidatesWithParams:completion:@
iceCandidatesWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterICECandidatesParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
iceCandidatesWithParams_completion mtrBaseClusterWebRTCTransportRequestor  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "ICECandidatesWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command End
--
-- This command notifies the stream requestor that the provider has ended the WebRTC session.
--
-- ObjC selector: @- endWithParams:completion:@
endWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRWebRTCTransportRequestorClusterEndParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
endWithParams_completion mtrBaseClusterWebRTCTransportRequestor  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "endWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentSessionsWithParams:completion:@
readAttributeCurrentSessionsWithParams_completion :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRReadParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> IO ()
readAttributeCurrentSessionsWithParams_completion mtrBaseClusterWebRTCTransportRequestor  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "readAttributeCurrentSessionsWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterWebRTCTransportRequestor  completion =
    sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterWebRTCTransportRequestor  completion =
    sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterWebRTCTransportRequestor  completion =
    sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterWebRTCTransportRequestor  completion =
    sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterWebRTCTransportRequestor  completion =
    sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRSubscribeParams params) => mtrBaseClusterWebRTCTransportRequestor -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterWebRTCTransportRequestor  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor => mtrBaseClusterWebRTCTransportRequestor -> IO (Id MTRBaseClusterWebRTCTransportRequestor)
init_ mtrBaseClusterWebRTCTransportRequestor  =
    sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterWebRTCTransportRequestor)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterWebRTCTransportRequestor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterWebRTCTransportRequestor mtrBaseClusterWebRTCTransportRequestor, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterWebRTCTransportRequestor -> device -> endpointID -> queue -> IO (Id MTRBaseClusterWebRTCTransportRequestor)
initWithDevice_endpointID_queue mtrBaseClusterWebRTCTransportRequestor  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterWebRTCTransportRequestor (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offerWithParams:completion:@
offerWithParams_completionSelector :: Selector
offerWithParams_completionSelector = mkSelector "offerWithParams:completion:"

-- | @Selector@ for @answerWithParams:completion:@
answerWithParams_completionSelector :: Selector
answerWithParams_completionSelector = mkSelector "answerWithParams:completion:"

-- | @Selector@ for @ICECandidatesWithParams:completion:@
iceCandidatesWithParams_completionSelector :: Selector
iceCandidatesWithParams_completionSelector = mkSelector "ICECandidatesWithParams:completion:"

-- | @Selector@ for @endWithParams:completion:@
endWithParams_completionSelector :: Selector
endWithParams_completionSelector = mkSelector "endWithParams:completion:"

-- | @Selector@ for @readAttributeCurrentSessionsWithParams:completion:@
readAttributeCurrentSessionsWithParams_completionSelector :: Selector
readAttributeCurrentSessionsWithParams_completionSelector = mkSelector "readAttributeCurrentSessionsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentSessionsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentSessionsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentSessionsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentSessionsWithClusterStateCache:endpoint:queue:completion:"

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

