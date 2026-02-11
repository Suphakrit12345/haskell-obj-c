{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Content Control
--
-- This cluster is used for managing the content control (including "parental control") settings on a media device such as a TV, or Set-top Box.
--
-- Generated bindings for @MTRBaseClusterContentControl@.
module ObjC.Matter.MTRBaseClusterContentControl
  ( MTRBaseClusterContentControl
  , IsMTRBaseClusterContentControl(..)
  , updatePINWithParams_completion
  , resetPINWithParams_completion
  , resetPINWithCompletion
  , enableWithParams_completion
  , enableWithCompletion
  , disableWithParams_completion
  , disableWithCompletion
  , addBonusTimeWithParams_completion
  , addBonusTimeWithCompletion
  , setScreenDailyTimeWithParams_completion
  , blockUnratedContentWithParams_completion
  , blockUnratedContentWithCompletion
  , unblockUnratedContentWithParams_completion
  , unblockUnratedContentWithCompletion
  , setOnDemandRatingThresholdWithParams_completion
  , setScheduledContentRatingThresholdWithParams_completion
  , readAttributeEnabledWithCompletion
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnDemandRatingsWithCompletion
  , subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completion
  , readAttributeOnDemandRatingThresholdWithCompletion
  , subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completion
  , readAttributeScheduledContentRatingsWithCompletion
  , subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandler
  , readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completion
  , readAttributeScheduledContentRatingThresholdWithCompletion
  , subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandler
  , readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completion
  , readAttributeScreenDailyTimeWithCompletion
  , subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeRemainingScreenTimeWithCompletion
  , subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeBlockUnratedWithCompletion
  , subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandler
  , readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completion
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
  , updatePINWithParams_completionSelector
  , resetPINWithParams_completionSelector
  , resetPINWithCompletionSelector
  , enableWithParams_completionSelector
  , enableWithCompletionSelector
  , disableWithParams_completionSelector
  , disableWithCompletionSelector
  , addBonusTimeWithParams_completionSelector
  , addBonusTimeWithCompletionSelector
  , setScreenDailyTimeWithParams_completionSelector
  , blockUnratedContentWithParams_completionSelector
  , blockUnratedContentWithCompletionSelector
  , unblockUnratedContentWithParams_completionSelector
  , unblockUnratedContentWithCompletionSelector
  , setOnDemandRatingThresholdWithParams_completionSelector
  , setScheduledContentRatingThresholdWithParams_completionSelector
  , readAttributeEnabledWithCompletionSelector
  , subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnDemandRatingsWithCompletionSelector
  , subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeOnDemandRatingThresholdWithCompletionSelector
  , subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScheduledContentRatingsWithCompletionSelector
  , subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScheduledContentRatingThresholdWithCompletionSelector
  , subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeScreenDailyTimeWithCompletionSelector
  , subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeRemainingScreenTimeWithCompletionSelector
  , subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeBlockUnratedWithCompletionSelector
  , subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector
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

-- | Command UpdatePIN
--
-- The purpose of this command is to update the PIN used for protecting configuration of the content control settings. Upon success, the old PIN SHALL no longer work. The PIN is used to ensure that only the Node (or User) with the PIN code can make changes to the Content Control settings, for example, turn off Content Controls or modify the ScreenDailyTime. The PIN is composed of a numeric string of up to 6 human readable characters (displayable) . Upon receipt of this command, the media device SHALL check if the OldPIN field of this command is the same as the current PIN. If the PINs are the same, then the PIN code SHALL be set to NewPIN. Otherwise a response with InvalidPINCode error status SHALL be returned. The media device MAY provide a default PIN to the User via an out of band mechanism. For security reasons, it is recommended that a client encourage the user to update the PIN from its default value when performing configuration of the Content Control settings exposed by this cluster. The ResetPIN command can also be used to obtain the default PIN.
--
-- ObjC selector: @- updatePINWithParams:completion:@
updatePINWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterUpdatePINParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
updatePINWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "updatePINWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ResetPIN
--
-- The purpose of this command is to reset the PIN. If this command is executed successfully, a ResetPINResponse command with a new PIN SHALL be returned.
--
-- ObjC selector: @- resetPINWithParams:completion:@
resetPINWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterResetPINParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
resetPINWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "resetPINWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetPINWithCompletion:@
resetPINWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
resetPINWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "resetPINWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Enable
--
-- The purpose of this command is to turn on the Content Control feature on a media device. On receipt of the Enable command, the media device SHALL set the Enabled attribute to TRUE.
--
-- ObjC selector: @- enableWithParams:completion:@
enableWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterEnableParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
enableWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "enableWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- enableWithCompletion:@
enableWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
enableWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "enableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Disable
--
-- The purpose of this command is to turn off the Content Control feature on a media device. On receipt of the Disable command, the media device SHALL set the Enabled attribute to FALSE.
--
-- ObjC selector: @- disableWithParams:completion:@
disableWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterDisableParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
disableWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "disableWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- disableWithCompletion:@
disableWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
disableWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "disableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command AddBonusTime
--
-- The purpose of this command is to add the extra screen time for the user. If a client with Operate privilege invokes this command, the media device SHALL check whether the PINCode passed in the command matches the current PINCode value. If these match, then the RemainingScreenTime attribute SHALL be increased by the specified BonusTime value. If the PINs do not match, then a response with InvalidPINCode error status SHALL be returned, and no changes SHALL be made to RemainingScreenTime. If a client with Manage privilege or greater invokes this command, the media device SHALL ignore the PINCode field and directly increase the RemainingScreenTime attribute by the specified BonusTime value. A server that does not support the PM feature SHALL respond with InvalidPINCode to clients that only have Operate privilege unless: It has been provided with the PIN value to expect via an out of band mechanism, and The client has provided a PINCode that matches the expected PIN value.
--
-- ObjC selector: @- addBonusTimeWithParams:completion:@
addBonusTimeWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterAddBonusTimeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
addBonusTimeWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "addBonusTimeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- addBonusTimeWithCompletion:@
addBonusTimeWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
addBonusTimeWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "addBonusTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SetScreenDailyTime
--
-- The purpose of this command is to set the ScreenDailyTime attribute. On receipt of the SetScreenDailyTime command, the media device SHALL set the ScreenDailyTime attribute to the ScreenTime value.
--
-- ObjC selector: @- setScreenDailyTimeWithParams:completion:@
setScreenDailyTimeWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterSetScreenDailyTimeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
setScreenDailyTimeWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "setScreenDailyTimeWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command BlockUnratedContent
--
-- The purpose of this command is to specify whether programs with no Content rating must be blocked by this media device. On receipt of the BlockUnratedContent command, the media device SHALL set the BlockUnrated attribute to TRUE.
--
-- ObjC selector: @- blockUnratedContentWithParams:completion:@
blockUnratedContentWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterBlockUnratedContentParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
blockUnratedContentWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "blockUnratedContentWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- blockUnratedContentWithCompletion:@
blockUnratedContentWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
blockUnratedContentWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "blockUnratedContentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command UnblockUnratedContent
--
-- The purpose of this command is to specify whether programs with no Content rating must be blocked by this media device. On receipt of the UnblockUnratedContent command, the media device SHALL set the BlockUnrated attribute to FALSE.
--
-- ObjC selector: @- unblockUnratedContentWithParams:completion:@
unblockUnratedContentWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterUnblockUnratedContentParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
unblockUnratedContentWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "unblockUnratedContentWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- unblockUnratedContentWithCompletion:@
unblockUnratedContentWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
unblockUnratedContentWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "unblockUnratedContentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SetOnDemandRatingThreshold
--
-- The purpose of this command is to set the OnDemandRatingThreshold attribute. On receipt of the SetOnDemandRatingThreshold command, the media device SHALL check if the Rating field is one of values present in the OnDemandRatings attribute. If not, then a response with InvalidRating error status SHALL be returned.
--
-- ObjC selector: @- setOnDemandRatingThresholdWithParams:completion:@
setOnDemandRatingThresholdWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterSetOnDemandRatingThresholdParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
setOnDemandRatingThresholdWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "setOnDemandRatingThresholdWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SetScheduledContentRatingThreshold
--
-- The purpose of this command is to set ScheduledContentRatingThreshold attribute. On receipt of the SetScheduledContentRatingThreshold command, the media device SHALL check if the Rating field is one of values present in the ScheduledContentRatings attribute. If not, then a response with InvalidRating error status SHALL be returned.
--
-- ObjC selector: @- setScheduledContentRatingThresholdWithParams:completion:@
setScheduledContentRatingThresholdWithParams_completion :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRContentControlClusterSetScheduledContentRatingThresholdParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> IO ()
setScheduledContentRatingThresholdWithParams_completion mtrBaseClusterContentControl  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "setScheduledContentRatingThresholdWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeEnabledWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeEnabledWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeEnabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnDemandRatingsWithCompletion:@
readAttributeOnDemandRatingsWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeOnDemandRatingsWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeOnDemandRatingsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeOnDemandRatingThresholdWithCompletion:@
readAttributeOnDemandRatingThresholdWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeOnDemandRatingThresholdWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeOnDemandRatingThresholdWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScheduledContentRatingsWithCompletion:@
readAttributeScheduledContentRatingsWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeScheduledContentRatingsWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeScheduledContentRatingsWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScheduledContentRatingThresholdWithCompletion:@
readAttributeScheduledContentRatingThresholdWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeScheduledContentRatingThresholdWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeScheduledContentRatingThresholdWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeScreenDailyTimeWithCompletion:@
readAttributeScreenDailyTimeWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeScreenDailyTimeWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeScreenDailyTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeRemainingScreenTimeWithCompletion:@
readAttributeRemainingScreenTimeWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeRemainingScreenTimeWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeRemainingScreenTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeBlockUnratedWithCompletion:@
readAttributeBlockUnratedWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeBlockUnratedWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeBlockUnratedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:@
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterContentControl  completion =
    sendMsg mtrBaseClusterContentControl (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRSubscribeParams params) => mtrBaseClusterContentControl -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterContentControl  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterContentControl (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterContentControl mtrBaseClusterContentControl => mtrBaseClusterContentControl -> IO (Id MTRBaseClusterContentControl)
init_ mtrBaseClusterContentControl  =
    sendMsg mtrBaseClusterContentControl (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterContentControl)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterContentControl"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterContentControl mtrBaseClusterContentControl, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterContentControl -> device -> endpointID -> queue -> IO (Id MTRBaseClusterContentControl)
initWithDevice_endpointID_queue mtrBaseClusterContentControl  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterContentControl (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatePINWithParams:completion:@
updatePINWithParams_completionSelector :: Selector
updatePINWithParams_completionSelector = mkSelector "updatePINWithParams:completion:"

-- | @Selector@ for @resetPINWithParams:completion:@
resetPINWithParams_completionSelector :: Selector
resetPINWithParams_completionSelector = mkSelector "resetPINWithParams:completion:"

-- | @Selector@ for @resetPINWithCompletion:@
resetPINWithCompletionSelector :: Selector
resetPINWithCompletionSelector = mkSelector "resetPINWithCompletion:"

-- | @Selector@ for @enableWithParams:completion:@
enableWithParams_completionSelector :: Selector
enableWithParams_completionSelector = mkSelector "enableWithParams:completion:"

-- | @Selector@ for @enableWithCompletion:@
enableWithCompletionSelector :: Selector
enableWithCompletionSelector = mkSelector "enableWithCompletion:"

-- | @Selector@ for @disableWithParams:completion:@
disableWithParams_completionSelector :: Selector
disableWithParams_completionSelector = mkSelector "disableWithParams:completion:"

-- | @Selector@ for @disableWithCompletion:@
disableWithCompletionSelector :: Selector
disableWithCompletionSelector = mkSelector "disableWithCompletion:"

-- | @Selector@ for @addBonusTimeWithParams:completion:@
addBonusTimeWithParams_completionSelector :: Selector
addBonusTimeWithParams_completionSelector = mkSelector "addBonusTimeWithParams:completion:"

-- | @Selector@ for @addBonusTimeWithCompletion:@
addBonusTimeWithCompletionSelector :: Selector
addBonusTimeWithCompletionSelector = mkSelector "addBonusTimeWithCompletion:"

-- | @Selector@ for @setScreenDailyTimeWithParams:completion:@
setScreenDailyTimeWithParams_completionSelector :: Selector
setScreenDailyTimeWithParams_completionSelector = mkSelector "setScreenDailyTimeWithParams:completion:"

-- | @Selector@ for @blockUnratedContentWithParams:completion:@
blockUnratedContentWithParams_completionSelector :: Selector
blockUnratedContentWithParams_completionSelector = mkSelector "blockUnratedContentWithParams:completion:"

-- | @Selector@ for @blockUnratedContentWithCompletion:@
blockUnratedContentWithCompletionSelector :: Selector
blockUnratedContentWithCompletionSelector = mkSelector "blockUnratedContentWithCompletion:"

-- | @Selector@ for @unblockUnratedContentWithParams:completion:@
unblockUnratedContentWithParams_completionSelector :: Selector
unblockUnratedContentWithParams_completionSelector = mkSelector "unblockUnratedContentWithParams:completion:"

-- | @Selector@ for @unblockUnratedContentWithCompletion:@
unblockUnratedContentWithCompletionSelector :: Selector
unblockUnratedContentWithCompletionSelector = mkSelector "unblockUnratedContentWithCompletion:"

-- | @Selector@ for @setOnDemandRatingThresholdWithParams:completion:@
setOnDemandRatingThresholdWithParams_completionSelector :: Selector
setOnDemandRatingThresholdWithParams_completionSelector = mkSelector "setOnDemandRatingThresholdWithParams:completion:"

-- | @Selector@ for @setScheduledContentRatingThresholdWithParams:completion:@
setScheduledContentRatingThresholdWithParams_completionSelector :: Selector
setScheduledContentRatingThresholdWithParams_completionSelector = mkSelector "setScheduledContentRatingThresholdWithParams:completion:"

-- | @Selector@ for @readAttributeEnabledWithCompletion:@
readAttributeEnabledWithCompletionSelector :: Selector
readAttributeEnabledWithCompletionSelector = mkSelector "readAttributeEnabledWithCompletion:"

-- | @Selector@ for @subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeEnabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeEnabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeEnabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeEnabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnDemandRatingsWithCompletion:@
readAttributeOnDemandRatingsWithCompletionSelector :: Selector
readAttributeOnDemandRatingsWithCompletionSelector = mkSelector "readAttributeOnDemandRatingsWithCompletion:"

-- | @Selector@ for @subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnDemandRatingsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnDemandRatingsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnDemandRatingsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnDemandRatingsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeOnDemandRatingThresholdWithCompletion:@
readAttributeOnDemandRatingThresholdWithCompletionSelector :: Selector
readAttributeOnDemandRatingThresholdWithCompletionSelector = mkSelector "readAttributeOnDemandRatingThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeOnDemandRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeOnDemandRatingThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeOnDemandRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeOnDemandRatingThresholdWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScheduledContentRatingsWithCompletion:@
readAttributeScheduledContentRatingsWithCompletionSelector :: Selector
readAttributeScheduledContentRatingsWithCompletionSelector = mkSelector "readAttributeScheduledContentRatingsWithCompletion:"

-- | @Selector@ for @subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScheduledContentRatingsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScheduledContentRatingsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScheduledContentRatingsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScheduledContentRatingsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScheduledContentRatingThresholdWithCompletion:@
readAttributeScheduledContentRatingThresholdWithCompletionSelector :: Selector
readAttributeScheduledContentRatingThresholdWithCompletionSelector = mkSelector "readAttributeScheduledContentRatingThresholdWithCompletion:"

-- | @Selector@ for @subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScheduledContentRatingThresholdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScheduledContentRatingThresholdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:@
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScheduledContentRatingThresholdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScheduledContentRatingThresholdWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeScreenDailyTimeWithCompletion:@
readAttributeScreenDailyTimeWithCompletionSelector :: Selector
readAttributeScreenDailyTimeWithCompletionSelector = mkSelector "readAttributeScreenDailyTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeScreenDailyTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeScreenDailyTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeScreenDailyTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeScreenDailyTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeRemainingScreenTimeWithCompletion:@
readAttributeRemainingScreenTimeWithCompletionSelector :: Selector
readAttributeRemainingScreenTimeWithCompletionSelector = mkSelector "readAttributeRemainingScreenTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeRemainingScreenTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeRemainingScreenTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeRemainingScreenTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeRemainingScreenTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeBlockUnratedWithCompletion:@
readAttributeBlockUnratedWithCompletionSelector :: Selector
readAttributeBlockUnratedWithCompletionSelector = mkSelector "readAttributeBlockUnratedWithCompletion:"

-- | @Selector@ for @subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeBlockUnratedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeBlockUnratedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:@
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeBlockUnratedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeBlockUnratedWithClusterStateCache:endpoint:queue:completion:"

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

