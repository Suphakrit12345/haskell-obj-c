{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Playback
--
-- This cluster provides an interface for controlling Media Playback (PLAY, PAUSE, etc) on a media device such as a TV or Speaker.
--
-- Generated bindings for @MTRBaseClusterMediaPlayback@.
module ObjC.Matter.MTRBaseClusterMediaPlayback
  ( MTRBaseClusterMediaPlayback
  , IsMTRBaseClusterMediaPlayback(..)
  , playWithParams_completion
  , playWithCompletion
  , pauseWithParams_completion
  , pauseWithCompletion
  , stopWithParams_completion
  , stopWithCompletion
  , startOverWithParams_completion
  , startOverWithCompletion
  , previousWithParams_completion
  , previousWithCompletion
  , nextWithParams_completion
  , nextWithCompletion
  , rewindWithParams_completion
  , rewindWithCompletion
  , fastForwardWithParams_completion
  , fastForwardWithCompletion
  , skipForwardWithParams_completion
  , skipBackwardWithParams_completion
  , seekWithParams_completion
  , activateAudioTrackWithParams_completion
  , activateTextTrackWithParams_completion
  , deactivateTextTrackWithParams_completion
  , deactivateTextTrackWithCompletion
  , readAttributeCurrentStateWithCompletion
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartTimeWithCompletion
  , subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeDurationWithCompletion
  , subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandler
  , readAttributeDurationWithClusterStateCache_endpoint_queue_completion
  , readAttributeSampledPositionWithCompletion
  , subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completion
  , readAttributePlaybackSpeedWithCompletion
  , subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandler
  , readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completion
  , readAttributeSeekRangeEndWithCompletion
  , subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completion
  , readAttributeSeekRangeStartWithCompletion
  , subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveAudioTrackWithCompletion
  , subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completion
  , readAttributeAvailableAudioTracksWithCompletion
  , subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandler
  , readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completion
  , readAttributeActiveTextTrackWithCompletion
  , subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandler
  , readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completion
  , readAttributeAvailableTextTracksWithCompletion
  , subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandler
  , readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completion
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
  , playWithParams_completionHandler
  , playWithCompletionHandler
  , pauseWithParams_completionHandler
  , pauseWithCompletionHandler
  , stopPlaybackWithParams_completionHandler
  , stopPlaybackWithCompletionHandler
  , startOverWithParams_completionHandler
  , startOverWithCompletionHandler
  , previousWithParams_completionHandler
  , previousWithCompletionHandler
  , nextWithParams_completionHandler
  , nextWithCompletionHandler
  , rewindWithParams_completionHandler
  , rewindWithCompletionHandler
  , fastForwardWithParams_completionHandler
  , fastForwardWithCompletionHandler
  , skipForwardWithParams_completionHandler
  , skipBackwardWithParams_completionHandler
  , seekWithParams_completionHandler
  , readAttributeCurrentStateWithCompletionHandler
  , subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeStartTimeWithCompletionHandler
  , subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeDurationWithCompletionHandler
  , subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDurationWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSampledPositionWithCompletionHandler
  , subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePlaybackSpeedWithCompletionHandler
  , subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSeekRangeEndWithCompletionHandler
  , subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSeekRangeStartWithCompletionHandler
  , subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandler
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
  , playWithParams_completionSelector
  , playWithCompletionSelector
  , pauseWithParams_completionSelector
  , pauseWithCompletionSelector
  , stopWithParams_completionSelector
  , stopWithCompletionSelector
  , startOverWithParams_completionSelector
  , startOverWithCompletionSelector
  , previousWithParams_completionSelector
  , previousWithCompletionSelector
  , nextWithParams_completionSelector
  , nextWithCompletionSelector
  , rewindWithParams_completionSelector
  , rewindWithCompletionSelector
  , fastForwardWithParams_completionSelector
  , fastForwardWithCompletionSelector
  , skipForwardWithParams_completionSelector
  , skipBackwardWithParams_completionSelector
  , seekWithParams_completionSelector
  , activateAudioTrackWithParams_completionSelector
  , activateTextTrackWithParams_completionSelector
  , deactivateTextTrackWithParams_completionSelector
  , deactivateTextTrackWithCompletionSelector
  , readAttributeCurrentStateWithCompletionSelector
  , subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartTimeWithCompletionSelector
  , subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDurationWithCompletionSelector
  , subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSampledPositionWithCompletionSelector
  , subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePlaybackSpeedWithCompletionSelector
  , subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSeekRangeEndWithCompletionSelector
  , subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSeekRangeStartWithCompletionSelector
  , subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveAudioTrackWithCompletionSelector
  , subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAvailableAudioTracksWithCompletionSelector
  , subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeActiveTextTrackWithCompletionSelector
  , subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAvailableTextTracksWithCompletionSelector
  , subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector
  , readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector
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
  , playWithParams_completionHandlerSelector
  , playWithCompletionHandlerSelector
  , pauseWithParams_completionHandlerSelector
  , pauseWithCompletionHandlerSelector
  , stopPlaybackWithParams_completionHandlerSelector
  , stopPlaybackWithCompletionHandlerSelector
  , startOverWithParams_completionHandlerSelector
  , startOverWithCompletionHandlerSelector
  , previousWithParams_completionHandlerSelector
  , previousWithCompletionHandlerSelector
  , nextWithParams_completionHandlerSelector
  , nextWithCompletionHandlerSelector
  , rewindWithParams_completionHandlerSelector
  , rewindWithCompletionHandlerSelector
  , fastForwardWithParams_completionHandlerSelector
  , fastForwardWithCompletionHandlerSelector
  , skipForwardWithParams_completionHandlerSelector
  , skipBackwardWithParams_completionHandlerSelector
  , seekWithParams_completionHandlerSelector
  , readAttributeCurrentStateWithCompletionHandlerSelector
  , subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeStartTimeWithCompletionHandlerSelector
  , subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDurationWithCompletionHandlerSelector
  , subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSampledPositionWithCompletionHandlerSelector
  , subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePlaybackSpeedWithCompletionHandlerSelector
  , subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSeekRangeEndWithCompletionHandlerSelector
  , subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSeekRangeStartWithCompletionHandlerSelector
  , subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector
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

-- | Command Play
--
-- Upon receipt, this SHALL play media.
--
-- ObjC selector: @- playWithParams:completion:@
playWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
playWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "playWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- playWithCompletion:@
playWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
playWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "playWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Pause
--
-- Upon receipt, this SHALL pause media.
--
-- ObjC selector: @- pauseWithParams:completion:@
pauseWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
pauseWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "pauseWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseWithCompletion:@
pauseWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
pauseWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "pauseWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Stop
--
-- Upon receipt, this SHALL stop media. User experience is context-specific. This will often navigate the user back to the location where media was originally launched.
--
-- ObjC selector: @- stopWithParams:completion:@
stopWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStopParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
stopWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "stopWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithCompletion:@
stopWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
stopWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "stopWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command StartOver
--
-- Upon receipt, this SHALL Start Over with the current media playback item.
--
-- ObjC selector: @- startOverWithParams:completion:@
startOverWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
startOverWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "startOverWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startOverWithCompletion:@
startOverWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
startOverWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "startOverWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Previous
--
-- Upon receipt, this SHALL cause the handler to be invoked for "Previous". User experience is context-specific. This will often Go back to the previous media playback item.
--
-- ObjC selector: @- previousWithParams:completion:@
previousWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
previousWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "previousWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- previousWithCompletion:@
previousWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
previousWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "previousWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Next
--
-- Upon receipt, this SHALL cause the handler to be invoked for "Next". User experience is context-specific. This will often Go forward to the next media playback item.
--
-- ObjC selector: @- nextWithParams:completion:@
nextWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
nextWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "nextWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- nextWithCompletion:@
nextWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
nextWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "nextWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command Rewind
--
-- Upon receipt, this SHALL Rewind through media. Different Rewind speeds can be used on the TV based upon the number of sequential calls to this function. This is to avoid needing to define every speed now (multiple fast, slow motion, etc).
--
-- ObjC selector: @- rewindWithParams:completion:@
rewindWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
rewindWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "rewindWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- rewindWithCompletion:@
rewindWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
rewindWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "rewindWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command FastForward
--
-- Upon receipt, this SHALL Advance through media. Different FF speeds can be used on the TV based upon the number of sequential calls to this function. This is to avoid needing to define every speed now (multiple fast, slow motion, etc).
--
-- ObjC selector: @- fastForwardWithParams:completion:@
fastForwardWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
fastForwardWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "fastForwardWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- fastForwardWithCompletion:@
fastForwardWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
fastForwardWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "fastForwardWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Command SkipForward
--
-- Upon receipt, this SHALL Skip forward in the media by the given number of seconds, using the data as follows:
--
-- ObjC selector: @- skipForwardWithParams:completion:@
skipForwardWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipForwardWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "skipForwardWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command SkipBackward
--
-- Upon receipt, this SHALL Skip backward in the media by the given number of seconds, using the data as follows:
--
-- ObjC selector: @- skipBackwardWithParams:completion:@
skipBackwardWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipBackwardWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "skipBackwardWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command Seek
--
-- Upon receipt, this SHALL Skip backward in the media by the given number of seconds, using the data as follows:
--
-- ObjC selector: @- seekWithParams:completion:@
seekWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
seekWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "seekWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ActivateAudioTrack
--
-- Upon receipt, the server SHALL set the active Audio Track to the one identified by the TrackID in the Track catalog for the streaming media. If the TrackID does not exist in the Track catalog, OR does not correspond to the streaming media OR no media is being streamed at the time of receipt of this command, the server will return an error status of INVALID_ARGUMENT.
--
-- ObjC selector: @- activateAudioTrackWithParams:completion:@
activateAudioTrackWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateAudioTrackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
activateAudioTrackWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "activateAudioTrackWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command ActivateTextTrack
--
-- Upon receipt, the server SHALL set the active Text Track to the one identified by the TrackID in the Track catalog for the streaming media. If the TrackID does not exist in the Track catalog, OR does not correspond to the streaming media OR no media is being streamed at the time of receipt of this command, the server SHALL return an error status of INVALID_ARGUMENT.
--
-- ObjC selector: @- activateTextTrackWithParams:completion:@
activateTextTrackWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateTextTrackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
activateTextTrackWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "activateTextTrackWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Command DeactivateTextTrack
--
-- If a Text Track is active (i.e. being displayed), upon receipt of this command, the server SHALL stop displaying it.
--
-- ObjC selector: @- deactivateTextTrackWithParams:completion:@
deactivateTextTrackWithParams_completion :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterDeactivateTextTrackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
deactivateTextTrackWithParams_completion mtrBaseClusterMediaPlayback  params completion =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "deactivateTextTrackWithParams:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- deactivateTextTrackWithCompletion:@
deactivateTextTrackWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
deactivateTextTrackWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "deactivateTextTrackWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeCurrentStateWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeStartTimeWithCompletion:@
readAttributeStartTimeWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeStartTimeWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeStartTimeWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeDurationWithCompletion:@
readAttributeDurationWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeDurationWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeDurationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDurationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDurationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDurationWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSampledPositionWithCompletion:@
readAttributeSampledPositionWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSampledPositionWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeSampledPositionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributePlaybackSpeedWithCompletion:@
readAttributePlaybackSpeedWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributePlaybackSpeedWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributePlaybackSpeedWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:@
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSeekRangeEndWithCompletion:@
readAttributeSeekRangeEndWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeEndWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeSeekRangeEndWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeSeekRangeStartWithCompletion:@
readAttributeSeekRangeStartWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeStartWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeSeekRangeStartWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveAudioTrackWithCompletion:@
readAttributeActiveAudioTrackWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeActiveAudioTrackWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeActiveAudioTrackWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAvailableAudioTracksWithCompletion:@
readAttributeAvailableAudioTracksWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAvailableAudioTracksWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeAvailableAudioTracksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeActiveTextTrackWithCompletion:@
readAttributeActiveTextTrackWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeActiveTextTrackWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeActiveTextTrackWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAvailableTextTracksWithCompletion:@
readAttributeAvailableTextTracksWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAvailableTextTracksWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeAvailableTextTracksWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeGeneratedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeAcceptedCommandListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeAttributeListWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeFeatureMapWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterMediaPlayback  completion =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeClusterRevisionWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  params subscriptionEstablished reportHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablished :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr clusterStateCacheContainer $ \raw_clusterStateCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:") retVoid [argPtr (castPtr raw_clusterStateCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- init@
init_ :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> IO (Id MTRBaseClusterMediaPlayback)
init_ mtrBaseClusterMediaPlayback  =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRBaseClusterMediaPlayback)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterMediaPlayback -> device -> CUShort -> queue -> IO (Id MTRBaseClusterMediaPlayback)
initWithDevice_endpoint_queue mtrBaseClusterMediaPlayback  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrBaseClusterMediaPlayback (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- playWithParams:completionHandler:@
playWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
playWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "playWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- playWithCompletionHandler:@
playWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
playWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "playWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseWithParams:completionHandler:@
pauseWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
pauseWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "pauseWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseWithCompletionHandler:@
pauseWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
pauseWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "pauseWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopPlaybackWithParams:completionHandler:@
stopPlaybackWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStopPlaybackParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
stopPlaybackWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "stopPlaybackWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopPlaybackWithCompletionHandler:@
stopPlaybackWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
stopPlaybackWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "stopPlaybackWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startOverWithParams:completionHandler:@
startOverWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
startOverWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "startOverWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startOverWithCompletionHandler:@
startOverWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
startOverWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "startOverWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- previousWithParams:completionHandler:@
previousWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
previousWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "previousWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- previousWithCompletionHandler:@
previousWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
previousWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "previousWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- nextWithParams:completionHandler:@
nextWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
nextWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "nextWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- nextWithCompletionHandler:@
nextWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
nextWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "nextWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- rewindWithParams:completionHandler:@
rewindWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
rewindWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "rewindWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- rewindWithCompletionHandler:@
rewindWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
rewindWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "rewindWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fastForwardWithParams:completionHandler:@
fastForwardWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
fastForwardWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "fastForwardWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fastForwardWithCompletionHandler:@
fastForwardWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
fastForwardWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "fastForwardWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipForwardWithParams:completionHandler:@
skipForwardWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipForwardWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "skipForwardWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipBackwardWithParams:completionHandler:@
skipBackwardWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
skipBackwardWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "skipBackwardWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- seekWithParams:completionHandler:@
seekWithParams_completionHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params) => mtrBaseClusterMediaPlayback -> params -> Ptr () -> IO ()
seekWithParams_completionHandler mtrBaseClusterMediaPlayback  params completionHandler =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrBaseClusterMediaPlayback (mkSelector "seekWithParams:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeCurrentStateWithCompletionHandler:@
readAttributeCurrentStateWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeCurrentStateWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeCurrentStateWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeStartTimeWithCompletionHandler:@
readAttributeStartTimeWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeStartTimeWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeStartTimeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeDurationWithCompletionHandler:@
readAttributeDurationWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeDurationWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeDurationWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSampledPositionWithCompletionHandler:@
readAttributeSampledPositionWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSampledPositionWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeSampledPositionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributePlaybackSpeedWithCompletionHandler:@
readAttributePlaybackSpeedWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributePlaybackSpeedWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributePlaybackSpeedWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSeekRangeEndWithCompletionHandler:@
readAttributeSeekRangeEndWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeEndWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeSeekRangeEndWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeSeekRangeStartWithCompletionHandler:@
readAttributeSeekRangeStartWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeSeekRangeStartWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeSeekRangeStartWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeAttributeListWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeFeatureMapWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback => mtrBaseClusterMediaPlayback -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterMediaPlayback  completionHandler =
    sendMsg mtrBaseClusterMediaPlayback (mkSelector "readAttributeClusterRevisionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterMediaPlayback -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterMediaPlayback  minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:") retVoid [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ()), argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr subscriptionEstablishedHandler :: Ptr ()), argPtr (castPtr reportHandler :: Ptr ())]

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterMediaPlayback"
    withObjCPtr attributeCacheContainer $ \raw_attributeCacheContainer ->
      withObjCPtr endpoint $ \raw_endpoint ->
        withObjCPtr queue $ \raw_queue ->
          sendClassMsg cls' (mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:") retVoid [argPtr (castPtr raw_attributeCacheContainer :: Ptr ()), argPtr (castPtr raw_endpoint :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterMediaPlayback mtrBaseClusterMediaPlayback, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterMediaPlayback -> device -> endpointID -> queue -> IO (Id MTRBaseClusterMediaPlayback)
initWithDevice_endpointID_queue mtrBaseClusterMediaPlayback  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrBaseClusterMediaPlayback (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playWithParams:completion:@
playWithParams_completionSelector :: Selector
playWithParams_completionSelector = mkSelector "playWithParams:completion:"

-- | @Selector@ for @playWithCompletion:@
playWithCompletionSelector :: Selector
playWithCompletionSelector = mkSelector "playWithCompletion:"

-- | @Selector@ for @pauseWithParams:completion:@
pauseWithParams_completionSelector :: Selector
pauseWithParams_completionSelector = mkSelector "pauseWithParams:completion:"

-- | @Selector@ for @pauseWithCompletion:@
pauseWithCompletionSelector :: Selector
pauseWithCompletionSelector = mkSelector "pauseWithCompletion:"

-- | @Selector@ for @stopWithParams:completion:@
stopWithParams_completionSelector :: Selector
stopWithParams_completionSelector = mkSelector "stopWithParams:completion:"

-- | @Selector@ for @stopWithCompletion:@
stopWithCompletionSelector :: Selector
stopWithCompletionSelector = mkSelector "stopWithCompletion:"

-- | @Selector@ for @startOverWithParams:completion:@
startOverWithParams_completionSelector :: Selector
startOverWithParams_completionSelector = mkSelector "startOverWithParams:completion:"

-- | @Selector@ for @startOverWithCompletion:@
startOverWithCompletionSelector :: Selector
startOverWithCompletionSelector = mkSelector "startOverWithCompletion:"

-- | @Selector@ for @previousWithParams:completion:@
previousWithParams_completionSelector :: Selector
previousWithParams_completionSelector = mkSelector "previousWithParams:completion:"

-- | @Selector@ for @previousWithCompletion:@
previousWithCompletionSelector :: Selector
previousWithCompletionSelector = mkSelector "previousWithCompletion:"

-- | @Selector@ for @nextWithParams:completion:@
nextWithParams_completionSelector :: Selector
nextWithParams_completionSelector = mkSelector "nextWithParams:completion:"

-- | @Selector@ for @nextWithCompletion:@
nextWithCompletionSelector :: Selector
nextWithCompletionSelector = mkSelector "nextWithCompletion:"

-- | @Selector@ for @rewindWithParams:completion:@
rewindWithParams_completionSelector :: Selector
rewindWithParams_completionSelector = mkSelector "rewindWithParams:completion:"

-- | @Selector@ for @rewindWithCompletion:@
rewindWithCompletionSelector :: Selector
rewindWithCompletionSelector = mkSelector "rewindWithCompletion:"

-- | @Selector@ for @fastForwardWithParams:completion:@
fastForwardWithParams_completionSelector :: Selector
fastForwardWithParams_completionSelector = mkSelector "fastForwardWithParams:completion:"

-- | @Selector@ for @fastForwardWithCompletion:@
fastForwardWithCompletionSelector :: Selector
fastForwardWithCompletionSelector = mkSelector "fastForwardWithCompletion:"

-- | @Selector@ for @skipForwardWithParams:completion:@
skipForwardWithParams_completionSelector :: Selector
skipForwardWithParams_completionSelector = mkSelector "skipForwardWithParams:completion:"

-- | @Selector@ for @skipBackwardWithParams:completion:@
skipBackwardWithParams_completionSelector :: Selector
skipBackwardWithParams_completionSelector = mkSelector "skipBackwardWithParams:completion:"

-- | @Selector@ for @seekWithParams:completion:@
seekWithParams_completionSelector :: Selector
seekWithParams_completionSelector = mkSelector "seekWithParams:completion:"

-- | @Selector@ for @activateAudioTrackWithParams:completion:@
activateAudioTrackWithParams_completionSelector :: Selector
activateAudioTrackWithParams_completionSelector = mkSelector "activateAudioTrackWithParams:completion:"

-- | @Selector@ for @activateTextTrackWithParams:completion:@
activateTextTrackWithParams_completionSelector :: Selector
activateTextTrackWithParams_completionSelector = mkSelector "activateTextTrackWithParams:completion:"

-- | @Selector@ for @deactivateTextTrackWithParams:completion:@
deactivateTextTrackWithParams_completionSelector :: Selector
deactivateTextTrackWithParams_completionSelector = mkSelector "deactivateTextTrackWithParams:completion:"

-- | @Selector@ for @deactivateTextTrackWithCompletion:@
deactivateTextTrackWithCompletionSelector :: Selector
deactivateTextTrackWithCompletionSelector = mkSelector "deactivateTextTrackWithCompletion:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletion:@
readAttributeCurrentStateWithCompletionSelector :: Selector
readAttributeCurrentStateWithCompletionSelector = mkSelector "readAttributeCurrentStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeCurrentStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartTimeWithCompletion:@
readAttributeStartTimeWithCompletionSelector :: Selector
readAttributeStartTimeWithCompletionSelector = mkSelector "readAttributeStartTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeStartTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDurationWithCompletion:@
readAttributeDurationWithCompletionSelector :: Selector
readAttributeDurationWithCompletionSelector = mkSelector "readAttributeDurationWithCompletion:"

-- | @Selector@ for @subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDurationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDurationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDurationWithClusterStateCache:endpoint:queue:completion:@
readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeDurationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDurationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSampledPositionWithCompletion:@
readAttributeSampledPositionWithCompletionSelector :: Selector
readAttributeSampledPositionWithCompletionSelector = mkSelector "readAttributeSampledPositionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSampledPositionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSampledPositionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSampledPositionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSampledPositionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePlaybackSpeedWithCompletion:@
readAttributePlaybackSpeedWithCompletionSelector :: Selector
readAttributePlaybackSpeedWithCompletionSelector = mkSelector "readAttributePlaybackSpeedWithCompletion:"

-- | @Selector@ for @subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePlaybackSpeedWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePlaybackSpeedWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:@
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributePlaybackSpeedWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePlaybackSpeedWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSeekRangeEndWithCompletion:@
readAttributeSeekRangeEndWithCompletionSelector :: Selector
readAttributeSeekRangeEndWithCompletionSelector = mkSelector "readAttributeSeekRangeEndWithCompletion:"

-- | @Selector@ for @subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSeekRangeEndWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeEndWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSeekRangeEndWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSeekRangeEndWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSeekRangeStartWithCompletion:@
readAttributeSeekRangeStartWithCompletionSelector :: Selector
readAttributeSeekRangeStartWithCompletionSelector = mkSelector "readAttributeSeekRangeStartWithCompletion:"

-- | @Selector@ for @subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSeekRangeStartWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeStartWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:@
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeSeekRangeStartWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSeekRangeStartWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveAudioTrackWithCompletion:@
readAttributeActiveAudioTrackWithCompletionSelector :: Selector
readAttributeActiveAudioTrackWithCompletionSelector = mkSelector "readAttributeActiveAudioTrackWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveAudioTrackWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveAudioTrackWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveAudioTrackWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveAudioTrackWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAvailableAudioTracksWithCompletion:@
readAttributeAvailableAudioTracksWithCompletionSelector :: Selector
readAttributeAvailableAudioTracksWithCompletionSelector = mkSelector "readAttributeAvailableAudioTracksWithCompletion:"

-- | @Selector@ for @subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAvailableAudioTracksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAvailableAudioTracksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAvailableAudioTracksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAvailableAudioTracksWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeActiveTextTrackWithCompletion:@
readAttributeActiveTextTrackWithCompletionSelector :: Selector
readAttributeActiveTextTrackWithCompletionSelector = mkSelector "readAttributeActiveTextTrackWithCompletion:"

-- | @Selector@ for @subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeActiveTextTrackWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeActiveTextTrackWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:@
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeActiveTextTrackWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeActiveTextTrackWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAvailableTextTracksWithCompletion:@
readAttributeAvailableTextTracksWithCompletionSelector :: Selector
readAttributeAvailableTextTracksWithCompletionSelector = mkSelector "readAttributeAvailableTextTracksWithCompletion:"

-- | @Selector@ for @subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeAvailableTextTracksWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAvailableTextTracksWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:@
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector :: Selector
readAttributeAvailableTextTracksWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAvailableTextTracksWithClusterStateCache:endpoint:queue:completion:"

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

-- | @Selector@ for @playWithParams:completionHandler:@
playWithParams_completionHandlerSelector :: Selector
playWithParams_completionHandlerSelector = mkSelector "playWithParams:completionHandler:"

-- | @Selector@ for @playWithCompletionHandler:@
playWithCompletionHandlerSelector :: Selector
playWithCompletionHandlerSelector = mkSelector "playWithCompletionHandler:"

-- | @Selector@ for @pauseWithParams:completionHandler:@
pauseWithParams_completionHandlerSelector :: Selector
pauseWithParams_completionHandlerSelector = mkSelector "pauseWithParams:completionHandler:"

-- | @Selector@ for @pauseWithCompletionHandler:@
pauseWithCompletionHandlerSelector :: Selector
pauseWithCompletionHandlerSelector = mkSelector "pauseWithCompletionHandler:"

-- | @Selector@ for @stopPlaybackWithParams:completionHandler:@
stopPlaybackWithParams_completionHandlerSelector :: Selector
stopPlaybackWithParams_completionHandlerSelector = mkSelector "stopPlaybackWithParams:completionHandler:"

-- | @Selector@ for @stopPlaybackWithCompletionHandler:@
stopPlaybackWithCompletionHandlerSelector :: Selector
stopPlaybackWithCompletionHandlerSelector = mkSelector "stopPlaybackWithCompletionHandler:"

-- | @Selector@ for @startOverWithParams:completionHandler:@
startOverWithParams_completionHandlerSelector :: Selector
startOverWithParams_completionHandlerSelector = mkSelector "startOverWithParams:completionHandler:"

-- | @Selector@ for @startOverWithCompletionHandler:@
startOverWithCompletionHandlerSelector :: Selector
startOverWithCompletionHandlerSelector = mkSelector "startOverWithCompletionHandler:"

-- | @Selector@ for @previousWithParams:completionHandler:@
previousWithParams_completionHandlerSelector :: Selector
previousWithParams_completionHandlerSelector = mkSelector "previousWithParams:completionHandler:"

-- | @Selector@ for @previousWithCompletionHandler:@
previousWithCompletionHandlerSelector :: Selector
previousWithCompletionHandlerSelector = mkSelector "previousWithCompletionHandler:"

-- | @Selector@ for @nextWithParams:completionHandler:@
nextWithParams_completionHandlerSelector :: Selector
nextWithParams_completionHandlerSelector = mkSelector "nextWithParams:completionHandler:"

-- | @Selector@ for @nextWithCompletionHandler:@
nextWithCompletionHandlerSelector :: Selector
nextWithCompletionHandlerSelector = mkSelector "nextWithCompletionHandler:"

-- | @Selector@ for @rewindWithParams:completionHandler:@
rewindWithParams_completionHandlerSelector :: Selector
rewindWithParams_completionHandlerSelector = mkSelector "rewindWithParams:completionHandler:"

-- | @Selector@ for @rewindWithCompletionHandler:@
rewindWithCompletionHandlerSelector :: Selector
rewindWithCompletionHandlerSelector = mkSelector "rewindWithCompletionHandler:"

-- | @Selector@ for @fastForwardWithParams:completionHandler:@
fastForwardWithParams_completionHandlerSelector :: Selector
fastForwardWithParams_completionHandlerSelector = mkSelector "fastForwardWithParams:completionHandler:"

-- | @Selector@ for @fastForwardWithCompletionHandler:@
fastForwardWithCompletionHandlerSelector :: Selector
fastForwardWithCompletionHandlerSelector = mkSelector "fastForwardWithCompletionHandler:"

-- | @Selector@ for @skipForwardWithParams:completionHandler:@
skipForwardWithParams_completionHandlerSelector :: Selector
skipForwardWithParams_completionHandlerSelector = mkSelector "skipForwardWithParams:completionHandler:"

-- | @Selector@ for @skipBackwardWithParams:completionHandler:@
skipBackwardWithParams_completionHandlerSelector :: Selector
skipBackwardWithParams_completionHandlerSelector = mkSelector "skipBackwardWithParams:completionHandler:"

-- | @Selector@ for @seekWithParams:completionHandler:@
seekWithParams_completionHandlerSelector :: Selector
seekWithParams_completionHandlerSelector = mkSelector "seekWithParams:completionHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithCompletionHandler:@
readAttributeCurrentStateWithCompletionHandlerSelector :: Selector
readAttributeCurrentStateWithCompletionHandlerSelector = mkSelector "readAttributeCurrentStateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeCurrentStateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentStateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeCurrentStateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentStateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeStartTimeWithCompletionHandler:@
readAttributeStartTimeWithCompletionHandlerSelector :: Selector
readAttributeStartTimeWithCompletionHandlerSelector = mkSelector "readAttributeStartTimeWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeStartTimeWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartTimeWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeStartTimeWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeStartTimeWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeDurationWithCompletionHandler:@
readAttributeDurationWithCompletionHandlerSelector :: Selector
readAttributeDurationWithCompletionHandlerSelector = mkSelector "readAttributeDurationWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeDurationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDurationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeDurationWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDurationWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSampledPositionWithCompletionHandler:@
readAttributeSampledPositionWithCompletionHandlerSelector :: Selector
readAttributeSampledPositionWithCompletionHandlerSelector = mkSelector "readAttributeSampledPositionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSampledPositionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSampledPositionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSampledPositionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSampledPositionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePlaybackSpeedWithCompletionHandler:@
readAttributePlaybackSpeedWithCompletionHandlerSelector :: Selector
readAttributePlaybackSpeedWithCompletionHandlerSelector = mkSelector "readAttributePlaybackSpeedWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributePlaybackSpeedWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePlaybackSpeedWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributePlaybackSpeedWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePlaybackSpeedWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSeekRangeEndWithCompletionHandler:@
readAttributeSeekRangeEndWithCompletionHandlerSelector :: Selector
readAttributeSeekRangeEndWithCompletionHandlerSelector = mkSelector "readAttributeSeekRangeEndWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSeekRangeEndWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeEndWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSeekRangeEndWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSeekRangeEndWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSeekRangeStartWithCompletionHandler:@
readAttributeSeekRangeStartWithCompletionHandlerSelector :: Selector
readAttributeSeekRangeStartWithCompletionHandlerSelector = mkSelector "readAttributeSeekRangeStartWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector
subscribeAttributeSeekRangeStartWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSeekRangeStartWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector
readAttributeSeekRangeStartWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSeekRangeStartWithAttributeCache:endpoint:queue:completionHandler:"

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

