{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Media Playback    This cluster provides an interface for controlling Media Playback (PLAY, PAUSE, etc) on a media device such as a TV or Speaker.
--
-- Generated bindings for @MTRClusterMediaPlayback@.
module ObjC.Matter.MTRClusterMediaPlayback
  ( MTRClusterMediaPlayback
  , IsMTRClusterMediaPlayback(..)
  , playWithParams_expectedValues_expectedValueInterval_completion
  , playWithExpectedValues_expectedValueInterval_completion
  , pauseWithParams_expectedValues_expectedValueInterval_completion
  , pauseWithExpectedValues_expectedValueInterval_completion
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , startOverWithParams_expectedValues_expectedValueInterval_completion
  , startOverWithExpectedValues_expectedValueInterval_completion
  , previousWithParams_expectedValues_expectedValueInterval_completion
  , previousWithExpectedValues_expectedValueInterval_completion
  , nextWithParams_expectedValues_expectedValueInterval_completion
  , nextWithExpectedValues_expectedValueInterval_completion
  , rewindWithParams_expectedValues_expectedValueInterval_completion
  , rewindWithExpectedValues_expectedValueInterval_completion
  , fastForwardWithParams_expectedValues_expectedValueInterval_completion
  , fastForwardWithExpectedValues_expectedValueInterval_completion
  , skipForwardWithParams_expectedValues_expectedValueInterval_completion
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completion
  , seekWithParams_expectedValues_expectedValueInterval_completion
  , activateAudioTrackWithParams_expectedValues_expectedValueInterval_completion
  , activateTextTrackWithParams_expectedValues_expectedValueInterval_completion
  , deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completion
  , deactivateTextTrackWithExpectedValues_expectedValueInterval_completion
  , readAttributeCurrentStateWithParams
  , readAttributeStartTimeWithParams
  , readAttributeDurationWithParams
  , readAttributeSampledPositionWithParams
  , readAttributePlaybackSpeedWithParams
  , readAttributeSeekRangeEndWithParams
  , readAttributeSeekRangeStartWithParams
  , readAttributeActiveAudioTrackWithParams
  , readAttributeAvailableAudioTracksWithParams
  , readAttributeActiveTextTrackWithParams
  , readAttributeAvailableTextTracksWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , playWithParams_expectedValues_expectedValueInterval_completionHandler
  , playWithExpectedValues_expectedValueInterval_completionHandler
  , pauseWithParams_expectedValues_expectedValueInterval_completionHandler
  , pauseWithExpectedValues_expectedValueInterval_completionHandler
  , stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopPlaybackWithExpectedValues_expectedValueInterval_completionHandler
  , startOverWithParams_expectedValues_expectedValueInterval_completionHandler
  , startOverWithExpectedValues_expectedValueInterval_completionHandler
  , previousWithParams_expectedValues_expectedValueInterval_completionHandler
  , previousWithExpectedValues_expectedValueInterval_completionHandler
  , nextWithParams_expectedValues_expectedValueInterval_completionHandler
  , nextWithExpectedValues_expectedValueInterval_completionHandler
  , rewindWithParams_expectedValues_expectedValueInterval_completionHandler
  , rewindWithExpectedValues_expectedValueInterval_completionHandler
  , fastForwardWithParams_expectedValues_expectedValueInterval_completionHandler
  , fastForwardWithExpectedValues_expectedValueInterval_completionHandler
  , skipForwardWithParams_expectedValues_expectedValueInterval_completionHandler
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandler
  , seekWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , playWithParams_expectedValues_expectedValueInterval_completionSelector
  , playWithExpectedValues_expectedValueInterval_completionSelector
  , pauseWithParams_expectedValues_expectedValueInterval_completionSelector
  , pauseWithExpectedValues_expectedValueInterval_completionSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithExpectedValues_expectedValueInterval_completionSelector
  , startOverWithParams_expectedValues_expectedValueInterval_completionSelector
  , startOverWithExpectedValues_expectedValueInterval_completionSelector
  , previousWithParams_expectedValues_expectedValueInterval_completionSelector
  , previousWithExpectedValues_expectedValueInterval_completionSelector
  , nextWithParams_expectedValues_expectedValueInterval_completionSelector
  , nextWithExpectedValues_expectedValueInterval_completionSelector
  , rewindWithParams_expectedValues_expectedValueInterval_completionSelector
  , rewindWithExpectedValues_expectedValueInterval_completionSelector
  , fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector
  , fastForwardWithExpectedValues_expectedValueInterval_completionSelector
  , skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector
  , seekWithParams_expectedValues_expectedValueInterval_completionSelector
  , activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector
  , activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector
  , deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector
  , deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeCurrentStateWithParamsSelector
  , readAttributeStartTimeWithParamsSelector
  , readAttributeDurationWithParamsSelector
  , readAttributeSampledPositionWithParamsSelector
  , readAttributePlaybackSpeedWithParamsSelector
  , readAttributeSeekRangeEndWithParamsSelector
  , readAttributeSeekRangeStartWithParamsSelector
  , readAttributeActiveAudioTrackWithParamsSelector
  , readAttributeAvailableAudioTracksWithParamsSelector
  , readAttributeActiveTextTrackWithParamsSelector
  , readAttributeAvailableTextTracksWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , playWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , previousWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , nextWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
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

-- | @- playWithParams:expectedValues:expectedValueInterval:completion:@
playWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "playWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- playWithExpectedValues:expectedValueInterval:completion:@
playWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "playWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "stopWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startOverWithParams:expectedValues:expectedValueInterval:completion:@
startOverWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "startOverWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- startOverWithExpectedValues:expectedValueInterval:completion:@
startOverWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "startOverWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- previousWithParams:expectedValues:expectedValueInterval:completion:@
previousWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "previousWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- previousWithExpectedValues:expectedValueInterval:completion:@
previousWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "previousWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- nextWithParams:expectedValues:expectedValueInterval:completion:@
nextWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "nextWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- nextWithExpectedValues:expectedValueInterval:completion:@
nextWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "nextWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- rewindWithParams:expectedValues:expectedValueInterval:completion:@
rewindWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "rewindWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- rewindWithExpectedValues:expectedValueInterval:completion:@
rewindWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "rewindWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- fastForwardWithParams:expectedValues:expectedValueInterval:completion:@
fastForwardWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "fastForwardWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- fastForwardWithExpectedValues:expectedValueInterval:completion:@
fastForwardWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "fastForwardWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- skipForwardWithParams:expectedValues:expectedValueInterval:completion:@
skipForwardWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipForwardWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "skipForwardWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- skipBackwardWithParams:expectedValues:expectedValueInterval:completion:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipBackwardWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "skipBackwardWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- seekWithParams:expectedValues:expectedValueInterval:completion:@
seekWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
seekWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "seekWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateAudioTrackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateTextTrackWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterActivateTextTrackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
activateTextTrackWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterDeactivateTextTrackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
deactivateTextTrackWithExpectedValues_expectedValueInterval_completion mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeCurrentStateWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeCurrentStateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStartTimeWithParams:@
readAttributeStartTimeWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeStartTimeWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeStartTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDurationWithParams:@
readAttributeDurationWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeDurationWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeDurationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSampledPositionWithParams:@
readAttributeSampledPositionWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeSampledPositionWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeSampledPositionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePlaybackSpeedWithParams:@
readAttributePlaybackSpeedWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributePlaybackSpeedWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributePlaybackSpeedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSeekRangeEndWithParams:@
readAttributeSeekRangeEndWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeSeekRangeEndWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeSeekRangeEndWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSeekRangeStartWithParams:@
readAttributeSeekRangeStartWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeSeekRangeStartWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeSeekRangeStartWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveAudioTrackWithParams:@
readAttributeActiveAudioTrackWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeActiveAudioTrackWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeActiveAudioTrackWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAvailableAudioTracksWithParams:@
readAttributeAvailableAudioTracksWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAvailableAudioTracksWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeAvailableAudioTracksWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveTextTrackWithParams:@
readAttributeActiveTextTrackWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeActiveTextTrackWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeActiveTextTrackWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAvailableTextTracksWithParams:@
readAttributeAvailableTextTracksWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAvailableTextTracksWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeAvailableTextTracksWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRReadParams params) => mtrClusterMediaPlayback -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMediaPlayback  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterMediaPlayback (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterMediaPlayback mtrClusterMediaPlayback => mtrClusterMediaPlayback -> IO (Id MTRClusterMediaPlayback)
init_ mtrClusterMediaPlayback  =
    sendMsg mtrClusterMediaPlayback (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterMediaPlayback)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMediaPlayback"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRDevice device, IsNSObject queue) => mtrClusterMediaPlayback -> device -> CUShort -> queue -> IO (Id MTRClusterMediaPlayback)
initWithDevice_endpoint_queue mtrClusterMediaPlayback  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterMediaPlayback (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- playWithParams:expectedValues:expectedValueInterval:completionHandler:@
playWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPlayParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "playWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- playWithExpectedValues:expectedValueInterval:completionHandler:@
playWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
playWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "playWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- pauseWithExpectedValues:expectedValueInterval:completionHandler:@
pauseWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "pauseWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStopPlaybackParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startOverWithParams:expectedValues:expectedValueInterval:completionHandler:@
startOverWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterStartOverParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "startOverWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startOverWithExpectedValues:expectedValueInterval:completionHandler:@
startOverWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startOverWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "startOverWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- previousWithParams:expectedValues:expectedValueInterval:completionHandler:@
previousWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterPreviousParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "previousWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- previousWithExpectedValues:expectedValueInterval:completionHandler:@
previousWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
previousWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "previousWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- nextWithParams:expectedValues:expectedValueInterval:completionHandler:@
nextWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterNextParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "nextWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- nextWithExpectedValues:expectedValueInterval:completionHandler:@
nextWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
nextWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "nextWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- rewindWithParams:expectedValues:expectedValueInterval:completionHandler:@
rewindWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterRewindParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "rewindWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- rewindWithExpectedValues:expectedValueInterval:completionHandler:@
rewindWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
rewindWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "rewindWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterFastForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fastForwardWithExpectedValues:expectedValueInterval:completionHandler:@
fastForwardWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
fastForwardWithExpectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterMediaPlayback (mkSelector "fastForwardWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipForwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSkipBackwardParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- seekWithParams:expectedValues:expectedValueInterval:completionHandler:@
seekWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRMediaPlaybackClusterSeekParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMediaPlayback -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
seekWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterMediaPlayback  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterMediaPlayback (mkSelector "seekWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMediaPlayback mtrClusterMediaPlayback, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMediaPlayback -> device -> endpointID -> queue -> IO (Id MTRClusterMediaPlayback)
initWithDevice_endpointID_queue mtrClusterMediaPlayback  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterMediaPlayback (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playWithParams:expectedValues:expectedValueInterval:completion:@
playWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
playWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "playWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @playWithExpectedValues:expectedValueInterval:completion:@
playWithExpectedValues_expectedValueInterval_completionSelector :: Selector
playWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "playWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
pauseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completionSelector :: Selector
pauseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startOverWithParams:expectedValues:expectedValueInterval:completion:@
startOverWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
startOverWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startOverWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startOverWithExpectedValues:expectedValueInterval:completion:@
startOverWithExpectedValues_expectedValueInterval_completionSelector :: Selector
startOverWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startOverWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @previousWithParams:expectedValues:expectedValueInterval:completion:@
previousWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
previousWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "previousWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @previousWithExpectedValues:expectedValueInterval:completion:@
previousWithExpectedValues_expectedValueInterval_completionSelector :: Selector
previousWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "previousWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @nextWithParams:expectedValues:expectedValueInterval:completion:@
nextWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
nextWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "nextWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @nextWithExpectedValues:expectedValueInterval:completion:@
nextWithExpectedValues_expectedValueInterval_completionSelector :: Selector
nextWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "nextWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @rewindWithParams:expectedValues:expectedValueInterval:completion:@
rewindWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
rewindWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "rewindWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @rewindWithExpectedValues:expectedValueInterval:completion:@
rewindWithExpectedValues_expectedValueInterval_completionSelector :: Selector
rewindWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "rewindWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @fastForwardWithParams:expectedValues:expectedValueInterval:completion:@
fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
fastForwardWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "fastForwardWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @fastForwardWithExpectedValues:expectedValueInterval:completion:@
fastForwardWithExpectedValues_expectedValueInterval_completionSelector :: Selector
fastForwardWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "fastForwardWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipForwardWithParams:expectedValues:expectedValueInterval:completion:@
skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
skipForwardWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipForwardWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipBackwardWithParams:expectedValues:expectedValueInterval:completion:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
skipBackwardWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipBackwardWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @seekWithParams:expectedValues:expectedValueInterval:completion:@
seekWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
seekWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "seekWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
activateAudioTrackWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "activateAudioTrackWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
activateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "activateTextTrackWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
deactivateTextTrackWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "deactivateTextTrackWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:@
deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector :: Selector
deactivateTextTrackWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "deactivateTextTrackWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParamsSelector :: Selector
readAttributeCurrentStateWithParamsSelector = mkSelector "readAttributeCurrentStateWithParams:"

-- | @Selector@ for @readAttributeStartTimeWithParams:@
readAttributeStartTimeWithParamsSelector :: Selector
readAttributeStartTimeWithParamsSelector = mkSelector "readAttributeStartTimeWithParams:"

-- | @Selector@ for @readAttributeDurationWithParams:@
readAttributeDurationWithParamsSelector :: Selector
readAttributeDurationWithParamsSelector = mkSelector "readAttributeDurationWithParams:"

-- | @Selector@ for @readAttributeSampledPositionWithParams:@
readAttributeSampledPositionWithParamsSelector :: Selector
readAttributeSampledPositionWithParamsSelector = mkSelector "readAttributeSampledPositionWithParams:"

-- | @Selector@ for @readAttributePlaybackSpeedWithParams:@
readAttributePlaybackSpeedWithParamsSelector :: Selector
readAttributePlaybackSpeedWithParamsSelector = mkSelector "readAttributePlaybackSpeedWithParams:"

-- | @Selector@ for @readAttributeSeekRangeEndWithParams:@
readAttributeSeekRangeEndWithParamsSelector :: Selector
readAttributeSeekRangeEndWithParamsSelector = mkSelector "readAttributeSeekRangeEndWithParams:"

-- | @Selector@ for @readAttributeSeekRangeStartWithParams:@
readAttributeSeekRangeStartWithParamsSelector :: Selector
readAttributeSeekRangeStartWithParamsSelector = mkSelector "readAttributeSeekRangeStartWithParams:"

-- | @Selector@ for @readAttributeActiveAudioTrackWithParams:@
readAttributeActiveAudioTrackWithParamsSelector :: Selector
readAttributeActiveAudioTrackWithParamsSelector = mkSelector "readAttributeActiveAudioTrackWithParams:"

-- | @Selector@ for @readAttributeAvailableAudioTracksWithParams:@
readAttributeAvailableAudioTracksWithParamsSelector :: Selector
readAttributeAvailableAudioTracksWithParamsSelector = mkSelector "readAttributeAvailableAudioTracksWithParams:"

-- | @Selector@ for @readAttributeActiveTextTrackWithParams:@
readAttributeActiveTextTrackWithParamsSelector :: Selector
readAttributeActiveTextTrackWithParamsSelector = mkSelector "readAttributeActiveTextTrackWithParams:"

-- | @Selector@ for @readAttributeAvailableTextTracksWithParams:@
readAttributeAvailableTextTracksWithParamsSelector :: Selector
readAttributeAvailableTextTracksWithParamsSelector = mkSelector "readAttributeAvailableTextTracksWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @playWithParams:expectedValues:expectedValueInterval:completionHandler:@
playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
playWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "playWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @playWithExpectedValues:expectedValueInterval:completionHandler:@
playWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
playWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "playWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completionHandler:@
pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
pauseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completionHandler:@
pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
pauseWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopPlaybackWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopPlaybackWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:@
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
stopPlaybackWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopPlaybackWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @startOverWithParams:expectedValues:expectedValueInterval:completionHandler:@
startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
startOverWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "startOverWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @startOverWithExpectedValues:expectedValueInterval:completionHandler:@
startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
startOverWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "startOverWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @previousWithParams:expectedValues:expectedValueInterval:completionHandler:@
previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
previousWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "previousWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @previousWithExpectedValues:expectedValueInterval:completionHandler:@
previousWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
previousWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "previousWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @nextWithParams:expectedValues:expectedValueInterval:completionHandler:@
nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
nextWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "nextWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @nextWithExpectedValues:expectedValueInterval:completionHandler:@
nextWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
nextWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "nextWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @rewindWithParams:expectedValues:expectedValueInterval:completionHandler:@
rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
rewindWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "rewindWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @rewindWithExpectedValues:expectedValueInterval:completionHandler:@
rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
rewindWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "rewindWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
fastForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "fastForwardWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @fastForwardWithExpectedValues:expectedValueInterval:completionHandler:@
fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
fastForwardWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "fastForwardWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
skipForwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "skipForwardWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:@
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
skipBackwardWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "skipBackwardWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @seekWithParams:expectedValues:expectedValueInterval:completionHandler:@
seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
seekWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "seekWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

