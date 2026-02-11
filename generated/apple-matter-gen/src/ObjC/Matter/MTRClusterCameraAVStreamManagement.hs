{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Camera AV Stream Management    The Camera AV Stream Management cluster is used to allow clients to manage, control, and configure various audio, video, and snapshot streams on a camera.
--
-- Generated bindings for @MTRClusterCameraAVStreamManagement@.
module ObjC.Matter.MTRClusterCameraAVStreamManagement
  ( MTRClusterCameraAVStreamManagement
  , IsMTRClusterCameraAVStreamManagement(..)
  , audioStreamAllocateWithParams_expectedValues_expectedValueInterval_completion
  , audioStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion
  , videoStreamAllocateWithParams_expectedValues_expectedValueInterval_completion
  , videoStreamModifyWithParams_expectedValues_expectedValueInterval_completion
  , videoStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion
  , snapshotStreamAllocateWithParams_expectedValues_expectedValueInterval_completion
  , snapshotStreamModifyWithParams_expectedValues_expectedValueInterval_completion
  , snapshotStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion
  , setStreamPrioritiesWithParams_expectedValues_expectedValueInterval_completion
  , captureSnapshotWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxConcurrentEncodersWithParams
  , readAttributeMaxEncodedPixelRateWithParams
  , readAttributeVideoSensorParamsWithParams
  , readAttributeNightVisionUsesInfraredWithParams
  , readAttributeMinViewportResolutionWithParams
  , readAttributeRateDistortionTradeOffPointsWithParams
  , readAttributeMaxContentBufferSizeWithParams
  , readAttributeMicrophoneCapabilitiesWithParams
  , readAttributeSpeakerCapabilitiesWithParams
  , readAttributeTwoWayTalkSupportWithParams
  , readAttributeSnapshotCapabilitiesWithParams
  , readAttributeMaxNetworkBandwidthWithParams
  , readAttributeCurrentFrameRateWithParams
  , readAttributeHDRModeEnabledWithParams
  , writeAttributeHDRModeEnabledWithValue_expectedValueInterval
  , writeAttributeHDRModeEnabledWithValue_expectedValueInterval_params
  , readAttributeSupportedStreamUsagesWithParams
  , readAttributeAllocatedVideoStreamsWithParams
  , readAttributeAllocatedAudioStreamsWithParams
  , readAttributeAllocatedSnapshotStreamsWithParams
  , readAttributeStreamUsagePrioritiesWithParams
  , readAttributeSoftRecordingPrivacyModeEnabledWithParams
  , writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval
  , writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval_params
  , readAttributeSoftLivestreamPrivacyModeEnabledWithParams
  , writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval
  , writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval_params
  , readAttributeHardPrivacyModeOnWithParams
  , readAttributeNightVisionWithParams
  , writeAttributeNightVisionWithValue_expectedValueInterval
  , writeAttributeNightVisionWithValue_expectedValueInterval_params
  , readAttributeNightVisionIllumWithParams
  , writeAttributeNightVisionIllumWithValue_expectedValueInterval
  , writeAttributeNightVisionIllumWithValue_expectedValueInterval_params
  , readAttributeViewportWithParams
  , writeAttributeViewportWithValue_expectedValueInterval
  , writeAttributeViewportWithValue_expectedValueInterval_params
  , readAttributeSpeakerMutedWithParams
  , writeAttributeSpeakerMutedWithValue_expectedValueInterval
  , writeAttributeSpeakerMutedWithValue_expectedValueInterval_params
  , readAttributeSpeakerVolumeLevelWithParams
  , writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval
  , writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval_params
  , readAttributeSpeakerMaxLevelWithParams
  , readAttributeSpeakerMinLevelWithParams
  , readAttributeMicrophoneMutedWithParams
  , writeAttributeMicrophoneMutedWithValue_expectedValueInterval
  , writeAttributeMicrophoneMutedWithValue_expectedValueInterval_params
  , readAttributeMicrophoneVolumeLevelWithParams
  , writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval
  , writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval_params
  , readAttributeMicrophoneMaxLevelWithParams
  , readAttributeMicrophoneMinLevelWithParams
  , readAttributeMicrophoneAGCEnabledWithParams
  , writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval
  , writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval_params
  , readAttributeImageRotationWithParams
  , writeAttributeImageRotationWithValue_expectedValueInterval
  , writeAttributeImageRotationWithValue_expectedValueInterval_params
  , readAttributeImageFlipHorizontalWithParams
  , writeAttributeImageFlipHorizontalWithValue_expectedValueInterval
  , writeAttributeImageFlipHorizontalWithValue_expectedValueInterval_params
  , readAttributeImageFlipVerticalWithParams
  , writeAttributeImageFlipVerticalWithValue_expectedValueInterval
  , writeAttributeImageFlipVerticalWithValue_expectedValueInterval_params
  , readAttributeLocalVideoRecordingEnabledWithParams
  , writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval
  , writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval_params
  , readAttributeLocalSnapshotRecordingEnabledWithParams
  , writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval
  , writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval_params
  , readAttributeStatusLightEnabledWithParams
  , writeAttributeStatusLightEnabledWithValue_expectedValueInterval
  , writeAttributeStatusLightEnabledWithValue_expectedValueInterval_params
  , readAttributeStatusLightBrightnessWithParams
  , writeAttributeStatusLightBrightnessWithValue_expectedValueInterval
  , writeAttributeStatusLightBrightnessWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , audioStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector
  , audioStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector
  , videoStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector
  , videoStreamModifyWithParams_expectedValues_expectedValueInterval_completionSelector
  , videoStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector
  , snapshotStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector
  , snapshotStreamModifyWithParams_expectedValues_expectedValueInterval_completionSelector
  , snapshotStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector
  , setStreamPrioritiesWithParams_expectedValues_expectedValueInterval_completionSelector
  , captureSnapshotWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeMaxConcurrentEncodersWithParamsSelector
  , readAttributeMaxEncodedPixelRateWithParamsSelector
  , readAttributeVideoSensorParamsWithParamsSelector
  , readAttributeNightVisionUsesInfraredWithParamsSelector
  , readAttributeMinViewportResolutionWithParamsSelector
  , readAttributeRateDistortionTradeOffPointsWithParamsSelector
  , readAttributeMaxContentBufferSizeWithParamsSelector
  , readAttributeMicrophoneCapabilitiesWithParamsSelector
  , readAttributeSpeakerCapabilitiesWithParamsSelector
  , readAttributeTwoWayTalkSupportWithParamsSelector
  , readAttributeSnapshotCapabilitiesWithParamsSelector
  , readAttributeMaxNetworkBandwidthWithParamsSelector
  , readAttributeCurrentFrameRateWithParamsSelector
  , readAttributeHDRModeEnabledWithParamsSelector
  , writeAttributeHDRModeEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeHDRModeEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeSupportedStreamUsagesWithParamsSelector
  , readAttributeAllocatedVideoStreamsWithParamsSelector
  , readAttributeAllocatedAudioStreamsWithParamsSelector
  , readAttributeAllocatedSnapshotStreamsWithParamsSelector
  , readAttributeStreamUsagePrioritiesWithParamsSelector
  , readAttributeSoftRecordingPrivacyModeEnabledWithParamsSelector
  , writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeSoftLivestreamPrivacyModeEnabledWithParamsSelector
  , writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeHardPrivacyModeOnWithParamsSelector
  , readAttributeNightVisionWithParamsSelector
  , writeAttributeNightVisionWithValue_expectedValueIntervalSelector
  , writeAttributeNightVisionWithValue_expectedValueInterval_paramsSelector
  , readAttributeNightVisionIllumWithParamsSelector
  , writeAttributeNightVisionIllumWithValue_expectedValueIntervalSelector
  , writeAttributeNightVisionIllumWithValue_expectedValueInterval_paramsSelector
  , readAttributeViewportWithParamsSelector
  , writeAttributeViewportWithValue_expectedValueIntervalSelector
  , writeAttributeViewportWithValue_expectedValueInterval_paramsSelector
  , readAttributeSpeakerMutedWithParamsSelector
  , writeAttributeSpeakerMutedWithValue_expectedValueIntervalSelector
  , writeAttributeSpeakerMutedWithValue_expectedValueInterval_paramsSelector
  , readAttributeSpeakerVolumeLevelWithParamsSelector
  , writeAttributeSpeakerVolumeLevelWithValue_expectedValueIntervalSelector
  , writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeSpeakerMaxLevelWithParamsSelector
  , readAttributeSpeakerMinLevelWithParamsSelector
  , readAttributeMicrophoneMutedWithParamsSelector
  , writeAttributeMicrophoneMutedWithValue_expectedValueIntervalSelector
  , writeAttributeMicrophoneMutedWithValue_expectedValueInterval_paramsSelector
  , readAttributeMicrophoneVolumeLevelWithParamsSelector
  , writeAttributeMicrophoneVolumeLevelWithValue_expectedValueIntervalSelector
  , writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval_paramsSelector
  , readAttributeMicrophoneMaxLevelWithParamsSelector
  , readAttributeMicrophoneMinLevelWithParamsSelector
  , readAttributeMicrophoneAGCEnabledWithParamsSelector
  , writeAttributeMicrophoneAGCEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeImageRotationWithParamsSelector
  , writeAttributeImageRotationWithValue_expectedValueIntervalSelector
  , writeAttributeImageRotationWithValue_expectedValueInterval_paramsSelector
  , readAttributeImageFlipHorizontalWithParamsSelector
  , writeAttributeImageFlipHorizontalWithValue_expectedValueIntervalSelector
  , writeAttributeImageFlipHorizontalWithValue_expectedValueInterval_paramsSelector
  , readAttributeImageFlipVerticalWithParamsSelector
  , writeAttributeImageFlipVerticalWithValue_expectedValueIntervalSelector
  , writeAttributeImageFlipVerticalWithValue_expectedValueInterval_paramsSelector
  , readAttributeLocalVideoRecordingEnabledWithParamsSelector
  , writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeLocalSnapshotRecordingEnabledWithParamsSelector
  , writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeStatusLightEnabledWithParamsSelector
  , writeAttributeStatusLightEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeStatusLightEnabledWithValue_expectedValueInterval_paramsSelector
  , readAttributeStatusLightBrightnessWithParamsSelector
  , writeAttributeStatusLightBrightnessWithValue_expectedValueIntervalSelector
  , writeAttributeStatusLightBrightnessWithValue_expectedValueInterval_paramsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
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

-- | @- audioStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:@
audioStreamAllocateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterAudioStreamAllocateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
audioStreamAllocateWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "audioStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- audioStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:@
audioStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterAudioStreamDeallocateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
audioStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "audioStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- videoStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:@
videoStreamAllocateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterVideoStreamAllocateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
videoStreamAllocateWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "videoStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- videoStreamModifyWithParams:expectedValues:expectedValueInterval:completion:@
videoStreamModifyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterVideoStreamModifyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
videoStreamModifyWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "videoStreamModifyWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- videoStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:@
videoStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterVideoStreamDeallocateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
videoStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "videoStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- snapshotStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:@
snapshotStreamAllocateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
snapshotStreamAllocateWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "snapshotStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- snapshotStreamModifyWithParams:expectedValues:expectedValueInterval:completion:@
snapshotStreamModifyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterSnapshotStreamModifyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
snapshotStreamModifyWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "snapshotStreamModifyWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- snapshotStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:@
snapshotStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterSnapshotStreamDeallocateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
snapshotStreamDeallocateWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "snapshotStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setStreamPrioritiesWithParams:expectedValues:expectedValueInterval:completion:@
setStreamPrioritiesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterSetStreamPrioritiesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setStreamPrioritiesWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "setStreamPrioritiesWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- captureSnapshotWithParams:expectedValues:expectedValueInterval:completion:@
captureSnapshotWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRCameraAVStreamManagementClusterCaptureSnapshotParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
captureSnapshotWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVStreamManagement  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "captureSnapshotWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeMaxConcurrentEncodersWithParams:@
readAttributeMaxConcurrentEncodersWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMaxConcurrentEncodersWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMaxConcurrentEncodersWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxEncodedPixelRateWithParams:@
readAttributeMaxEncodedPixelRateWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMaxEncodedPixelRateWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMaxEncodedPixelRateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeVideoSensorParamsWithParams:@
readAttributeVideoSensorParamsWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeVideoSensorParamsWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeVideoSensorParamsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNightVisionUsesInfraredWithParams:@
readAttributeNightVisionUsesInfraredWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeNightVisionUsesInfraredWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeNightVisionUsesInfraredWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMinViewportResolutionWithParams:@
readAttributeMinViewportResolutionWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMinViewportResolutionWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMinViewportResolutionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRateDistortionTradeOffPointsWithParams:@
readAttributeRateDistortionTradeOffPointsWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeRateDistortionTradeOffPointsWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeRateDistortionTradeOffPointsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxContentBufferSizeWithParams:@
readAttributeMaxContentBufferSizeWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMaxContentBufferSizeWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMaxContentBufferSizeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMicrophoneCapabilitiesWithParams:@
readAttributeMicrophoneCapabilitiesWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMicrophoneCapabilitiesWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMicrophoneCapabilitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpeakerCapabilitiesWithParams:@
readAttributeSpeakerCapabilitiesWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSpeakerCapabilitiesWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSpeakerCapabilitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTwoWayTalkSupportWithParams:@
readAttributeTwoWayTalkSupportWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeTwoWayTalkSupportWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeTwoWayTalkSupportWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSnapshotCapabilitiesWithParams:@
readAttributeSnapshotCapabilitiesWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSnapshotCapabilitiesWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSnapshotCapabilitiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMaxNetworkBandwidthWithParams:@
readAttributeMaxNetworkBandwidthWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMaxNetworkBandwidthWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMaxNetworkBandwidthWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeCurrentFrameRateWithParams:@
readAttributeCurrentFrameRateWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeCurrentFrameRateWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeCurrentFrameRateWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeHDRModeEnabledWithParams:@
readAttributeHDRModeEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeHDRModeEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeHDRModeEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeHDRModeEnabledWithValue:expectedValueInterval:@
writeAttributeHDRModeEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeHDRModeEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeHDRModeEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeHDRModeEnabledWithValue:expectedValueInterval:params:@
writeAttributeHDRModeEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeHDRModeEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeHDRModeEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSupportedStreamUsagesWithParams:@
readAttributeSupportedStreamUsagesWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSupportedStreamUsagesWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSupportedStreamUsagesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAllocatedVideoStreamsWithParams:@
readAttributeAllocatedVideoStreamsWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeAllocatedVideoStreamsWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeAllocatedVideoStreamsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAllocatedAudioStreamsWithParams:@
readAttributeAllocatedAudioStreamsWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeAllocatedAudioStreamsWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeAllocatedAudioStreamsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAllocatedSnapshotStreamsWithParams:@
readAttributeAllocatedSnapshotStreamsWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeAllocatedSnapshotStreamsWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeAllocatedSnapshotStreamsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStreamUsagePrioritiesWithParams:@
readAttributeStreamUsagePrioritiesWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeStreamUsagePrioritiesWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeStreamUsagePrioritiesWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSoftRecordingPrivacyModeEnabledWithParams:@
readAttributeSoftRecordingPrivacyModeEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSoftRecordingPrivacyModeEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSoftRecordingPrivacyModeEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:@
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:params:@
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSoftLivestreamPrivacyModeEnabledWithParams:@
readAttributeSoftLivestreamPrivacyModeEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSoftLivestreamPrivacyModeEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSoftLivestreamPrivacyModeEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:@
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:params:@
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeHardPrivacyModeOnWithParams:@
readAttributeHardPrivacyModeOnWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeHardPrivacyModeOnWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeHardPrivacyModeOnWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNightVisionWithParams:@
readAttributeNightVisionWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeNightVisionWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeNightVisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeNightVisionWithValue:expectedValueInterval:@
writeAttributeNightVisionWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNightVisionWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeNightVisionWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeNightVisionWithValue:expectedValueInterval:params:@
writeAttributeNightVisionWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNightVisionWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeNightVisionWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeNightVisionIllumWithParams:@
readAttributeNightVisionIllumWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeNightVisionIllumWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeNightVisionIllumWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeNightVisionIllumWithValue:expectedValueInterval:@
writeAttributeNightVisionIllumWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNightVisionIllumWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeNightVisionIllumWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeNightVisionIllumWithValue:expectedValueInterval:params:@
writeAttributeNightVisionIllumWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNightVisionIllumWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeNightVisionIllumWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeViewportWithParams:@
readAttributeViewportWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeViewportWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeViewportWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeViewportWithValue:expectedValueInterval:@
writeAttributeViewportWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeViewportWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeViewportWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeViewportWithValue:expectedValueInterval:params:@
writeAttributeViewportWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeViewportWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeViewportWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSpeakerMutedWithParams:@
readAttributeSpeakerMutedWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSpeakerMutedWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSpeakerMutedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSpeakerMutedWithValue:expectedValueInterval:@
writeAttributeSpeakerMutedWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSpeakerMutedWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSpeakerMutedWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSpeakerMutedWithValue:expectedValueInterval:params:@
writeAttributeSpeakerMutedWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSpeakerMutedWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSpeakerMutedWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSpeakerVolumeLevelWithParams:@
readAttributeSpeakerVolumeLevelWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSpeakerVolumeLevelWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSpeakerVolumeLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:@
writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:params:@
writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeSpeakerMaxLevelWithParams:@
readAttributeSpeakerMaxLevelWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSpeakerMaxLevelWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSpeakerMaxLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSpeakerMinLevelWithParams:@
readAttributeSpeakerMinLevelWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeSpeakerMinLevelWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeSpeakerMinLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMicrophoneMutedWithParams:@
readAttributeMicrophoneMutedWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMicrophoneMutedWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMicrophoneMutedWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMicrophoneMutedWithValue:expectedValueInterval:@
writeAttributeMicrophoneMutedWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMicrophoneMutedWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeMicrophoneMutedWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMicrophoneMutedWithValue:expectedValueInterval:params:@
writeAttributeMicrophoneMutedWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMicrophoneMutedWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeMicrophoneMutedWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMicrophoneVolumeLevelWithParams:@
readAttributeMicrophoneVolumeLevelWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMicrophoneVolumeLevelWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMicrophoneVolumeLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:@
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:params:@
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeMicrophoneMaxLevelWithParams:@
readAttributeMicrophoneMaxLevelWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMicrophoneMaxLevelWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMicrophoneMaxLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMicrophoneMinLevelWithParams:@
readAttributeMicrophoneMinLevelWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMicrophoneMinLevelWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMicrophoneMinLevelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMicrophoneAGCEnabledWithParams:@
readAttributeMicrophoneAGCEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeMicrophoneAGCEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeMicrophoneAGCEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:@
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:params:@
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeImageRotationWithParams:@
readAttributeImageRotationWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeImageRotationWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeImageRotationWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeImageRotationWithValue:expectedValueInterval:@
writeAttributeImageRotationWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeImageRotationWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeImageRotationWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeImageRotationWithValue:expectedValueInterval:params:@
writeAttributeImageRotationWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeImageRotationWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeImageRotationWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeImageFlipHorizontalWithParams:@
readAttributeImageFlipHorizontalWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeImageFlipHorizontalWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeImageFlipHorizontalWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:@
writeAttributeImageFlipHorizontalWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeImageFlipHorizontalWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:params:@
writeAttributeImageFlipHorizontalWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeImageFlipHorizontalWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeImageFlipVerticalWithParams:@
readAttributeImageFlipVerticalWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeImageFlipVerticalWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeImageFlipVerticalWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeImageFlipVerticalWithValue:expectedValueInterval:@
writeAttributeImageFlipVerticalWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeImageFlipVerticalWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeImageFlipVerticalWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeImageFlipVerticalWithValue:expectedValueInterval:params:@
writeAttributeImageFlipVerticalWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeImageFlipVerticalWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeImageFlipVerticalWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLocalVideoRecordingEnabledWithParams:@
readAttributeLocalVideoRecordingEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeLocalVideoRecordingEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeLocalVideoRecordingEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:@
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:params:@
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeLocalSnapshotRecordingEnabledWithParams:@
readAttributeLocalSnapshotRecordingEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeLocalSnapshotRecordingEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeLocalSnapshotRecordingEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:@
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:params:@
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeStatusLightEnabledWithParams:@
readAttributeStatusLightEnabledWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeStatusLightEnabledWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeStatusLightEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeStatusLightEnabledWithValue:expectedValueInterval:@
writeAttributeStatusLightEnabledWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStatusLightEnabledWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeStatusLightEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeStatusLightEnabledWithValue:expectedValueInterval:params:@
writeAttributeStatusLightEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStatusLightEnabledWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeStatusLightEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeStatusLightBrightnessWithParams:@
readAttributeStatusLightBrightnessWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeStatusLightBrightnessWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeStatusLightBrightnessWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:@
writeAttributeStatusLightBrightnessWithValue_expectedValueInterval :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStatusLightBrightnessWithValue_expectedValueInterval mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:params:@
writeAttributeStatusLightBrightnessWithValue_expectedValueInterval_params :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterCameraAVStreamManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStatusLightBrightnessWithValue_expectedValueInterval_params mtrClusterCameraAVStreamManagement  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRReadParams params) => mtrClusterCameraAVStreamManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCameraAVStreamManagement  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterCameraAVStreamManagement (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement => mtrClusterCameraAVStreamManagement -> IO (Id MTRClusterCameraAVStreamManagement)
init_ mtrClusterCameraAVStreamManagement  =
    sendMsg mtrClusterCameraAVStreamManagement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterCameraAVStreamManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCameraAVStreamManagement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCameraAVStreamManagement mtrClusterCameraAVStreamManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCameraAVStreamManagement -> device -> endpointID -> queue -> IO (Id MTRClusterCameraAVStreamManagement)
initWithDevice_endpointID_queue mtrClusterCameraAVStreamManagement  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterCameraAVStreamManagement (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:@
audioStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
audioStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "audioStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @audioStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:@
audioStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
audioStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "audioStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @videoStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:@
videoStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
videoStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "videoStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @videoStreamModifyWithParams:expectedValues:expectedValueInterval:completion:@
videoStreamModifyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
videoStreamModifyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "videoStreamModifyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @videoStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:@
videoStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
videoStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "videoStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @snapshotStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:@
snapshotStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
snapshotStreamAllocateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "snapshotStreamAllocateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @snapshotStreamModifyWithParams:expectedValues:expectedValueInterval:completion:@
snapshotStreamModifyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
snapshotStreamModifyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "snapshotStreamModifyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @snapshotStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:@
snapshotStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
snapshotStreamDeallocateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "snapshotStreamDeallocateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setStreamPrioritiesWithParams:expectedValues:expectedValueInterval:completion:@
setStreamPrioritiesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setStreamPrioritiesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setStreamPrioritiesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @captureSnapshotWithParams:expectedValues:expectedValueInterval:completion:@
captureSnapshotWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
captureSnapshotWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "captureSnapshotWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxConcurrentEncodersWithParams:@
readAttributeMaxConcurrentEncodersWithParamsSelector :: Selector
readAttributeMaxConcurrentEncodersWithParamsSelector = mkSelector "readAttributeMaxConcurrentEncodersWithParams:"

-- | @Selector@ for @readAttributeMaxEncodedPixelRateWithParams:@
readAttributeMaxEncodedPixelRateWithParamsSelector :: Selector
readAttributeMaxEncodedPixelRateWithParamsSelector = mkSelector "readAttributeMaxEncodedPixelRateWithParams:"

-- | @Selector@ for @readAttributeVideoSensorParamsWithParams:@
readAttributeVideoSensorParamsWithParamsSelector :: Selector
readAttributeVideoSensorParamsWithParamsSelector = mkSelector "readAttributeVideoSensorParamsWithParams:"

-- | @Selector@ for @readAttributeNightVisionUsesInfraredWithParams:@
readAttributeNightVisionUsesInfraredWithParamsSelector :: Selector
readAttributeNightVisionUsesInfraredWithParamsSelector = mkSelector "readAttributeNightVisionUsesInfraredWithParams:"

-- | @Selector@ for @readAttributeMinViewportResolutionWithParams:@
readAttributeMinViewportResolutionWithParamsSelector :: Selector
readAttributeMinViewportResolutionWithParamsSelector = mkSelector "readAttributeMinViewportResolutionWithParams:"

-- | @Selector@ for @readAttributeRateDistortionTradeOffPointsWithParams:@
readAttributeRateDistortionTradeOffPointsWithParamsSelector :: Selector
readAttributeRateDistortionTradeOffPointsWithParamsSelector = mkSelector "readAttributeRateDistortionTradeOffPointsWithParams:"

-- | @Selector@ for @readAttributeMaxContentBufferSizeWithParams:@
readAttributeMaxContentBufferSizeWithParamsSelector :: Selector
readAttributeMaxContentBufferSizeWithParamsSelector = mkSelector "readAttributeMaxContentBufferSizeWithParams:"

-- | @Selector@ for @readAttributeMicrophoneCapabilitiesWithParams:@
readAttributeMicrophoneCapabilitiesWithParamsSelector :: Selector
readAttributeMicrophoneCapabilitiesWithParamsSelector = mkSelector "readAttributeMicrophoneCapabilitiesWithParams:"

-- | @Selector@ for @readAttributeSpeakerCapabilitiesWithParams:@
readAttributeSpeakerCapabilitiesWithParamsSelector :: Selector
readAttributeSpeakerCapabilitiesWithParamsSelector = mkSelector "readAttributeSpeakerCapabilitiesWithParams:"

-- | @Selector@ for @readAttributeTwoWayTalkSupportWithParams:@
readAttributeTwoWayTalkSupportWithParamsSelector :: Selector
readAttributeTwoWayTalkSupportWithParamsSelector = mkSelector "readAttributeTwoWayTalkSupportWithParams:"

-- | @Selector@ for @readAttributeSnapshotCapabilitiesWithParams:@
readAttributeSnapshotCapabilitiesWithParamsSelector :: Selector
readAttributeSnapshotCapabilitiesWithParamsSelector = mkSelector "readAttributeSnapshotCapabilitiesWithParams:"

-- | @Selector@ for @readAttributeMaxNetworkBandwidthWithParams:@
readAttributeMaxNetworkBandwidthWithParamsSelector :: Selector
readAttributeMaxNetworkBandwidthWithParamsSelector = mkSelector "readAttributeMaxNetworkBandwidthWithParams:"

-- | @Selector@ for @readAttributeCurrentFrameRateWithParams:@
readAttributeCurrentFrameRateWithParamsSelector :: Selector
readAttributeCurrentFrameRateWithParamsSelector = mkSelector "readAttributeCurrentFrameRateWithParams:"

-- | @Selector@ for @readAttributeHDRModeEnabledWithParams:@
readAttributeHDRModeEnabledWithParamsSelector :: Selector
readAttributeHDRModeEnabledWithParamsSelector = mkSelector "readAttributeHDRModeEnabledWithParams:"

-- | @Selector@ for @writeAttributeHDRModeEnabledWithValue:expectedValueInterval:@
writeAttributeHDRModeEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeHDRModeEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeHDRModeEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeHDRModeEnabledWithValue:expectedValueInterval:params:@
writeAttributeHDRModeEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeHDRModeEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeHDRModeEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedStreamUsagesWithParams:@
readAttributeSupportedStreamUsagesWithParamsSelector :: Selector
readAttributeSupportedStreamUsagesWithParamsSelector = mkSelector "readAttributeSupportedStreamUsagesWithParams:"

-- | @Selector@ for @readAttributeAllocatedVideoStreamsWithParams:@
readAttributeAllocatedVideoStreamsWithParamsSelector :: Selector
readAttributeAllocatedVideoStreamsWithParamsSelector = mkSelector "readAttributeAllocatedVideoStreamsWithParams:"

-- | @Selector@ for @readAttributeAllocatedAudioStreamsWithParams:@
readAttributeAllocatedAudioStreamsWithParamsSelector :: Selector
readAttributeAllocatedAudioStreamsWithParamsSelector = mkSelector "readAttributeAllocatedAudioStreamsWithParams:"

-- | @Selector@ for @readAttributeAllocatedSnapshotStreamsWithParams:@
readAttributeAllocatedSnapshotStreamsWithParamsSelector :: Selector
readAttributeAllocatedSnapshotStreamsWithParamsSelector = mkSelector "readAttributeAllocatedSnapshotStreamsWithParams:"

-- | @Selector@ for @readAttributeStreamUsagePrioritiesWithParams:@
readAttributeStreamUsagePrioritiesWithParamsSelector :: Selector
readAttributeStreamUsagePrioritiesWithParamsSelector = mkSelector "readAttributeStreamUsagePrioritiesWithParams:"

-- | @Selector@ for @readAttributeSoftRecordingPrivacyModeEnabledWithParams:@
readAttributeSoftRecordingPrivacyModeEnabledWithParamsSelector :: Selector
readAttributeSoftRecordingPrivacyModeEnabledWithParamsSelector = mkSelector "readAttributeSoftRecordingPrivacyModeEnabledWithParams:"

-- | @Selector@ for @writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:@
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:params:@
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSoftRecordingPrivacyModeEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSoftRecordingPrivacyModeEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSoftLivestreamPrivacyModeEnabledWithParams:@
readAttributeSoftLivestreamPrivacyModeEnabledWithParamsSelector :: Selector
readAttributeSoftLivestreamPrivacyModeEnabledWithParamsSelector = mkSelector "readAttributeSoftLivestreamPrivacyModeEnabledWithParams:"

-- | @Selector@ for @writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:@
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:params:@
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSoftLivestreamPrivacyModeEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSoftLivestreamPrivacyModeEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeHardPrivacyModeOnWithParams:@
readAttributeHardPrivacyModeOnWithParamsSelector :: Selector
readAttributeHardPrivacyModeOnWithParamsSelector = mkSelector "readAttributeHardPrivacyModeOnWithParams:"

-- | @Selector@ for @readAttributeNightVisionWithParams:@
readAttributeNightVisionWithParamsSelector :: Selector
readAttributeNightVisionWithParamsSelector = mkSelector "readAttributeNightVisionWithParams:"

-- | @Selector@ for @writeAttributeNightVisionWithValue:expectedValueInterval:@
writeAttributeNightVisionWithValue_expectedValueIntervalSelector :: Selector
writeAttributeNightVisionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeNightVisionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeNightVisionWithValue:expectedValueInterval:params:@
writeAttributeNightVisionWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeNightVisionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeNightVisionWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNightVisionIllumWithParams:@
readAttributeNightVisionIllumWithParamsSelector :: Selector
readAttributeNightVisionIllumWithParamsSelector = mkSelector "readAttributeNightVisionIllumWithParams:"

-- | @Selector@ for @writeAttributeNightVisionIllumWithValue:expectedValueInterval:@
writeAttributeNightVisionIllumWithValue_expectedValueIntervalSelector :: Selector
writeAttributeNightVisionIllumWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeNightVisionIllumWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeNightVisionIllumWithValue:expectedValueInterval:params:@
writeAttributeNightVisionIllumWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeNightVisionIllumWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeNightVisionIllumWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeViewportWithParams:@
readAttributeViewportWithParamsSelector :: Selector
readAttributeViewportWithParamsSelector = mkSelector "readAttributeViewportWithParams:"

-- | @Selector@ for @writeAttributeViewportWithValue:expectedValueInterval:@
writeAttributeViewportWithValue_expectedValueIntervalSelector :: Selector
writeAttributeViewportWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeViewportWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeViewportWithValue:expectedValueInterval:params:@
writeAttributeViewportWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeViewportWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeViewportWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSpeakerMutedWithParams:@
readAttributeSpeakerMutedWithParamsSelector :: Selector
readAttributeSpeakerMutedWithParamsSelector = mkSelector "readAttributeSpeakerMutedWithParams:"

-- | @Selector@ for @writeAttributeSpeakerMutedWithValue:expectedValueInterval:@
writeAttributeSpeakerMutedWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSpeakerMutedWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSpeakerMutedWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSpeakerMutedWithValue:expectedValueInterval:params:@
writeAttributeSpeakerMutedWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSpeakerMutedWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSpeakerMutedWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSpeakerVolumeLevelWithParams:@
readAttributeSpeakerVolumeLevelWithParamsSelector :: Selector
readAttributeSpeakerVolumeLevelWithParamsSelector = mkSelector "readAttributeSpeakerVolumeLevelWithParams:"

-- | @Selector@ for @writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:@
writeAttributeSpeakerVolumeLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSpeakerVolumeLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:params:@
writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSpeakerVolumeLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSpeakerVolumeLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSpeakerMaxLevelWithParams:@
readAttributeSpeakerMaxLevelWithParamsSelector :: Selector
readAttributeSpeakerMaxLevelWithParamsSelector = mkSelector "readAttributeSpeakerMaxLevelWithParams:"

-- | @Selector@ for @readAttributeSpeakerMinLevelWithParams:@
readAttributeSpeakerMinLevelWithParamsSelector :: Selector
readAttributeSpeakerMinLevelWithParamsSelector = mkSelector "readAttributeSpeakerMinLevelWithParams:"

-- | @Selector@ for @readAttributeMicrophoneMutedWithParams:@
readAttributeMicrophoneMutedWithParamsSelector :: Selector
readAttributeMicrophoneMutedWithParamsSelector = mkSelector "readAttributeMicrophoneMutedWithParams:"

-- | @Selector@ for @writeAttributeMicrophoneMutedWithValue:expectedValueInterval:@
writeAttributeMicrophoneMutedWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMicrophoneMutedWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMicrophoneMutedWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMicrophoneMutedWithValue:expectedValueInterval:params:@
writeAttributeMicrophoneMutedWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMicrophoneMutedWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMicrophoneMutedWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMicrophoneVolumeLevelWithParams:@
readAttributeMicrophoneVolumeLevelWithParamsSelector :: Selector
readAttributeMicrophoneVolumeLevelWithParamsSelector = mkSelector "readAttributeMicrophoneVolumeLevelWithParams:"

-- | @Selector@ for @writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:@
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:params:@
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMicrophoneVolumeLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMicrophoneVolumeLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMicrophoneMaxLevelWithParams:@
readAttributeMicrophoneMaxLevelWithParamsSelector :: Selector
readAttributeMicrophoneMaxLevelWithParamsSelector = mkSelector "readAttributeMicrophoneMaxLevelWithParams:"

-- | @Selector@ for @readAttributeMicrophoneMinLevelWithParams:@
readAttributeMicrophoneMinLevelWithParamsSelector :: Selector
readAttributeMicrophoneMinLevelWithParamsSelector = mkSelector "readAttributeMicrophoneMinLevelWithParams:"

-- | @Selector@ for @readAttributeMicrophoneAGCEnabledWithParams:@
readAttributeMicrophoneAGCEnabledWithParamsSelector :: Selector
readAttributeMicrophoneAGCEnabledWithParamsSelector = mkSelector "readAttributeMicrophoneAGCEnabledWithParams:"

-- | @Selector@ for @writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:@
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:params:@
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeMicrophoneAGCEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMicrophoneAGCEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeImageRotationWithParams:@
readAttributeImageRotationWithParamsSelector :: Selector
readAttributeImageRotationWithParamsSelector = mkSelector "readAttributeImageRotationWithParams:"

-- | @Selector@ for @writeAttributeImageRotationWithValue:expectedValueInterval:@
writeAttributeImageRotationWithValue_expectedValueIntervalSelector :: Selector
writeAttributeImageRotationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeImageRotationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeImageRotationWithValue:expectedValueInterval:params:@
writeAttributeImageRotationWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeImageRotationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeImageRotationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeImageFlipHorizontalWithParams:@
readAttributeImageFlipHorizontalWithParamsSelector :: Selector
readAttributeImageFlipHorizontalWithParamsSelector = mkSelector "readAttributeImageFlipHorizontalWithParams:"

-- | @Selector@ for @writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:@
writeAttributeImageFlipHorizontalWithValue_expectedValueIntervalSelector :: Selector
writeAttributeImageFlipHorizontalWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:params:@
writeAttributeImageFlipHorizontalWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeImageFlipHorizontalWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeImageFlipHorizontalWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeImageFlipVerticalWithParams:@
readAttributeImageFlipVerticalWithParamsSelector :: Selector
readAttributeImageFlipVerticalWithParamsSelector = mkSelector "readAttributeImageFlipVerticalWithParams:"

-- | @Selector@ for @writeAttributeImageFlipVerticalWithValue:expectedValueInterval:@
writeAttributeImageFlipVerticalWithValue_expectedValueIntervalSelector :: Selector
writeAttributeImageFlipVerticalWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeImageFlipVerticalWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeImageFlipVerticalWithValue:expectedValueInterval:params:@
writeAttributeImageFlipVerticalWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeImageFlipVerticalWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeImageFlipVerticalWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLocalVideoRecordingEnabledWithParams:@
readAttributeLocalVideoRecordingEnabledWithParamsSelector :: Selector
readAttributeLocalVideoRecordingEnabledWithParamsSelector = mkSelector "readAttributeLocalVideoRecordingEnabledWithParams:"

-- | @Selector@ for @writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:@
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:params:@
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocalVideoRecordingEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalVideoRecordingEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLocalSnapshotRecordingEnabledWithParams:@
readAttributeLocalSnapshotRecordingEnabledWithParamsSelector :: Selector
readAttributeLocalSnapshotRecordingEnabledWithParamsSelector = mkSelector "readAttributeLocalSnapshotRecordingEnabledWithParams:"

-- | @Selector@ for @writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:@
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:params:@
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeLocalSnapshotRecordingEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalSnapshotRecordingEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStatusLightEnabledWithParams:@
readAttributeStatusLightEnabledWithParamsSelector :: Selector
readAttributeStatusLightEnabledWithParamsSelector = mkSelector "readAttributeStatusLightEnabledWithParams:"

-- | @Selector@ for @writeAttributeStatusLightEnabledWithValue:expectedValueInterval:@
writeAttributeStatusLightEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeStatusLightEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStatusLightEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStatusLightEnabledWithValue:expectedValueInterval:params:@
writeAttributeStatusLightEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeStatusLightEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStatusLightEnabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStatusLightBrightnessWithParams:@
readAttributeStatusLightBrightnessWithParamsSelector :: Selector
readAttributeStatusLightBrightnessWithParamsSelector = mkSelector "readAttributeStatusLightBrightnessWithParams:"

-- | @Selector@ for @writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:@
writeAttributeStatusLightBrightnessWithValue_expectedValueIntervalSelector :: Selector
writeAttributeStatusLightBrightnessWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:params:@
writeAttributeStatusLightBrightnessWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeStatusLightBrightnessWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStatusLightBrightnessWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

