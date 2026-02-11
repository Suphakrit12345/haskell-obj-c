{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CNAssetSpatialAudioInfo
--
-- A helper class to inspect recordings made when Spatial Audio setting is turned on. An instance of this class contains the default audio track with Spatial Audio, metadata read from the file that can be applied	during to enhance the playback experience. This class also provides tunable parameters to change the intensity & mode of the playback experience.
--
-- The goal of this class is to assist users operate on assets in which audio has been captured in multiple formats like Spatial Audio and Stereo to allow more audio customization.	Users can audition playback of this asset with an immersive audio rendering effect applied by fetching an AVAudioMix containing the necessary metadata serialized in the file as well as any user supplied changes.	Once the results of the audition are satisfactory, clients can create a copy of the asset with the audio effect burned in.
--
-- Generated bindings for @CNAssetSpatialAudioInfo@.
module ObjC.Cinematic.CNAssetSpatialAudioInfo
  ( CNAssetSpatialAudioInfo
  , IsCNAssetSpatialAudioInfo(..)
  , init_
  , new
  , checkIfContainsSpatialAudio_completionHandler
  , loadFromAsset_completionHandler
  , audioMixWithEffectIntensity_renderingStyle
  , assetReaderOutputSettingsForContentType
  , assetWriterInputSettingsForContentType
  , isSupported
  , defaultSpatialAudioTrack
  , defaultEffectIntensity
  , defaultRenderingStyle
  , spatialAudioMixMetadata
  , initSelector
  , newSelector
  , checkIfContainsSpatialAudio_completionHandlerSelector
  , loadFromAsset_completionHandlerSelector
  , audioMixWithEffectIntensity_renderingStyleSelector
  , assetReaderOutputSettingsForContentTypeSelector
  , assetWriterInputSettingsForContentTypeSelector
  , isSupportedSelector
  , defaultSpatialAudioTrackSelector
  , defaultEffectIntensitySelector
  , defaultRenderingStyleSelector
  , spatialAudioMixMetadataSelector

  -- * Enum types
  , CNSpatialAudioContentType(CNSpatialAudioContentType)
  , pattern CNSpatialAudioContentTypeStereo
  , pattern CNSpatialAudioContentTypeSpatial
  , CNSpatialAudioRenderingStyle(CNSpatialAudioRenderingStyle)
  , pattern CNSpatialAudioRenderingStyleCinematic
  , pattern CNSpatialAudioRenderingStyleStudio
  , pattern CNSpatialAudioRenderingStyleInFrame
  , pattern CNSpatialAudioRenderingStyleCinematicBackgroundStem
  , pattern CNSpatialAudioRenderingStyleCinematicForegroundStem
  , pattern CNSpatialAudioRenderingStyleStudioForegroundStem
  , pattern CNSpatialAudioRenderingStyleInFrameForegroundStem
  , pattern CNSpatialAudioRenderingStyleStandard
  , pattern CNSpatialAudioRenderingStyleStudioBackgroundStem
  , pattern CNSpatialAudioRenderingStyleInFrameBackgroundStem

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

import ObjC.Cinematic.Internal.Classes
import ObjC.Cinematic.Internal.Enums
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> IO (Id CNAssetSpatialAudioInfo)
init_ cnAssetSpatialAudioInfo  =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNAssetSpatialAudioInfo)
new  =
  do
    cls' <- getRequiredClass "CNAssetSpatialAudioInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | checkIfContainsSpatialAudio:
--
-- Check if asset meets all the requirements to operate with Spatial Audio and its accompanying effects
--
-- @asset@ — An instance of AVAsset.
--
-- @completionHandler@ — Completion handler to return the result
--
-- Returns: Boolean
--
-- ObjC selector: @+ checkIfContainsSpatialAudio:completionHandler:@
checkIfContainsSpatialAudio_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
checkIfContainsSpatialAudio_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNAssetSpatialAudioInfo"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "checkIfContainsSpatialAudio:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | loadFromAsset:
--
-- Returns an instance of CNAssetAudioInfo for an AVAsset object asynchronously.
--
-- @asset@ — An instance of AVAsset
--
-- @completionHandler@ — Completion handler to return the result
--
-- Returns: An instance of CNAssetSpatialAudioInfo delivered via the completion handler or an error on failure
--
-- ObjC selector: @+ loadFromAsset:completionHandler:@
loadFromAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
loadFromAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNAssetSpatialAudioInfo"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "loadFromAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | audioMixWithEffectIntensity:renderingStyle:
--
-- returns an instance of @AVAudioMix@ encapsulating all spatial audio related data with specified effect intensity and rendering style.
--
-- Returns an @AVAudioMix@ containing all the necessary state to operate on the asset with Spatial Audio effects enabled
--
-- ObjC selector: @- audioMixWithEffectIntensity:renderingStyle:@
audioMixWithEffectIntensity_renderingStyle :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> CFloat -> CNSpatialAudioRenderingStyle -> IO (Id AVAudioMix)
audioMixWithEffectIntensity_renderingStyle cnAssetSpatialAudioInfo  effectIntensity renderingStyle =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "audioMixWithEffectIntensity:renderingStyle:") (retPtr retVoid) [argCFloat effectIntensity, argCLong (coerce renderingStyle)] >>= retainedObject . castPtr

-- | assetReaderOutputSettingsForContentType
--
-- Returns a dictionary of settings and the source track that should be used to fetch LPCM samples from this track with the effect applied
--
-- Use the returned NSDictionary with the @defaulSpatialAudioTrack@ to initialize an instance of @AVAssetReaderAudioMixOutput@
--
-- ObjC selector: @- assetReaderOutputSettingsForContentType:@
assetReaderOutputSettingsForContentType :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> CNSpatialAudioContentType -> IO (Id NSDictionary)
assetReaderOutputSettingsForContentType cnAssetSpatialAudioInfo  contentType =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "assetReaderOutputSettingsForContentType:") (retPtr retVoid) [argCLong (coerce contentType)] >>= retainedObject . castPtr

-- | assetWriterInputSettingsForContentType
--
-- Returns a dictionary of settings that should be used to encode LPCM samples using @AVAssetWriterInput@
--
-- ObjC selector: @- assetWriterInputSettingsForContentType:@
assetWriterInputSettingsForContentType :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> CNSpatialAudioContentType -> IO (Id NSDictionary)
assetWriterInputSettingsForContentType cnAssetSpatialAudioInfo  contentType =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "assetWriterInputSettingsForContentType:") (retPtr retVoid) [argCLong (coerce contentType)] >>= retainedObject . castPtr

-- | isSupported
--
-- Indicates whether the current device supports Audio Mix.
--
-- ObjC selector: @+ isSupported@
isSupported :: IO Bool
isSupported  =
  do
    cls' <- getRequiredClass "CNAssetSpatialAudioInfo"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isSupported") retCULong []

-- | defaulSpatialAudioTrack
--
-- default @AVAssetTrack@ containing Spatial Audio
--
-- ObjC selector: @- defaultSpatialAudioTrack@
defaultSpatialAudioTrack :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> IO (Id AVAssetTrack)
defaultSpatialAudioTrack cnAssetSpatialAudioInfo  =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "defaultSpatialAudioTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | defaultEffectIntensity
--
-- default effect intensity value as provided by the system. Supported range is [0.0-1.0]
--
-- ObjC selector: @- defaultEffectIntensity@
defaultEffectIntensity :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> IO CFloat
defaultEffectIntensity cnAssetSpatialAudioInfo  =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "defaultEffectIntensity") retCFloat []

-- | defaultRenderingStyle
--
-- default rendering style as provided by the system
--
-- ObjC selector: @- defaultRenderingStyle@
defaultRenderingStyle :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> IO CNSpatialAudioRenderingStyle
defaultRenderingStyle cnAssetSpatialAudioInfo  =
    fmap (coerce :: CLong -> CNSpatialAudioRenderingStyle) $ sendMsg cnAssetSpatialAudioInfo (mkSelector "defaultRenderingStyle") retCLong []

-- | spatialAudioMixMetadata
--
-- The result of audio analysis during recording which contains metadata necessary to properly configure the Audio Mix feature during playback or editing..				Can be used with @AUAudioUnit@ instances that support AudioUnitPropertyID @kProperty_SpatialAudioMixMetadata@
--
-- ObjC selector: @- spatialAudioMixMetadata@
spatialAudioMixMetadata :: IsCNAssetSpatialAudioInfo cnAssetSpatialAudioInfo => cnAssetSpatialAudioInfo -> IO (Id NSData)
spatialAudioMixMetadata cnAssetSpatialAudioInfo  =
    sendMsg cnAssetSpatialAudioInfo (mkSelector "spatialAudioMixMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @checkIfContainsSpatialAudio:completionHandler:@
checkIfContainsSpatialAudio_completionHandlerSelector :: Selector
checkIfContainsSpatialAudio_completionHandlerSelector = mkSelector "checkIfContainsSpatialAudio:completionHandler:"

-- | @Selector@ for @loadFromAsset:completionHandler:@
loadFromAsset_completionHandlerSelector :: Selector
loadFromAsset_completionHandlerSelector = mkSelector "loadFromAsset:completionHandler:"

-- | @Selector@ for @audioMixWithEffectIntensity:renderingStyle:@
audioMixWithEffectIntensity_renderingStyleSelector :: Selector
audioMixWithEffectIntensity_renderingStyleSelector = mkSelector "audioMixWithEffectIntensity:renderingStyle:"

-- | @Selector@ for @assetReaderOutputSettingsForContentType:@
assetReaderOutputSettingsForContentTypeSelector :: Selector
assetReaderOutputSettingsForContentTypeSelector = mkSelector "assetReaderOutputSettingsForContentType:"

-- | @Selector@ for @assetWriterInputSettingsForContentType:@
assetWriterInputSettingsForContentTypeSelector :: Selector
assetWriterInputSettingsForContentTypeSelector = mkSelector "assetWriterInputSettingsForContentType:"

-- | @Selector@ for @isSupported@
isSupportedSelector :: Selector
isSupportedSelector = mkSelector "isSupported"

-- | @Selector@ for @defaultSpatialAudioTrack@
defaultSpatialAudioTrackSelector :: Selector
defaultSpatialAudioTrackSelector = mkSelector "defaultSpatialAudioTrack"

-- | @Selector@ for @defaultEffectIntensity@
defaultEffectIntensitySelector :: Selector
defaultEffectIntensitySelector = mkSelector "defaultEffectIntensity"

-- | @Selector@ for @defaultRenderingStyle@
defaultRenderingStyleSelector :: Selector
defaultRenderingStyleSelector = mkSelector "defaultRenderingStyle"

-- | @Selector@ for @spatialAudioMixMetadata@
spatialAudioMixMetadataSelector :: Selector
spatialAudioMixMetadataSelector = mkSelector "spatialAudioMixMetadata"

