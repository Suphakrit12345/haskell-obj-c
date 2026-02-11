{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information associated with an AVAsset for a cinematic video.
--
-- Generated bindings for @CNAssetInfo@.
module ObjC.Cinematic.CNAssetInfo
  ( CNAssetInfo
  , IsCNAssetInfo(..)
  , checkIfCinematic_completionHandler
  , loadFromAsset_completionHandler
  , init_
  , new
  , asset
  , allCinematicTracks
  , cinematicVideoTrack
  , cinematicDisparityTrack
  , cinematicMetadataTrack
  , frameTimingTrack
  , videoCompositionTracks
  , videoCompositionTrackIDs
  , sampleDataTrackIDs
  , checkIfCinematic_completionHandlerSelector
  , loadFromAsset_completionHandlerSelector
  , initSelector
  , newSelector
  , assetSelector
  , allCinematicTracksSelector
  , cinematicVideoTrackSelector
  , cinematicDisparityTrackSelector
  , cinematicMetadataTrackSelector
  , frameTimingTrackSelector
  , videoCompositionTracksSelector
  , videoCompositionTrackIDsSelector
  , sampleDataTrackIDsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Check if asset is cinematic asynchronously.
--
-- ObjC selector: @+ checkIfCinematic:completionHandler:@
checkIfCinematic_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
checkIfCinematic_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNAssetInfo"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "checkIfCinematic:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Load cinematic asset information asynchronously.
--
-- ObjC selector: @+ loadFromAsset:completionHandler:@
loadFromAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
loadFromAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNAssetInfo"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "loadFromAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- init@
init_ :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id CNAssetInfo)
init_ cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNAssetInfo)
new  =
  do
    cls' <- getRequiredClass "CNAssetInfo"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- asset@
asset :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAsset)
asset cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "asset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allCinematicTracks@
allCinematicTracks :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
allCinematicTracks cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "allCinematicTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cinematicVideoTrack@
cinematicVideoTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
cinematicVideoTrack cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "cinematicVideoTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cinematicDisparityTrack@
cinematicDisparityTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
cinematicDisparityTrack cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "cinematicDisparityTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cinematicMetadataTrack@
cinematicMetadataTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
cinematicMetadataTrack cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "cinematicMetadataTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Track to be used for frame timing
--
-- ObjC selector: @- frameTimingTrack@
frameTimingTrack :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id AVAssetTrack)
frameTimingTrack cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "frameTimingTrack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Tracks required to construct AVAssetReaderVideoCompositionOutput.
--
-- ObjC selector: @- videoCompositionTracks@
videoCompositionTracks :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
videoCompositionTracks cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "videoCompositionTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Source video track IDs required to implement AVVideoCompositionInstruction protocol
--
-- ObjC selector: @- videoCompositionTrackIDs@
videoCompositionTrackIDs :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
videoCompositionTrackIDs cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "videoCompositionTrackIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Source metadata track IDs required to implement AVVideoCompositionInstruction protocol
--
-- ObjC selector: @- sampleDataTrackIDs@
sampleDataTrackIDs :: IsCNAssetInfo cnAssetInfo => cnAssetInfo -> IO (Id NSArray)
sampleDataTrackIDs cnAssetInfo  =
    sendMsg cnAssetInfo (mkSelector "sampleDataTrackIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkIfCinematic:completionHandler:@
checkIfCinematic_completionHandlerSelector :: Selector
checkIfCinematic_completionHandlerSelector = mkSelector "checkIfCinematic:completionHandler:"

-- | @Selector@ for @loadFromAsset:completionHandler:@
loadFromAsset_completionHandlerSelector :: Selector
loadFromAsset_completionHandlerSelector = mkSelector "loadFromAsset:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @asset@
assetSelector :: Selector
assetSelector = mkSelector "asset"

-- | @Selector@ for @allCinematicTracks@
allCinematicTracksSelector :: Selector
allCinematicTracksSelector = mkSelector "allCinematicTracks"

-- | @Selector@ for @cinematicVideoTrack@
cinematicVideoTrackSelector :: Selector
cinematicVideoTrackSelector = mkSelector "cinematicVideoTrack"

-- | @Selector@ for @cinematicDisparityTrack@
cinematicDisparityTrackSelector :: Selector
cinematicDisparityTrackSelector = mkSelector "cinematicDisparityTrack"

-- | @Selector@ for @cinematicMetadataTrack@
cinematicMetadataTrackSelector :: Selector
cinematicMetadataTrackSelector = mkSelector "cinematicMetadataTrack"

-- | @Selector@ for @frameTimingTrack@
frameTimingTrackSelector :: Selector
frameTimingTrackSelector = mkSelector "frameTimingTrack"

-- | @Selector@ for @videoCompositionTracks@
videoCompositionTracksSelector :: Selector
videoCompositionTracksSelector = mkSelector "videoCompositionTracks"

-- | @Selector@ for @videoCompositionTrackIDs@
videoCompositionTrackIDsSelector :: Selector
videoCompositionTrackIDsSelector = mkSelector "videoCompositionTrackIDs"

-- | @Selector@ for @sampleDataTrackIDs@
sampleDataTrackIDsSelector :: Selector
sampleDataTrackIDsSelector = mkSelector "sampleDataTrackIDs"

