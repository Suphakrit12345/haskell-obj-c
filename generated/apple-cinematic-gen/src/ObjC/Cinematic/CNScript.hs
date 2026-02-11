{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Database of focus decisions with methods to change them. Knows what has been detected in each frame and which detection is being focused on. All operations are executed in a thread-safe manner, but that also means that a long-running update can stall a lookup. Best practice is to lookup what you need up front (outside your critical code) and pass the immutable results to where it's needed. That way, you're not blocked when you access the information, say inside the rendering portion of your code.
--
-- Generated bindings for @CNScript@.
module ObjC.Cinematic.CNScript
  ( CNScript
  , IsCNScript(..)
  , loadFromAsset_changes_progress_completionHandler
  , reloadWithChanges
  , changes
  , detectionTrackForID
  , detectionTrackForDecision
  , addUserDecision
  , removeUserDecision
  , removeAllUserDecisions
  , addDetectionTrack
  , removeDetectionTrack
  , init_
  , new
  , fNumber
  , setFNumber
  , addedDetectionTracks
  , loadFromAsset_changes_progress_completionHandlerSelector
  , reloadWithChangesSelector
  , changesSelector
  , detectionTrackForIDSelector
  , detectionTrackForDecisionSelector
  , addUserDecisionSelector
  , removeUserDecisionSelector
  , removeAllUserDecisionsSelector
  , addDetectionTrackSelector
  , removeDetectionTrackSelector
  , initSelector
  , newSelector
  , fNumberSelector
  , setFNumberSelector
  , addedDetectionTracksSelector


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
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load cinematic script asynchronously from a cinematic asset. - Parameters:   - asset: the cinematic asset to be loaded.   - changes: optional changes since asset was recorded. Can be obtained from a previous editing session. If @nil@, the asset is loaded as originally recorded.   - progress: optional progress object to track progress or cancel loading. Represents just the loading of this asset. Create with desired total unit count or use zero to have the unit count filled in automatically.  If @nil@, no progress is reported.   - completionHandler: called with the loaded cinematic script when done, or with with an error if it fails. If progress is canceled before it completes, the completion handler is called with an error.
--
-- ObjC selector: @+ loadFromAsset:changes:progress:completionHandler:@
loadFromAsset_changes_progress_completionHandler :: (IsAVAsset asset, IsCNScriptChanges changes, IsNSProgress progress) => asset -> changes -> progress -> Ptr () -> IO ()
loadFromAsset_changes_progress_completionHandler asset changes progress completionHandler =
  do
    cls' <- getRequiredClass "CNScript"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr changes $ \raw_changes ->
        withObjCPtr progress $ \raw_progress ->
          sendClassMsg cls' (mkSelector "loadFromAsset:changes:progress:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_changes :: Ptr ()), argPtr (castPtr raw_progress :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Reload the cinematic script with optional changes applied, removing any previous changes made. This can be more efficient than loading the asset from scratch. - Parameters:   - changes: optional changes since asset was recorded. Can be obtained from a previous editing session. If @nil@, the asset is reloaded as originally recorded.
--
-- ObjC selector: @- reloadWithChanges:@
reloadWithChanges :: (IsCNScript cnScript, IsCNScriptChanges changes) => cnScript -> changes -> IO ()
reloadWithChanges cnScript  changes =
  withObjCPtr changes $ \raw_changes ->
      sendMsg cnScript (mkSelector "reloadWithChanges:") retVoid [argPtr (castPtr raw_changes :: Ptr ())]

-- | Changes made since cinematic asset was recorded. Can be used to checkpoint and later restore changes made so far.
--
-- ObjC selector: @- changes@
changes :: IsCNScript cnScript => cnScript -> IO (Id CNScriptChanges)
changes cnScript  =
    sendMsg cnScript (mkSelector "changes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A detection track representing all detections with the given detectionID over the entire cinematic script.
--
-- ObjC selector: @- detectionTrackForID:@
detectionTrackForID :: IsCNScript cnScript => cnScript -> CLong -> IO (Id CNDetectionTrack)
detectionTrackForID cnScript  detectionID =
    sendMsg cnScript (mkSelector "detectionTrackForID:") (retPtr retVoid) [argCLong detectionID] >>= retainedObject . castPtr

-- | A detection track representing all detections that would be chosen by a given decision.
--
-- ObjC selector: @- detectionTrackForDecision:@
detectionTrackForDecision :: (IsCNScript cnScript, IsCNDecision decision) => cnScript -> decision -> IO (Id CNDetectionTrack)
detectionTrackForDecision cnScript  decision =
  withObjCPtr decision $ \raw_decision ->
      sendMsg cnScript (mkSelector "detectionTrackForDecision:") (retPtr retVoid) [argPtr (castPtr raw_decision :: Ptr ())] >>= retainedObject . castPtr

-- | Add a new user decision. Replaces an existing user decision if the times are identical.
--
-- Adding a decision can fail if the decision focuses on an detection or group that does not exist or if its time is not within the time range of the cinematic script.
--
-- - Returns: whether adding was successful
--
-- ObjC selector: @- addUserDecision:@
addUserDecision :: (IsCNScript cnScript, IsCNDecision decision) => cnScript -> decision -> IO Bool
addUserDecision cnScript  decision =
  withObjCPtr decision $ \raw_decision ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnScript (mkSelector "addUserDecision:") retCULong [argPtr (castPtr raw_decision :: Ptr ())]

-- | Remove an existing user decision.
--
-- User decisions added to the script or those made at recording time (by tapping during recording) can be removed. Decisions that are not user decisions cannot be removed.
--
-- - Returns: whether removal was successful
--
-- ObjC selector: @- removeUserDecision:@
removeUserDecision :: (IsCNScript cnScript, IsCNDecision decision) => cnScript -> decision -> IO Bool
removeUserDecision cnScript  decision =
  withObjCPtr decision $ \raw_decision ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnScript (mkSelector "removeUserDecision:") retCULong [argPtr (castPtr raw_decision :: Ptr ())]

-- | Remove all user decisions and revert to base decisions only.
--
-- ObjC selector: @- removeAllUserDecisions@
removeAllUserDecisions :: IsCNScript cnScript => cnScript -> IO ()
removeAllUserDecisions cnScript  =
    sendMsg cnScript (mkSelector "removeAllUserDecisions") retVoid []

-- | Add user created detection track.
--
-- - Returns: the detectionID assigned to the added track, which can be used for later lookup or decision creation.
--
-- ObjC selector: @- addDetectionTrack:@
addDetectionTrack :: (IsCNScript cnScript, IsCNDetectionTrack detectionTrack) => cnScript -> detectionTrack -> IO CLong
addDetectionTrack cnScript  detectionTrack =
  withObjCPtr detectionTrack $ \raw_detectionTrack ->
      sendMsg cnScript (mkSelector "addDetectionTrack:") retCLong [argPtr (castPtr raw_detectionTrack :: Ptr ())]

-- | Remove user created detection track.
--
-- Tracks created at recording time cannot be removed.
--
-- - Returns: whether removal was successful
--
-- ObjC selector: @- removeDetectionTrack:@
removeDetectionTrack :: (IsCNScript cnScript, IsCNDetectionTrack detectionTrack) => cnScript -> detectionTrack -> IO Bool
removeDetectionTrack cnScript  detectionTrack =
  withObjCPtr detectionTrack $ \raw_detectionTrack ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnScript (mkSelector "removeDetectionTrack:") retCULong [argPtr (castPtr raw_detectionTrack :: Ptr ())]

-- | @- init@
init_ :: IsCNScript cnScript => cnScript -> IO (Id CNScript)
init_ cnScript  =
    sendMsg cnScript (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNScript)
new  =
  do
    cls' <- getRequiredClass "CNScript"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The f/number to apply to the entire movie, initially set to that of the recorded movie.
--
-- Pass this to the rendering session in the rendering frame attributes to match the selected aperture. Change this property when the user selects a different aperture for the edited movie. Changes to this property are reflected in the script changes for later restoration.
--
-- ObjC selector: @- fNumber@
fNumber :: IsCNScript cnScript => cnScript -> IO CFloat
fNumber cnScript  =
    sendMsg cnScript (mkSelector "fNumber") retCFloat []

-- | The f/number to apply to the entire movie, initially set to that of the recorded movie.
--
-- Pass this to the rendering session in the rendering frame attributes to match the selected aperture. Change this property when the user selects a different aperture for the edited movie. Changes to this property are reflected in the script changes for later restoration.
--
-- ObjC selector: @- setFNumber:@
setFNumber :: IsCNScript cnScript => cnScript -> CFloat -> IO ()
setFNumber cnScript  value =
    sendMsg cnScript (mkSelector "setFNumber:") retVoid [argCFloat value]

-- | All detection tracks that have been added since recording.
--
-- ObjC selector: @- addedDetectionTracks@
addedDetectionTracks :: IsCNScript cnScript => cnScript -> IO (Id NSArray)
addedDetectionTracks cnScript  =
    sendMsg cnScript (mkSelector "addedDetectionTracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromAsset:changes:progress:completionHandler:@
loadFromAsset_changes_progress_completionHandlerSelector :: Selector
loadFromAsset_changes_progress_completionHandlerSelector = mkSelector "loadFromAsset:changes:progress:completionHandler:"

-- | @Selector@ for @reloadWithChanges:@
reloadWithChangesSelector :: Selector
reloadWithChangesSelector = mkSelector "reloadWithChanges:"

-- | @Selector@ for @changes@
changesSelector :: Selector
changesSelector = mkSelector "changes"

-- | @Selector@ for @detectionTrackForID:@
detectionTrackForIDSelector :: Selector
detectionTrackForIDSelector = mkSelector "detectionTrackForID:"

-- | @Selector@ for @detectionTrackForDecision:@
detectionTrackForDecisionSelector :: Selector
detectionTrackForDecisionSelector = mkSelector "detectionTrackForDecision:"

-- | @Selector@ for @addUserDecision:@
addUserDecisionSelector :: Selector
addUserDecisionSelector = mkSelector "addUserDecision:"

-- | @Selector@ for @removeUserDecision:@
removeUserDecisionSelector :: Selector
removeUserDecisionSelector = mkSelector "removeUserDecision:"

-- | @Selector@ for @removeAllUserDecisions@
removeAllUserDecisionsSelector :: Selector
removeAllUserDecisionsSelector = mkSelector "removeAllUserDecisions"

-- | @Selector@ for @addDetectionTrack:@
addDetectionTrackSelector :: Selector
addDetectionTrackSelector = mkSelector "addDetectionTrack:"

-- | @Selector@ for @removeDetectionTrack:@
removeDetectionTrackSelector :: Selector
removeDetectionTrackSelector = mkSelector "removeDetectionTrack:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @setFNumber:@
setFNumberSelector :: Selector
setFNumberSelector = mkSelector "setFNumber:"

-- | @Selector@ for @addedDetectionTracks@
addedDetectionTracksSelector :: Selector
addedDetectionTracksSelector = mkSelector "addedDetectionTracks"

