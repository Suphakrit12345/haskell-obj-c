{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Cinematic.Internal.Classes (
    module ObjC.Cinematic.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- CNAssetInfo ----------

-- | Information associated with an AVAsset for a cinematic video.
-- 
-- Phantom type for @CNAssetInfo@.
data CNAssetInfo

instance IsObjCObject (Id CNAssetInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNAssetInfo"

class IsNSObject a => IsCNAssetInfo a where
  toCNAssetInfo :: a -> Id CNAssetInfo

instance IsCNAssetInfo (Id CNAssetInfo) where
  toCNAssetInfo = unsafeCastId

instance IsNSObject (Id CNAssetInfo) where
  toNSObject = unsafeCastId

-- ---------- CNAssetSpatialAudioInfo ----------

-- | CNAssetSpatialAudioInfo
--
-- A helper class to inspect recordings made when Spatial Audio setting is turned on. An instance of this class contains the default audio track with Spatial Audio, metadata read from the file that can be applied	during to enhance the playback experience. This class also provides tunable parameters to change the intensity & mode of the playback experience.
--
-- The goal of this class is to assist users operate on assets in which audio has been captured in multiple formats like Spatial Audio and Stereo to allow more audio customization.	Users can audition playback of this asset with an immersive audio rendering effect applied by fetching an AVAudioMix containing the necessary metadata serialized in the file as well as any user supplied changes.	Once the results of the audition are satisfactory, clients can create a copy of the asset with the audio effect burned in.
-- 
-- Phantom type for @CNAssetSpatialAudioInfo@.
data CNAssetSpatialAudioInfo

instance IsObjCObject (Id CNAssetSpatialAudioInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNAssetSpatialAudioInfo"

class IsNSObject a => IsCNAssetSpatialAudioInfo a where
  toCNAssetSpatialAudioInfo :: a -> Id CNAssetSpatialAudioInfo

instance IsCNAssetSpatialAudioInfo (Id CNAssetSpatialAudioInfo) where
  toCNAssetSpatialAudioInfo = unsafeCastId

instance IsNSObject (Id CNAssetSpatialAudioInfo) where
  toNSObject = unsafeCastId

-- ---------- CNBoundsPrediction ----------

-- | Phantom type for @CNBoundsPrediction@.
data CNBoundsPrediction

instance IsObjCObject (Id CNBoundsPrediction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNBoundsPrediction"

class IsNSObject a => IsCNBoundsPrediction a where
  toCNBoundsPrediction :: a -> Id CNBoundsPrediction

instance IsCNBoundsPrediction (Id CNBoundsPrediction) where
  toCNBoundsPrediction = unsafeCastId

instance IsNSObject (Id CNBoundsPrediction) where
  toNSObject = unsafeCastId

-- ---------- CNDecision ----------

-- | Represents a decision to focus on a specific detectionID or detectionGroupID; optionally strong.
--
-- A strong decision keeps focus for as long as possible.
-- 
-- Phantom type for @CNDecision@.
data CNDecision

instance IsObjCObject (Id CNDecision) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNDecision"

class IsNSObject a => IsCNDecision a where
  toCNDecision :: a -> Id CNDecision

instance IsCNDecision (Id CNDecision) where
  toCNDecision = unsafeCastId

instance IsNSObject (Id CNDecision) where
  toNSObject = unsafeCastId

-- ---------- CNDetection ----------

-- | A cinematic detection of a subject.
--
-- Specifies the type, distance (as disparity), bounds (as a normalized rectangle), and time (as CMTime) of the detection. Detections obtained from the cinematic script include a detectionID that can be used to track the detection over time. Some types of detections also include a detectionGroupID that associates related detections (e.g. the face and torso of the same person).
-- 
-- Phantom type for @CNDetection@.
data CNDetection

instance IsObjCObject (Id CNDetection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNDetection"

class IsNSObject a => IsCNDetection a where
  toCNDetection :: a -> Id CNDetection

instance IsCNDetection (Id CNDetection) where
  toCNDetection = unsafeCastId

instance IsNSObject (Id CNDetection) where
  toNSObject = unsafeCastId

-- ---------- CNDetectionTrack ----------

-- | Abstract class representing a series of detections of the same subject over time.
-- 
-- Phantom type for @CNDetectionTrack@.
data CNDetectionTrack

instance IsObjCObject (Id CNDetectionTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNDetectionTrack"

class IsNSObject a => IsCNDetectionTrack a where
  toCNDetectionTrack :: a -> Id CNDetectionTrack

instance IsCNDetectionTrack (Id CNDetectionTrack) where
  toCNDetectionTrack = unsafeCastId

instance IsNSObject (Id CNDetectionTrack) where
  toNSObject = unsafeCastId

-- ---------- CNObjectTracker ----------

-- | Converts a normalized point or rectangle into a detection track that tracks an object over time.
-- 
-- Phantom type for @CNObjectTracker@.
data CNObjectTracker

instance IsObjCObject (Id CNObjectTracker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNObjectTracker"

class IsNSObject a => IsCNObjectTracker a where
  toCNObjectTracker :: a -> Id CNObjectTracker

instance IsCNObjectTracker (Id CNObjectTracker) where
  toCNObjectTracker = unsafeCastId

instance IsNSObject (Id CNObjectTracker) where
  toNSObject = unsafeCastId

-- ---------- CNRenderingSession ----------

-- | Phantom type for @CNRenderingSession@.
data CNRenderingSession

instance IsObjCObject (Id CNRenderingSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNRenderingSession"

class IsNSObject a => IsCNRenderingSession a where
  toCNRenderingSession :: a -> Id CNRenderingSession

instance IsCNRenderingSession (Id CNRenderingSession) where
  toCNRenderingSession = unsafeCastId

instance IsNSObject (Id CNRenderingSession) where
  toNSObject = unsafeCastId

-- ---------- CNRenderingSessionAttributes ----------

-- | Movie-wide information required by the rendering session.
-- 
-- Phantom type for @CNRenderingSessionAttributes@.
data CNRenderingSessionAttributes

instance IsObjCObject (Id CNRenderingSessionAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNRenderingSessionAttributes"

class IsNSObject a => IsCNRenderingSessionAttributes a where
  toCNRenderingSessionAttributes :: a -> Id CNRenderingSessionAttributes

instance IsCNRenderingSessionAttributes (Id CNRenderingSessionAttributes) where
  toCNRenderingSessionAttributes = unsafeCastId

instance IsNSObject (Id CNRenderingSessionAttributes) where
  toNSObject = unsafeCastId

-- ---------- CNRenderingSessionFrameAttributes ----------

-- | Frame-specific information required to render a frame in a rendering session.
-- 
-- Phantom type for @CNRenderingSessionFrameAttributes@.
data CNRenderingSessionFrameAttributes

instance IsObjCObject (Id CNRenderingSessionFrameAttributes) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNRenderingSessionFrameAttributes"

class IsNSObject a => IsCNRenderingSessionFrameAttributes a where
  toCNRenderingSessionFrameAttributes :: a -> Id CNRenderingSessionFrameAttributes

instance IsCNRenderingSessionFrameAttributes (Id CNRenderingSessionFrameAttributes) where
  toCNRenderingSessionFrameAttributes = unsafeCastId

instance IsNSObject (Id CNRenderingSessionFrameAttributes) where
  toNSObject = unsafeCastId

-- ---------- CNScript ----------

-- | Database of focus decisions with methods to change them. Knows what has been detected in each frame and which detection is being focused on. All operations are executed in a thread-safe manner, but that also means that a long-running update can stall a lookup. Best practice is to lookup what you need up front (outside your critical code) and pass the immutable results to where it's needed. That way, you're not blocked when you access the information, say inside the rendering portion of your code.
-- 
-- Phantom type for @CNScript@.
data CNScript

instance IsObjCObject (Id CNScript) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNScript"

class IsNSObject a => IsCNScript a where
  toCNScript :: a -> Id CNScript

instance IsCNScript (Id CNScript) where
  toCNScript = unsafeCastId

instance IsNSObject (Id CNScript) where
  toNSObject = unsafeCastId

-- ---------- CNScriptChanges ----------

-- | Represents a snapshot of changes made to the cinematic script since recording. Can be used as a snapshot to quickly revert to previously saved edits via @-[CNScript reloadWithChanges:]@
-- 
-- Phantom type for @CNScriptChanges@.
data CNScriptChanges

instance IsObjCObject (Id CNScriptChanges) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNScriptChanges"

class IsNSObject a => IsCNScriptChanges a where
  toCNScriptChanges :: a -> Id CNScriptChanges

instance IsCNScriptChanges (Id CNScriptChanges) where
  toCNScriptChanges = unsafeCastId

instance IsNSObject (Id CNScriptChanges) where
  toNSObject = unsafeCastId

-- ---------- CNScriptFrame ----------

-- | Represents focus & detection information at a particular time.
--
-- Indicates where to focus (disparity) and what to focus on (detection) at a particular time in the movie. It also provides access to all known detections that can be focused on at that time. Utility methods support looking up a detection by detectionID or detectionGroupID.
--
-- Frames are obtained from the cinematic script using @frame(at:tolerance:)@ or @frames(in:)@.
-- 
-- Phantom type for @CNScriptFrame@.
data CNScriptFrame

instance IsObjCObject (Id CNScriptFrame) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNScriptFrame"

class IsNSObject a => IsCNScriptFrame a where
  toCNScriptFrame :: a -> Id CNScriptFrame

instance IsCNScriptFrame (Id CNScriptFrame) where
  toCNScriptFrame = unsafeCastId

instance IsNSObject (Id CNScriptFrame) where
  toNSObject = unsafeCastId

-- ---------- CNCompositionInfo ----------

-- | Information about composition tracks added to an AVComposition for a cinematic asset.
-- 
-- Phantom type for @CNCompositionInfo@.
data CNCompositionInfo

instance IsObjCObject (Id CNCompositionInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNCompositionInfo"

class IsCNAssetInfo a => IsCNCompositionInfo a where
  toCNCompositionInfo :: a -> Id CNCompositionInfo

instance IsCNCompositionInfo (Id CNCompositionInfo) where
  toCNCompositionInfo = unsafeCastId

instance IsCNAssetInfo (Id CNCompositionInfo) where
  toCNAssetInfo = unsafeCastId

instance IsNSObject (Id CNCompositionInfo) where
  toNSObject = unsafeCastId

-- ---------- CNCustomDetectionTrack ----------

-- | A discrete detection track composed of individual detections.
-- 
-- Phantom type for @CNCustomDetectionTrack@.
data CNCustomDetectionTrack

instance IsObjCObject (Id CNCustomDetectionTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNCustomDetectionTrack"

class IsCNDetectionTrack a => IsCNCustomDetectionTrack a where
  toCNCustomDetectionTrack :: a -> Id CNCustomDetectionTrack

instance IsCNCustomDetectionTrack (Id CNCustomDetectionTrack) where
  toCNCustomDetectionTrack = unsafeCastId

instance IsCNDetectionTrack (Id CNCustomDetectionTrack) where
  toCNDetectionTrack = unsafeCastId

instance IsNSObject (Id CNCustomDetectionTrack) where
  toNSObject = unsafeCastId

-- ---------- CNFixedDetectionTrack ----------

-- | A continuous detection track representing focus at a fixed disparity.
-- 
-- Phantom type for @CNFixedDetectionTrack@.
data CNFixedDetectionTrack

instance IsObjCObject (Id CNFixedDetectionTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CNFixedDetectionTrack"

class IsCNDetectionTrack a => IsCNFixedDetectionTrack a where
  toCNFixedDetectionTrack :: a -> Id CNFixedDetectionTrack

instance IsCNFixedDetectionTrack (Id CNFixedDetectionTrack) where
  toCNFixedDetectionTrack = unsafeCastId

instance IsCNDetectionTrack (Id CNFixedDetectionTrack) where
  toCNDetectionTrack = unsafeCastId

instance IsNSObject (Id CNFixedDetectionTrack) where
  toNSObject = unsafeCastId
