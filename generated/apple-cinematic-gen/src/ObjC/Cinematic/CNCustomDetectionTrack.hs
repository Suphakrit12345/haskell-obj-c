{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A discrete detection track composed of individual detections.
--
-- Generated bindings for @CNCustomDetectionTrack@.
module ObjC.Cinematic.CNCustomDetectionTrack
  ( CNCustomDetectionTrack
  , IsCNCustomDetectionTrack(..)
  , initWithDetections_smooth
  , allDetections
  , initWithDetections_smoothSelector
  , allDetectionsSelector


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
import ObjC.Foundation.Internal.Classes

-- | Initialize a custom detection track with an array of detections, optionally applying smoothing.
--
-- The smoothing algorithm used is the same one that is used for built-in detections during recording. It compensates for some amount of jitter in the disparity measure by smoothing out variability.
--
-- ObjC selector: @- initWithDetections:smooth:@
initWithDetections_smooth :: (IsCNCustomDetectionTrack cnCustomDetectionTrack, IsNSArray detections) => cnCustomDetectionTrack -> detections -> Bool -> IO (Id CNCustomDetectionTrack)
initWithDetections_smooth cnCustomDetectionTrack  detections applySmoothing =
  withObjCPtr detections $ \raw_detections ->
      sendMsg cnCustomDetectionTrack (mkSelector "initWithDetections:smooth:") (retPtr retVoid) [argPtr (castPtr raw_detections :: Ptr ()), argCULong (if applySmoothing then 1 else 0)] >>= ownedObject . castPtr

-- | @- allDetections@
allDetections :: IsCNCustomDetectionTrack cnCustomDetectionTrack => cnCustomDetectionTrack -> IO (Id NSArray)
allDetections cnCustomDetectionTrack  =
    sendMsg cnCustomDetectionTrack (mkSelector "allDetections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDetections:smooth:@
initWithDetections_smoothSelector :: Selector
initWithDetections_smoothSelector = mkSelector "initWithDetections:smooth:"

-- | @Selector@ for @allDetections@
allDetectionsSelector :: Selector
allDetectionsSelector = mkSelector "allDetections"

