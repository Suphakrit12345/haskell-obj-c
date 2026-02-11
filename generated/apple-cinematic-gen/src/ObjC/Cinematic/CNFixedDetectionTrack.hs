{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A continuous detection track representing focus at a fixed disparity.
--
-- Generated bindings for @CNFixedDetectionTrack@.
module ObjC.Cinematic.CNFixedDetectionTrack
  ( CNFixedDetectionTrack
  , IsCNFixedDetectionTrack(..)
  , initWithFocusDisparity
  , initWithOriginalDetection
  , focusDisparity
  , originalDetection
  , initWithFocusDisparitySelector
  , initWithOriginalDetectionSelector
  , focusDisparitySelector
  , originalDetectionSelector


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

-- | Create a detection track with fixed focus at the given disparity.
--
-- ObjC selector: @- initWithFocusDisparity:@
initWithFocusDisparity :: IsCNFixedDetectionTrack cnFixedDetectionTrack => cnFixedDetectionTrack -> CFloat -> IO (Id CNFixedDetectionTrack)
initWithFocusDisparity cnFixedDetectionTrack  focusDisparity =
    sendMsg cnFixedDetectionTrack (mkSelector "initWithFocusDisparity:") (retPtr retVoid) [argCFloat focusDisparity] >>= ownedObject . castPtr

-- | Create a detection track with fixed focus at the disparity of an existing detection.
--
-- ObjC selector: @- initWithOriginalDetection:@
initWithOriginalDetection :: (IsCNFixedDetectionTrack cnFixedDetectionTrack, IsCNDetection originalDetection) => cnFixedDetectionTrack -> originalDetection -> IO (Id CNFixedDetectionTrack)
initWithOriginalDetection cnFixedDetectionTrack  originalDetection =
  withObjCPtr originalDetection $ \raw_originalDetection ->
      sendMsg cnFixedDetectionTrack (mkSelector "initWithOriginalDetection:") (retPtr retVoid) [argPtr (castPtr raw_originalDetection :: Ptr ())] >>= ownedObject . castPtr

-- | @- focusDisparity@
focusDisparity :: IsCNFixedDetectionTrack cnFixedDetectionTrack => cnFixedDetectionTrack -> IO CFloat
focusDisparity cnFixedDetectionTrack  =
    sendMsg cnFixedDetectionTrack (mkSelector "focusDisparity") retCFloat []

-- | The original detection upon which this fixed detection track was based, if any.
--
-- This is the way to determine the time and rect from which fixed focus originated, if any. This detection is not part of the detection track and has a different detectionID or none.
--
-- - Important: To get a detection from the fixed detection track, use detectionAtOrBeforeTime: instead, which will return a properly time-stamped detection.
--
-- ObjC selector: @- originalDetection@
originalDetection :: IsCNFixedDetectionTrack cnFixedDetectionTrack => cnFixedDetectionTrack -> IO (Id CNDetection)
originalDetection cnFixedDetectionTrack  =
    sendMsg cnFixedDetectionTrack (mkSelector "originalDetection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFocusDisparity:@
initWithFocusDisparitySelector :: Selector
initWithFocusDisparitySelector = mkSelector "initWithFocusDisparity:"

-- | @Selector@ for @initWithOriginalDetection:@
initWithOriginalDetectionSelector :: Selector
initWithOriginalDetectionSelector = mkSelector "initWithOriginalDetection:"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @originalDetection@
originalDetectionSelector :: Selector
originalDetectionSelector = mkSelector "originalDetection"

