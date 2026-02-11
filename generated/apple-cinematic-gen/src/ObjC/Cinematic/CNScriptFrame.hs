{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents focus & detection information at a particular time.
--
-- Indicates where to focus (disparity) and what to focus on (detection) at a particular time in the movie. It also provides access to all known detections that can be focused on at that time. Utility methods support looking up a detection by detectionID or detectionGroupID.
--
-- Frames are obtained from the cinematic script using @frame(at:tolerance:)@ or @frames(in:)@.
--
-- Generated bindings for @CNScriptFrame@.
module ObjC.Cinematic.CNScriptFrame
  ( CNScriptFrame
  , IsCNScriptFrame(..)
  , init_
  , new
  , detectionForID
  , bestDetectionForGroupID
  , focusDisparity
  , focusDetection
  , allDetections
  , initSelector
  , newSelector
  , detectionForIDSelector
  , bestDetectionForGroupIDSelector
  , focusDisparitySelector
  , focusDetectionSelector
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

-- | @- init@
init_ :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO (Id CNScriptFrame)
init_ cnScriptFrame  =
    sendMsg cnScriptFrame (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNScriptFrame)
new  =
  do
    cls' <- getRequiredClass "CNScriptFrame"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The detection in this frame with the given detection ID, if any.
--
-- ObjC selector: @- detectionForID:@
detectionForID :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> CLong -> IO (Id CNDetection)
detectionForID cnScriptFrame  detectionID =
    sendMsg cnScriptFrame (mkSelector "detectionForID:") (retPtr retVoid) [argCLong detectionID] >>= retainedObject . castPtr

-- | The best detection to focus on in this frame among those with the given detectionGroupID. For example, a face is preferred to the corresponding torso, even though both have the same detectionGroupID.
--
-- ObjC selector: @- bestDetectionForGroupID:@
bestDetectionForGroupID :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> CLong -> IO (Id CNDetection)
bestDetectionForGroupID cnScriptFrame  detectionGroupID =
    sendMsg cnScriptFrame (mkSelector "bestDetectionForGroupID:") (retPtr retVoid) [argCLong detectionGroupID] >>= retainedObject . castPtr

-- | The disparity value representing the focus plane at which the script is focused in this frame.
--
-- A larger disparity results in the focus plane being closer to the camera. The scale and offset of disparity is not defined.
--
-- Pass this to the rendering session when rendering the corresponding frame of the movie to focus at the recommended depth.
--
-- ObjC selector: @- focusDisparity@
focusDisparity :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO CFloat
focusDisparity cnScriptFrame  =
    sendMsg cnScriptFrame (mkSelector "focusDisparity") retCFloat []

-- | The detection on which the script is focused in this frame.
--
-- The focusDisparity of the focusDetection can be different from that of the frame such as when a rack focus is in progress.
--
-- ObjC selector: @- focusDetection@
focusDetection :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO (Id CNDetection)
focusDetection cnScriptFrame  =
    sendMsg cnScriptFrame (mkSelector "focusDetection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All detected objects in this frame.
--
-- ObjC selector: @- allDetections@
allDetections :: IsCNScriptFrame cnScriptFrame => cnScriptFrame -> IO (Id NSArray)
allDetections cnScriptFrame  =
    sendMsg cnScriptFrame (mkSelector "allDetections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @detectionForID:@
detectionForIDSelector :: Selector
detectionForIDSelector = mkSelector "detectionForID:"

-- | @Selector@ for @bestDetectionForGroupID:@
bestDetectionForGroupIDSelector :: Selector
bestDetectionForGroupIDSelector = mkSelector "bestDetectionForGroupID:"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @focusDetection@
focusDetectionSelector :: Selector
focusDetectionSelector = mkSelector "focusDetection"

-- | @Selector@ for @allDetections@
allDetectionsSelector :: Selector
allDetectionsSelector = mkSelector "allDetections"

