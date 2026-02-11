{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Frame-specific information required to render a frame in a rendering session.
--
-- Generated bindings for @CNRenderingSessionFrameAttributes@.
module ObjC.Cinematic.CNRenderingSessionFrameAttributes
  ( CNRenderingSessionFrameAttributes
  , IsCNRenderingSessionFrameAttributes(..)
  , initWithSampleBuffer_sessionAttributes
  , initWithTimedMetadataGroup_sessionAttributes
  , init_
  , new
  , focusDisparity
  , setFocusDisparity
  , fNumber
  , setFNumber
  , initWithSampleBuffer_sessionAttributesSelector
  , initWithTimedMetadataGroup_sessionAttributesSelector
  , initSelector
  , newSelector
  , focusDisparitySelector
  , setFocusDisparitySelector
  , fNumberSelector
  , setFNumberSelector


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

-- | Initialize rendering frame attributes from a sample buffer read from a cinematic metadata track. - Parameters:   - sampleBuffer: A sample buffer read from the timed cinematic metadata track of a cinematic asset.   - sessionAttributes: Rendering session attributes loaded from a cinematic asset.
--
-- ObjC selector: @- initWithSampleBuffer:sessionAttributes:@
initWithSampleBuffer_sessionAttributes :: (IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes, IsCNRenderingSessionAttributes sessionAttributes) => cnRenderingSessionFrameAttributes -> Ptr () -> sessionAttributes -> IO (Id CNRenderingSessionFrameAttributes)
initWithSampleBuffer_sessionAttributes cnRenderingSessionFrameAttributes  sampleBuffer sessionAttributes =
  withObjCPtr sessionAttributes $ \raw_sessionAttributes ->
      sendMsg cnRenderingSessionFrameAttributes (mkSelector "initWithSampleBuffer:sessionAttributes:") (retPtr retVoid) [argPtr sampleBuffer, argPtr (castPtr raw_sessionAttributes :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize rendering frame attributes from a timed metadata group read from a cinematic metadata track. - Parameters:   - metadataGroup: An AVTimedMetadataGroup read from the timed cinematic metadata track of a cinematic asset.   - sessionAttributes: Rendering session attributes loaded from a cinematic asset.
--
-- ObjC selector: @- initWithTimedMetadataGroup:sessionAttributes:@
initWithTimedMetadataGroup_sessionAttributes :: (IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes, IsAVTimedMetadataGroup metadataGroup, IsCNRenderingSessionAttributes sessionAttributes) => cnRenderingSessionFrameAttributes -> metadataGroup -> sessionAttributes -> IO (Id CNRenderingSessionFrameAttributes)
initWithTimedMetadataGroup_sessionAttributes cnRenderingSessionFrameAttributes  metadataGroup sessionAttributes =
  withObjCPtr metadataGroup $ \raw_metadataGroup ->
    withObjCPtr sessionAttributes $ \raw_sessionAttributes ->
        sendMsg cnRenderingSessionFrameAttributes (mkSelector "initWithTimedMetadataGroup:sessionAttributes:") (retPtr retVoid) [argPtr (castPtr raw_metadataGroup :: Ptr ()), argPtr (castPtr raw_sessionAttributes :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> IO (Id CNRenderingSessionFrameAttributes)
init_ cnRenderingSessionFrameAttributes  =
    sendMsg cnRenderingSessionFrameAttributes (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNRenderingSessionFrameAttributes)
new  =
  do
    cls' <- getRequiredClass "CNRenderingSessionFrameAttributes"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The disparity value which represents the focus plane at which the rendered image should be in focus.
--
-- A larger disparity results in the focus plane being closer to the camera. The scale and offset of disparity is not defined. It is best practice to obtain disparity values from detections or by interpolation between known disparity values.
--
-- ObjC selector: @- focusDisparity@
focusDisparity :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> IO CFloat
focusDisparity cnRenderingSessionFrameAttributes  =
    sendMsg cnRenderingSessionFrameAttributes (mkSelector "focusDisparity") retCFloat []

-- | The disparity value which represents the focus plane at which the rendered image should be in focus.
--
-- A larger disparity results in the focus plane being closer to the camera. The scale and offset of disparity is not defined. It is best practice to obtain disparity values from detections or by interpolation between known disparity values.
--
-- ObjC selector: @- setFocusDisparity:@
setFocusDisparity :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> CFloat -> IO ()
setFocusDisparity cnRenderingSessionFrameAttributes  value =
    sendMsg cnRenderingSessionFrameAttributes (mkSelector "setFocusDisparity:") retVoid [argCFloat value]

-- | The f-stop value which inversely affects the aperture used to render the image.
--
-- A smaller f/ number results in larger bokeh and a shallower depth of field in the rendered image.
--
-- ObjC selector: @- fNumber@
fNumber :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> IO CFloat
fNumber cnRenderingSessionFrameAttributes  =
    sendMsg cnRenderingSessionFrameAttributes (mkSelector "fNumber") retCFloat []

-- | The f-stop value which inversely affects the aperture used to render the image.
--
-- A smaller f/ number results in larger bokeh and a shallower depth of field in the rendered image.
--
-- ObjC selector: @- setFNumber:@
setFNumber :: IsCNRenderingSessionFrameAttributes cnRenderingSessionFrameAttributes => cnRenderingSessionFrameAttributes -> CFloat -> IO ()
setFNumber cnRenderingSessionFrameAttributes  value =
    sendMsg cnRenderingSessionFrameAttributes (mkSelector "setFNumber:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSampleBuffer:sessionAttributes:@
initWithSampleBuffer_sessionAttributesSelector :: Selector
initWithSampleBuffer_sessionAttributesSelector = mkSelector "initWithSampleBuffer:sessionAttributes:"

-- | @Selector@ for @initWithTimedMetadataGroup:sessionAttributes:@
initWithTimedMetadataGroup_sessionAttributesSelector :: Selector
initWithTimedMetadataGroup_sessionAttributesSelector = mkSelector "initWithTimedMetadataGroup:sessionAttributes:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @setFocusDisparity:@
setFocusDisparitySelector :: Selector
setFocusDisparitySelector = mkSelector "setFocusDisparity:"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @setFNumber:@
setFNumberSelector :: Selector
setFNumberSelector = mkSelector "setFNumber:"

