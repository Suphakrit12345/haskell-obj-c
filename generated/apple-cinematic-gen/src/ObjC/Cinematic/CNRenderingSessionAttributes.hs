{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Movie-wide information required by the rendering session.
--
-- Generated bindings for @CNRenderingSessionAttributes@.
module ObjC.Cinematic.CNRenderingSessionAttributes
  ( CNRenderingSessionAttributes
  , IsCNRenderingSessionAttributes(..)
  , loadFromAsset_completionHandler
  , init_
  , new
  , renderingVersion
  , loadFromAsset_completionHandlerSelector
  , initSelector
  , newSelector
  , renderingVersionSelector


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

-- | Load rendering session attributes from an asset asynchronously.
--
-- ObjC selector: @+ loadFromAsset:completionHandler:@
loadFromAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
loadFromAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "CNRenderingSessionAttributes"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "loadFromAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- init@
init_ :: IsCNRenderingSessionAttributes cnRenderingSessionAttributes => cnRenderingSessionAttributes -> IO (Id CNRenderingSessionAttributes)
init_ cnRenderingSessionAttributes  =
    sendMsg cnRenderingSessionAttributes (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNRenderingSessionAttributes)
new  =
  do
    cls' <- getRequiredClass "CNRenderingSessionAttributes"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Rendering version used to render the original.
--
-- ObjC selector: @- renderingVersion@
renderingVersion :: IsCNRenderingSessionAttributes cnRenderingSessionAttributes => cnRenderingSessionAttributes -> IO CLong
renderingVersion cnRenderingSessionAttributes  =
    sendMsg cnRenderingSessionAttributes (mkSelector "renderingVersion") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromAsset:completionHandler:@
loadFromAsset_completionHandlerSelector :: Selector
loadFromAsset_completionHandlerSelector = mkSelector "loadFromAsset:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @renderingVersion@
renderingVersionSelector :: Selector
renderingVersionSelector = mkSelector "renderingVersion"

