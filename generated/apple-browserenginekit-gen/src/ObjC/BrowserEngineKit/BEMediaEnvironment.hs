{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a media playback environment
--
-- Generated bindings for @BEMediaEnvironment@.
module ObjC.BrowserEngineKit.BEMediaEnvironment
  ( BEMediaEnvironment
  , IsBEMediaEnvironment(..)
  , init_
  , new
  , initWithWebPageURL
  , activateWithError
  , suspendWithError
  , makeCaptureSessionWithError
  , initSelector
  , newSelector
  , initWithWebPageURLSelector
  , activateWithErrorSelector
  , suspendWithErrorSelector
  , makeCaptureSessionWithErrorSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBEMediaEnvironment beMediaEnvironment => beMediaEnvironment -> IO (Id BEMediaEnvironment)
init_ beMediaEnvironment  =
    sendMsg beMediaEnvironment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BEMediaEnvironment)
new  =
  do
    cls' <- getRequiredClass "BEMediaEnvironment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new media playback environment identified by the web page URL
--
-- - Parameters:   - url: The URL identifying the media playback environment
--
-- ObjC selector: @- initWithWebPageURL:@
initWithWebPageURL :: (IsBEMediaEnvironment beMediaEnvironment, IsNSURL url) => beMediaEnvironment -> url -> IO (Id BEMediaEnvironment)
initWithWebPageURL beMediaEnvironment  url =
  withObjCPtr url $ \raw_url ->
      sendMsg beMediaEnvironment (mkSelector "initWithWebPageURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Activates the media environment.
--
-- ObjC selector: @- activateWithError:@
activateWithError :: (IsBEMediaEnvironment beMediaEnvironment, IsNSError error_) => beMediaEnvironment -> error_ -> IO Bool
activateWithError beMediaEnvironment  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg beMediaEnvironment (mkSelector "activateWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Suspends the media environment.
--
-- ObjC selector: @- suspendWithError:@
suspendWithError :: (IsBEMediaEnvironment beMediaEnvironment, IsNSError error_) => beMediaEnvironment -> error_ -> IO Bool
suspendWithError beMediaEnvironment  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg beMediaEnvironment (mkSelector "suspendWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Creates a new capture session in this media environment.
--
-- The media environment must be activated before the capture session can be started.
--
-- ObjC selector: @- makeCaptureSessionWithError:@
makeCaptureSessionWithError :: (IsBEMediaEnvironment beMediaEnvironment, IsNSError error_) => beMediaEnvironment -> error_ -> IO (Id AVCaptureSession)
makeCaptureSessionWithError beMediaEnvironment  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg beMediaEnvironment (mkSelector "makeCaptureSessionWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithWebPageURL:@
initWithWebPageURLSelector :: Selector
initWithWebPageURLSelector = mkSelector "initWithWebPageURL:"

-- | @Selector@ for @activateWithError:@
activateWithErrorSelector :: Selector
activateWithErrorSelector = mkSelector "activateWithError:"

-- | @Selector@ for @suspendWithError:@
suspendWithErrorSelector :: Selector
suspendWithErrorSelector = mkSelector "suspendWithError:"

-- | @Selector@ for @makeCaptureSessionWithError:@
makeCaptureSessionWithErrorSelector :: Selector
makeCaptureSessionWithErrorSelector = mkSelector "makeCaptureSessionWithError:"

