{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object representing capabilities that can be granted to a helper extension process.
--
-- Generated bindings for @BEProcessCapability@.
module ObjC.BrowserEngineKit.BEProcessCapability
  ( BEProcessCapability
  , IsBEProcessCapability(..)
  , mediaPlaybackAndCaptureWithEnvironment
  , background
  , foreground
  , suspended
  , requestWithError
  , mediaPlaybackAndCaptureWithEnvironmentSelector
  , backgroundSelector
  , foregroundSelector
  , suspendedSelector
  , requestWithErrorSelector


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
import ObjC.Foundation.Internal.Classes

-- | The helper extension process may access AV hardware required for media capture and playback.
--
-- ObjC selector: @+ mediaPlaybackAndCaptureWithEnvironment:@
mediaPlaybackAndCaptureWithEnvironment :: IsBEMediaEnvironment environment => environment -> IO (Id BEProcessCapability)
mediaPlaybackAndCaptureWithEnvironment environment =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    withObjCPtr environment $ \raw_environment ->
      sendClassMsg cls' (mkSelector "mediaPlaybackAndCaptureWithEnvironment:") (retPtr retVoid) [argPtr (castPtr raw_environment :: Ptr ())] >>= retainedObject . castPtr

-- | The helper extension process may run in the background to finish work.
--
-- ObjC selector: @+ background@
background :: IO (Id BEProcessCapability)
background  =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMsg cls' (mkSelector "background") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The helper extension process may run at foreground priority to work on behalf of the host process while it is foreground.
--
-- ObjC selector: @+ foreground@
foreground :: IO (Id BEProcessCapability)
foreground  =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMsg cls' (mkSelector "foreground") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The helper extension process may remain resident in a suspended state (it will not be granted CPU time).
--
-- ObjC selector: @+ suspended@
suspended :: IO (Id BEProcessCapability)
suspended  =
  do
    cls' <- getRequiredClass "BEProcessCapability"
    sendClassMsg cls' (mkSelector "suspended") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Requests the capability to be granted to the current process.
--
-- Returns the granted capability or nil and an error if it can not be granted
--
-- ObjC selector: @- requestWithError:@
requestWithError :: (IsBEProcessCapability beProcessCapability, IsNSError error_) => beProcessCapability -> error_ -> IO RawId
requestWithError beProcessCapability  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg beProcessCapability (mkSelector "requestWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaPlaybackAndCaptureWithEnvironment:@
mediaPlaybackAndCaptureWithEnvironmentSelector :: Selector
mediaPlaybackAndCaptureWithEnvironmentSelector = mkSelector "mediaPlaybackAndCaptureWithEnvironment:"

-- | @Selector@ for @background@
backgroundSelector :: Selector
backgroundSelector = mkSelector "background"

-- | @Selector@ for @foreground@
foregroundSelector :: Selector
foregroundSelector = mkSelector "foreground"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @requestWithError:@
requestWithErrorSelector :: Selector
requestWithErrorSelector = mkSelector "requestWithError:"

