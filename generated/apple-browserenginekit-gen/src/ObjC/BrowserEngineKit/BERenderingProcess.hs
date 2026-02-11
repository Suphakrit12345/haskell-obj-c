{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a running GPU extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can only be one extension process per host browser. The first time this type is initialized, a  process will be launched. If a extension process is all ready running, the returned object will represent the already running process.
--
-- Generated bindings for @BERenderingProcess@.
module ObjC.BrowserEngineKit.BERenderingProcess
  ( BERenderingProcess
  , IsBERenderingProcess(..)
  , init_
  , new
  , renderingProcessWithInterruptionHandler_completion
  , renderingProcessWithBundleID_interruptionHandler_completion
  , invalidate
  , makeLibXPCConnectionError
  , grantCapability_error_invalidationHandler
  , grantCapability_error
  , initSelector
  , newSelector
  , renderingProcessWithInterruptionHandler_completionSelector
  , renderingProcessWithBundleID_interruptionHandler_completionSelector
  , invalidateSelector
  , makeLibXPCConnectionErrorSelector
  , grantCapability_error_invalidationHandlerSelector
  , grantCapability_errorSelector


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

-- | @- init@
init_ :: IsBERenderingProcess beRenderingProcess => beRenderingProcess -> IO (Id BERenderingProcess)
init_ beRenderingProcess  =
    sendMsg beRenderingProcess (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BERenderingProcess)
new  =
  do
    cls' <- getRequiredClass "BERenderingProcess"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Asynchronously finds an existing extension process or launches one.
--
-- This initializer finds an existing extension rendering process. If it’s unable to find an existing process, it launches a new extension process.
--
-- - Parameters:   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BERenderingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ renderingProcessWithInterruptionHandler:completion:@
renderingProcessWithInterruptionHandler_completion :: Ptr () -> Ptr () -> IO ()
renderingProcessWithInterruptionHandler_completion interruptionHandler completion =
  do
    cls' <- getRequiredClass "BERenderingProcess"
    sendClassMsg cls' (mkSelector "renderingProcessWithInterruptionHandler:completion:") retVoid [argPtr (castPtr interruptionHandler :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Asynchronously launches a rendering extension process.
--
-- This initializer launches a new rendering extension process with the provided bundle identifier.
--
-- - Parameters:   - @bundleID@ : The bundle identifier of the rendering extension process to launch.   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BERenderingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ renderingProcessWithBundleID:interruptionHandler:completion:@
renderingProcessWithBundleID_interruptionHandler_completion :: IsNSString bundleID => bundleID -> Ptr () -> Ptr () -> IO ()
renderingProcessWithBundleID_interruptionHandler_completion bundleID interruptionHandler completion =
  do
    cls' <- getRequiredClass "BERenderingProcess"
    withObjCPtr bundleID $ \raw_bundleID ->
      sendClassMsg cls' (mkSelector "renderingProcessWithBundleID:interruptionHandler:completion:") retVoid [argPtr (castPtr raw_bundleID :: Ptr ()), argPtr (castPtr interruptionHandler :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Stops the extension process.
--
-- When you call this method, you tell the system your app no longer needs this extension process. If this is the last connection from the host process to the extension process, the system terminates the extension process.
--
-- ObjC selector: @- invalidate@
invalidate :: IsBERenderingProcess beRenderingProcess => beRenderingProcess -> IO ()
invalidate beRenderingProcess  =
    sendMsg beRenderingProcess (mkSelector "invalidate") retVoid []

-- | Creates a new libXPC connection to the extension process.
--
-- This method creates a connection to the extension process and returns it. If it is not possible to make an XPC connection, this method will return nil and populate the @error@ out param.
--
-- - Returns: The connection object representing the created libXPC connection or nil.
--
-- ObjC selector: @- makeLibXPCConnectionError:@
makeLibXPCConnectionError :: (IsBERenderingProcess beRenderingProcess, IsNSError error_) => beRenderingProcess -> error_ -> IO (Id NSObject)
makeLibXPCConnectionError beRenderingProcess  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg beRenderingProcess (mkSelector "makeLibXPCConnectionError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Grants the specified capability to the process with invalidation handler.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.   - invalidationHandler: The invalidation handler
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandler :: (IsBERenderingProcess beRenderingProcess, IsBEProcessCapability capability, IsNSError error_) => beRenderingProcess -> capability -> error_ -> Ptr () -> IO RawId
grantCapability_error_invalidationHandler beRenderingProcess  capability error_ invalidationHandler =
  withObjCPtr capability $ \raw_capability ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg beRenderingProcess (mkSelector "grantCapability:error:invalidationHandler:") (retPtr retVoid) [argPtr (castPtr raw_capability :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr invalidationHandler :: Ptr ())]

-- | Grants the specified capability to the process.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:@
grantCapability_error :: (IsBERenderingProcess beRenderingProcess, IsBEProcessCapability capability, IsNSError error_) => beRenderingProcess -> capability -> error_ -> IO RawId
grantCapability_error beRenderingProcess  capability error_ =
  withObjCPtr capability $ \raw_capability ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg beRenderingProcess (mkSelector "grantCapability:error:") (retPtr retVoid) [argPtr (castPtr raw_capability :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @renderingProcessWithInterruptionHandler:completion:@
renderingProcessWithInterruptionHandler_completionSelector :: Selector
renderingProcessWithInterruptionHandler_completionSelector = mkSelector "renderingProcessWithInterruptionHandler:completion:"

-- | @Selector@ for @renderingProcessWithBundleID:interruptionHandler:completion:@
renderingProcessWithBundleID_interruptionHandler_completionSelector :: Selector
renderingProcessWithBundleID_interruptionHandler_completionSelector = mkSelector "renderingProcessWithBundleID:interruptionHandler:completion:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @makeLibXPCConnectionError:@
makeLibXPCConnectionErrorSelector :: Selector
makeLibXPCConnectionErrorSelector = mkSelector "makeLibXPCConnectionError:"

-- | @Selector@ for @grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandlerSelector :: Selector
grantCapability_error_invalidationHandlerSelector = mkSelector "grantCapability:error:invalidationHandler:"

-- | @Selector@ for @grantCapability:error:@
grantCapability_errorSelector :: Selector
grantCapability_errorSelector = mkSelector "grantCapability:error:"

