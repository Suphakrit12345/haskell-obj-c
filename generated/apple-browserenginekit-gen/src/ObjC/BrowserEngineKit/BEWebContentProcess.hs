{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a running web content extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can multiple web content process per  host browser. Each time this type is initialized, a new extension process will be launched.
--
-- Generated bindings for @BEWebContentProcess@.
module ObjC.BrowserEngineKit.BEWebContentProcess
  ( BEWebContentProcess
  , IsBEWebContentProcess(..)
  , init_
  , new
  , webContentProcessWithInterruptionHandler_completion
  , webContentProcessWithBundleID_interruptionHandler_completion
  , invalidate
  , makeLibXPCConnectionError
  , grantCapability_error_invalidationHandler
  , grantCapability_error
  , initSelector
  , newSelector
  , webContentProcessWithInterruptionHandler_completionSelector
  , webContentProcessWithBundleID_interruptionHandler_completionSelector
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
init_ :: IsBEWebContentProcess beWebContentProcess => beWebContentProcess -> IO (Id BEWebContentProcess)
init_ beWebContentProcess  =
    sendMsg beWebContentProcess (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BEWebContentProcess)
new  =
  do
    cls' <- getRequiredClass "BEWebContentProcess"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Asynchronously launches a web content process
--
-- This initializer launches a new web content extension process.
--
-- - Parameters:   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BEWebContentProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ webContentProcessWithInterruptionHandler:completion:@
webContentProcessWithInterruptionHandler_completion :: Ptr () -> Ptr () -> IO ()
webContentProcessWithInterruptionHandler_completion interruptionHandler completion =
  do
    cls' <- getRequiredClass "BEWebContentProcess"
    sendClassMsg cls' (mkSelector "webContentProcessWithInterruptionHandler:completion:") retVoid [argPtr (castPtr interruptionHandler :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Asynchronously launches a web content process
--
-- This initializer launches a new web content extension process.
--
-- - Parameters:   - @bundleID@ : The bundle identifier of the WebContent process to launch.   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BEWebContentProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ webContentProcessWithBundleID:interruptionHandler:completion:@
webContentProcessWithBundleID_interruptionHandler_completion :: IsNSString bundleID => bundleID -> Ptr () -> Ptr () -> IO ()
webContentProcessWithBundleID_interruptionHandler_completion bundleID interruptionHandler completion =
  do
    cls' <- getRequiredClass "BEWebContentProcess"
    withObjCPtr bundleID $ \raw_bundleID ->
      sendClassMsg cls' (mkSelector "webContentProcessWithBundleID:interruptionHandler:completion:") retVoid [argPtr (castPtr raw_bundleID :: Ptr ()), argPtr (castPtr interruptionHandler :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Stops the extension process.
--
-- When you call this method, you tell the system your app no longer needs this extension process. If this is the last connection from the host process to the extension process, the system terminates the extension process.
--
-- ObjC selector: @- invalidate@
invalidate :: IsBEWebContentProcess beWebContentProcess => beWebContentProcess -> IO ()
invalidate beWebContentProcess  =
    sendMsg beWebContentProcess (mkSelector "invalidate") retVoid []

-- | Creates a new libXPC connection to the extension process.
--
-- This method creates a connection to the extension process and returns it. If it is not possible to make an XPC connection, this method will return nil and populate the @error@ out param.
--
-- - Returns: The connection object representing the created libXPC connection or nil.
--
-- ObjC selector: @- makeLibXPCConnectionError:@
makeLibXPCConnectionError :: (IsBEWebContentProcess beWebContentProcess, IsNSError error_) => beWebContentProcess -> error_ -> IO (Id NSObject)
makeLibXPCConnectionError beWebContentProcess  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg beWebContentProcess (mkSelector "makeLibXPCConnectionError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Grants the specified capability to the process with invalidation handler.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.   - invalidationHandler: The invalidation handler
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandler :: (IsBEWebContentProcess beWebContentProcess, IsBEProcessCapability capability, IsNSError error_) => beWebContentProcess -> capability -> error_ -> Ptr () -> IO RawId
grantCapability_error_invalidationHandler beWebContentProcess  capability error_ invalidationHandler =
  withObjCPtr capability $ \raw_capability ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg beWebContentProcess (mkSelector "grantCapability:error:invalidationHandler:") (retPtr retVoid) [argPtr (castPtr raw_capability :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr invalidationHandler :: Ptr ())]

-- | Grants the specified capability to the process.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:@
grantCapability_error :: (IsBEWebContentProcess beWebContentProcess, IsBEProcessCapability capability, IsNSError error_) => beWebContentProcess -> capability -> error_ -> IO RawId
grantCapability_error beWebContentProcess  capability error_ =
  withObjCPtr capability $ \raw_capability ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg beWebContentProcess (mkSelector "grantCapability:error:") (retPtr retVoid) [argPtr (castPtr raw_capability :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @webContentProcessWithInterruptionHandler:completion:@
webContentProcessWithInterruptionHandler_completionSelector :: Selector
webContentProcessWithInterruptionHandler_completionSelector = mkSelector "webContentProcessWithInterruptionHandler:completion:"

-- | @Selector@ for @webContentProcessWithBundleID:interruptionHandler:completion:@
webContentProcessWithBundleID_interruptionHandler_completionSelector :: Selector
webContentProcessWithBundleID_interruptionHandler_completionSelector = mkSelector "webContentProcessWithBundleID:interruptionHandler:completion:"

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

