{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a running network extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can only be one extension process per host browser. The first time this type is initialized, a  process will be launched. If a extension process is all ready running, the returned object will represent the already running process.
--
-- Generated bindings for @BENetworkingProcess@.
module ObjC.BrowserEngineKit.BENetworkingProcess
  ( BENetworkingProcess
  , IsBENetworkingProcess(..)
  , init_
  , new
  , networkProcessWithInterruptionHandler_completion
  , networkProcessWithBundleID_interruptionHandler_completion
  , invalidate
  , makeLibXPCConnectionError
  , grantCapability_error_invalidationHandler
  , grantCapability_error
  , initSelector
  , newSelector
  , networkProcessWithInterruptionHandler_completionSelector
  , networkProcessWithBundleID_interruptionHandler_completionSelector
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
init_ :: IsBENetworkingProcess beNetworkingProcess => beNetworkingProcess -> IO (Id BENetworkingProcess)
init_ beNetworkingProcess  =
    sendMsg beNetworkingProcess (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BENetworkingProcess)
new  =
  do
    cls' <- getRequiredClass "BENetworkingProcess"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Asynchronously finds an existing network extension process or launches a one.
--
-- This initializer finds an existing networking extension process. If it’s unable to find an existing process, it launches a new extension process.
--
-- - Parameters:   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BENetworkingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ networkProcessWithInterruptionHandler:completion:@
networkProcessWithInterruptionHandler_completion :: Ptr () -> Ptr () -> IO ()
networkProcessWithInterruptionHandler_completion interruptionHandler completion =
  do
    cls' <- getRequiredClass "BENetworkingProcess"
    sendClassMsg cls' (mkSelector "networkProcessWithInterruptionHandler:completion:") retVoid [argPtr (castPtr interruptionHandler :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Asynchronously launches a network extension process.
--
-- This initializer launches a new network extension process with the provided bundle identifier.
--
-- - Parameters:   - @bundleID@ : The bundle identifier of the network extension process to launch.   - @interruptionHandler@ : A block that is called if the extension process terminates.   - @completion@ : A block called with a new ``BENetworkingProcess`` when the extension process has     launched or with an error.
--
-- ObjC selector: @+ networkProcessWithBundleID:interruptionHandler:completion:@
networkProcessWithBundleID_interruptionHandler_completion :: IsNSString bundleID => bundleID -> Ptr () -> Ptr () -> IO ()
networkProcessWithBundleID_interruptionHandler_completion bundleID interruptionHandler completion =
  do
    cls' <- getRequiredClass "BENetworkingProcess"
    withObjCPtr bundleID $ \raw_bundleID ->
      sendClassMsg cls' (mkSelector "networkProcessWithBundleID:interruptionHandler:completion:") retVoid [argPtr (castPtr raw_bundleID :: Ptr ()), argPtr (castPtr interruptionHandler :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Stops the extension process.
--
-- When you call this method, you tell the system your app no longer needs this extension process. The system will terminate the extension process.
--
-- ObjC selector: @- invalidate@
invalidate :: IsBENetworkingProcess beNetworkingProcess => beNetworkingProcess -> IO ()
invalidate beNetworkingProcess  =
    sendMsg beNetworkingProcess (mkSelector "invalidate") retVoid []

-- | Creates a new libXPC connection to the extension process.
--
-- This method creates a connection to the extension process and returns it. If it is not possible to make an XPC connection, this method will return nil and populate the @error@ out param.
--
-- - Returns: The connection object representing the created libXPC connection or nil.
--
-- ObjC selector: @- makeLibXPCConnectionError:@
makeLibXPCConnectionError :: (IsBENetworkingProcess beNetworkingProcess, IsNSError error_) => beNetworkingProcess -> error_ -> IO (Id NSObject)
makeLibXPCConnectionError beNetworkingProcess  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg beNetworkingProcess (mkSelector "makeLibXPCConnectionError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Grants the specified capability to the process with invalidation handler.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.   - invalidationHandler: The invalidation handler
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:invalidationHandler:@
grantCapability_error_invalidationHandler :: (IsBENetworkingProcess beNetworkingProcess, IsBEProcessCapability capability, IsNSError error_) => beNetworkingProcess -> capability -> error_ -> Ptr () -> IO RawId
grantCapability_error_invalidationHandler beNetworkingProcess  capability error_ invalidationHandler =
  withObjCPtr capability $ \raw_capability ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg beNetworkingProcess (mkSelector "grantCapability:error:invalidationHandler:") (retPtr retVoid) [argPtr (castPtr raw_capability :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr invalidationHandler :: Ptr ())]

-- | Grants the specified capability to the process.
--
-- This method grants the specified capability to the process or returns nil and an error if it can not be granted.
--
-- - Parameters:   - capability: The capability to be granted   - error: The error out param populated if the capability cannot be granted.
--
-- - Returns: an invalidatable grant object that represents the granted capability.
--
-- ObjC selector: @- grantCapability:error:@
grantCapability_error :: (IsBENetworkingProcess beNetworkingProcess, IsBEProcessCapability capability, IsNSError error_) => beNetworkingProcess -> capability -> error_ -> IO RawId
grantCapability_error beNetworkingProcess  capability error_ =
  withObjCPtr capability $ \raw_capability ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg beNetworkingProcess (mkSelector "grantCapability:error:") (retPtr retVoid) [argPtr (castPtr raw_capability :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @networkProcessWithInterruptionHandler:completion:@
networkProcessWithInterruptionHandler_completionSelector :: Selector
networkProcessWithInterruptionHandler_completionSelector = mkSelector "networkProcessWithInterruptionHandler:completion:"

-- | @Selector@ for @networkProcessWithBundleID:interruptionHandler:completion:@
networkProcessWithBundleID_interruptionHandler_completionSelector :: Selector
networkProcessWithBundleID_interruptionHandler_completionSelector = mkSelector "networkProcessWithBundleID:interruptionHandler:completion:"

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

