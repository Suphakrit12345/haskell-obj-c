{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceControllerFactory@.
module ObjC.Matter.MTRDeviceControllerFactory
  ( MTRDeviceControllerFactory
  , IsMTRDeviceControllerFactory(..)
  , init_
  , new
  , sharedInstance
  , startControllerFactory_error
  , stopControllerFactory
  , createControllerOnExistingFabric_error
  , createControllerOnNewFabric_error
  , preWarmCommissioningSession
  , running
  , knownFabrics
  , initSelector
  , newSelector
  , sharedInstanceSelector
  , startControllerFactory_errorSelector
  , stopControllerFactorySelector
  , createControllerOnExistingFabric_errorSelector
  , createControllerOnNewFabric_errorSelector
  , preWarmCommissioningSessionSelector
  , runningSelector
  , knownFabricsSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO (Id MTRDeviceControllerFactory)
init_ mtrDeviceControllerFactory  =
    sendMsg mtrDeviceControllerFactory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceControllerFactory)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerFactory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Return the single MTRDeviceControllerFactory we support existing.  It starts off in a "not started" state.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id MTRDeviceControllerFactory)
sharedInstance  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerFactory"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Start the controller factory. Repeated calls to startControllerFactory without calls to stopControllerFactory in between are NO-OPs. Use the isRunning property to check whether the controller factory needs to be started up.
--
-- @startupParams@ — data needed to start up the controller factory.
--
-- Returns: Whether startup succeded.
--
-- ObjC selector: @- startControllerFactory:error:@
startControllerFactory_error :: (IsMTRDeviceControllerFactory mtrDeviceControllerFactory, IsMTRDeviceControllerFactoryParams startupParams, IsNSError error_) => mtrDeviceControllerFactory -> startupParams -> error_ -> IO Bool
startControllerFactory_error mtrDeviceControllerFactory  startupParams error_ =
  withObjCPtr startupParams $ \raw_startupParams ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceControllerFactory (mkSelector "startControllerFactory:error:") retCULong [argPtr (castPtr raw_startupParams :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Stop the controller factory. This will shut down any outstanding controllers as part of the factory stopping.
--
-- Repeated calls to stopControllerFactory without calls to startControllerFactory in between are NO-OPs.
--
-- ObjC selector: @- stopControllerFactory@
stopControllerFactory :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO ()
stopControllerFactory mtrDeviceControllerFactory  =
    sendMsg mtrDeviceControllerFactory (mkSelector "stopControllerFactory") retVoid []

-- | Create a MTRDeviceController on an existing fabric.  Returns nil on failure.
--
-- This method will fail if there is no such fabric or if there is already a controller started for that fabric.
--
-- The fabric is identified by the root public key and fabric id in the startupParams.
--
-- This method can only be used if the factory was initialized with storage. When using per-controller storage, use [MTRDeviceController initWithParameters:error:].
--
-- ObjC selector: @- createControllerOnExistingFabric:error:@
createControllerOnExistingFabric_error :: (IsMTRDeviceControllerFactory mtrDeviceControllerFactory, IsMTRDeviceControllerStartupParams startupParams, IsNSError error_) => mtrDeviceControllerFactory -> startupParams -> error_ -> IO (Id MTRDeviceController)
createControllerOnExistingFabric_error mtrDeviceControllerFactory  startupParams error_ =
  withObjCPtr startupParams $ \raw_startupParams ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDeviceControllerFactory (mkSelector "createControllerOnExistingFabric:error:") (retPtr retVoid) [argPtr (castPtr raw_startupParams :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Create a MTRDeviceController on a new fabric.  Returns nil on failure.
--
-- This method will fail if the given fabric already exists.
--
-- The fabric is identified by the root public key and fabric id in the startupParams.
--
-- This method can only be used if the factory was initialized with storage. When using per-controller storage, use [MTRDeviceController initWithParameters:error:].
--
-- ObjC selector: @- createControllerOnNewFabric:error:@
createControllerOnNewFabric_error :: (IsMTRDeviceControllerFactory mtrDeviceControllerFactory, IsMTRDeviceControllerStartupParams startupParams, IsNSError error_) => mtrDeviceControllerFactory -> startupParams -> error_ -> IO (Id MTRDeviceController)
createControllerOnNewFabric_error mtrDeviceControllerFactory  startupParams error_ =
  withObjCPtr startupParams $ \raw_startupParams ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrDeviceControllerFactory (mkSelector "createControllerOnNewFabric:error:") (retPtr retVoid) [argPtr (castPtr raw_startupParams :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | If possible, pre-warm the Matter stack for setting up a commissioning session.
--
-- This may be called before -[MTRDeviceController setupCommissioningSessionWithPayload:] if it is known that a commissioning attempt will soon take place, but the commissioning payload is not known yet.
--
-- The controller factory must be running for pre-warming to take place.  Pre-warming can take place before any controllers are started.
--
-- ObjC selector: @- preWarmCommissioningSession@
preWarmCommissioningSession :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO ()
preWarmCommissioningSession mtrDeviceControllerFactory  =
    sendMsg mtrDeviceControllerFactory (mkSelector "preWarmCommissioningSession") retVoid []

-- | If true, the factory is in a state where it can create controllers: startControllerFactory has been called, but stopControllerFactory has not been called since then.
--
-- ObjC selector: @- running@
running :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO Bool
running mtrDeviceControllerFactory  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceControllerFactory (mkSelector "running") retCULong []

-- | Returns the list of MTRFabricInfo representing the fabrics the MTRDeviceControllerFactory knows about and the corresponding node identities of the controller factory on those fabrics.  Returns nil if the factory is not running or if there is an error reading fabric information.
--
-- All entries in this list will have a non-nil rootCertificate.
--
-- ObjC selector: @- knownFabrics@
knownFabrics :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO (Id NSArray)
knownFabrics mtrDeviceControllerFactory  =
    sendMsg mtrDeviceControllerFactory (mkSelector "knownFabrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @startControllerFactory:error:@
startControllerFactory_errorSelector :: Selector
startControllerFactory_errorSelector = mkSelector "startControllerFactory:error:"

-- | @Selector@ for @stopControllerFactory@
stopControllerFactorySelector :: Selector
stopControllerFactorySelector = mkSelector "stopControllerFactory"

-- | @Selector@ for @createControllerOnExistingFabric:error:@
createControllerOnExistingFabric_errorSelector :: Selector
createControllerOnExistingFabric_errorSelector = mkSelector "createControllerOnExistingFabric:error:"

-- | @Selector@ for @createControllerOnNewFabric:error:@
createControllerOnNewFabric_errorSelector :: Selector
createControllerOnNewFabric_errorSelector = mkSelector "createControllerOnNewFabric:error:"

-- | @Selector@ for @preWarmCommissioningSession@
preWarmCommissioningSessionSelector :: Selector
preWarmCommissioningSessionSelector = mkSelector "preWarmCommissioningSession"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @knownFabrics@
knownFabricsSelector :: Selector
knownFabricsSelector = mkSelector "knownFabrics"

