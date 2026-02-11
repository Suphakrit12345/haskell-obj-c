{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRControllerFactory@.
module ObjC.Matter.MTRControllerFactory
  ( MTRControllerFactory
  , IsMTRControllerFactory(..)
  , sharedInstance
  , startup
  , shutdown
  , startControllerOnExistingFabric
  , startControllerOnNewFabric
  , init_
  , new
  , isRunning
  , sharedInstanceSelector
  , startupSelector
  , shutdownSelector
  , startControllerOnExistingFabricSelector
  , startControllerOnNewFabricSelector
  , initSelector
  , newSelector
  , isRunningSelector


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

-- | @+ sharedInstance@
sharedInstance :: IO (Id MTRControllerFactory)
sharedInstance  =
  do
    cls' <- getRequiredClass "MTRControllerFactory"
    sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startup:@
startup :: (IsMTRControllerFactory mtrControllerFactory, IsMTRControllerFactoryParams startupParams) => mtrControllerFactory -> startupParams -> IO Bool
startup mtrControllerFactory  startupParams =
  withObjCPtr startupParams $ \raw_startupParams ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrControllerFactory (mkSelector "startup:") retCULong [argPtr (castPtr raw_startupParams :: Ptr ())]

-- | @- shutdown@
shutdown :: IsMTRControllerFactory mtrControllerFactory => mtrControllerFactory -> IO ()
shutdown mtrControllerFactory  =
    sendMsg mtrControllerFactory (mkSelector "shutdown") retVoid []

-- | @- startControllerOnExistingFabric:@
startControllerOnExistingFabric :: (IsMTRControllerFactory mtrControllerFactory, IsMTRDeviceControllerStartupParams startupParams) => mtrControllerFactory -> startupParams -> IO (Id MTRDeviceController)
startControllerOnExistingFabric mtrControllerFactory  startupParams =
  withObjCPtr startupParams $ \raw_startupParams ->
      sendMsg mtrControllerFactory (mkSelector "startControllerOnExistingFabric:") (retPtr retVoid) [argPtr (castPtr raw_startupParams :: Ptr ())] >>= retainedObject . castPtr

-- | @- startControllerOnNewFabric:@
startControllerOnNewFabric :: (IsMTRControllerFactory mtrControllerFactory, IsMTRDeviceControllerStartupParams startupParams) => mtrControllerFactory -> startupParams -> IO (Id MTRDeviceController)
startControllerOnNewFabric mtrControllerFactory  startupParams =
  withObjCPtr startupParams $ \raw_startupParams ->
      sendMsg mtrControllerFactory (mkSelector "startControllerOnNewFabric:") (retPtr retVoid) [argPtr (castPtr raw_startupParams :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRControllerFactory mtrControllerFactory => mtrControllerFactory -> IO (Id MTRControllerFactory)
init_ mtrControllerFactory  =
    sendMsg mtrControllerFactory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRControllerFactory)
new  =
  do
    cls' <- getRequiredClass "MTRControllerFactory"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- isRunning@
isRunning :: IsMTRControllerFactory mtrControllerFactory => mtrControllerFactory -> IO Bool
isRunning mtrControllerFactory  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrControllerFactory (mkSelector "isRunning") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @startup:@
startupSelector :: Selector
startupSelector = mkSelector "startup:"

-- | @Selector@ for @shutdown@
shutdownSelector :: Selector
shutdownSelector = mkSelector "shutdown"

-- | @Selector@ for @startControllerOnExistingFabric:@
startControllerOnExistingFabricSelector :: Selector
startControllerOnExistingFabricSelector = mkSelector "startControllerOnExistingFabric:"

-- | @Selector@ for @startControllerOnNewFabric:@
startControllerOnNewFabricSelector :: Selector
startControllerOnNewFabricSelector = mkSelector "startControllerOnNewFabric:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @isRunning@
isRunningSelector :: Selector
isRunningSelector = mkSelector "isRunning"

