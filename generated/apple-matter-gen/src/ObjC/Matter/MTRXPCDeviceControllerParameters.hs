{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRXPCDeviceControllerParameters@.
module ObjC.Matter.MTRXPCDeviceControllerParameters
  ( MTRXPCDeviceControllerParameters
  , IsMTRXPCDeviceControllerParameters(..)
  , init_
  , new
  , initWithXPCConnectionBlock_uniqueIdentifier
  , initWithXPConnectionBlock_uniqueIdentifier
  , uniqueIdentifier
  , xpcConnectionBlock
  , initSelector
  , newSelector
  , initWithXPCConnectionBlock_uniqueIdentifierSelector
  , initWithXPConnectionBlock_uniqueIdentifierSelector
  , uniqueIdentifierSelector
  , xpcConnectionBlockSelector


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
init_ :: IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters => mtrxpcDeviceControllerParameters -> IO (Id MTRXPCDeviceControllerParameters)
init_ mtrxpcDeviceControllerParameters  =
    sendMsg mtrxpcDeviceControllerParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRXPCDeviceControllerParameters)
new  =
  do
    cls' <- getRequiredClass "MTRXPCDeviceControllerParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A controller created from this way will connect to a remote instance of an MTRDeviceController loaded in an XPC Service
--
-- @xpcConnectionBlock@ — The XPC Connection block that will return an NSXPCConnection to the intended listener.
--
-- @uniqueIdentifier@ — The unique id to assign to the controller.
--
-- ObjC selector: @- initWithXPCConnectionBlock:uniqueIdentifier:@
initWithXPCConnectionBlock_uniqueIdentifier :: (IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters, IsNSUUID uniqueIdentifier) => mtrxpcDeviceControllerParameters -> Ptr () -> uniqueIdentifier -> IO (Id MTRXPCDeviceControllerParameters)
initWithXPCConnectionBlock_uniqueIdentifier mtrxpcDeviceControllerParameters  xpcConnectionBlock uniqueIdentifier =
  withObjCPtr uniqueIdentifier $ \raw_uniqueIdentifier ->
      sendMsg mtrxpcDeviceControllerParameters (mkSelector "initWithXPCConnectionBlock:uniqueIdentifier:") (retPtr retVoid) [argPtr (castPtr xpcConnectionBlock :: Ptr ()), argPtr (castPtr raw_uniqueIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithXPConnectionBlock:uniqueIdentifier:@
initWithXPConnectionBlock_uniqueIdentifier :: (IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters, IsNSUUID uniqueIdentifier) => mtrxpcDeviceControllerParameters -> Ptr () -> uniqueIdentifier -> IO (Id MTRXPCDeviceControllerParameters)
initWithXPConnectionBlock_uniqueIdentifier mtrxpcDeviceControllerParameters  xpcConnectionBlock uniqueIdentifier =
  withObjCPtr uniqueIdentifier $ \raw_uniqueIdentifier ->
      sendMsg mtrxpcDeviceControllerParameters (mkSelector "initWithXPConnectionBlock:uniqueIdentifier:") (retPtr retVoid) [argPtr (castPtr xpcConnectionBlock :: Ptr ()), argPtr (castPtr raw_uniqueIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- uniqueIdentifier@
uniqueIdentifier :: IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters => mtrxpcDeviceControllerParameters -> IO (Id NSUUID)
uniqueIdentifier mtrxpcDeviceControllerParameters  =
    sendMsg mtrxpcDeviceControllerParameters (mkSelector "uniqueIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- xpcConnectionBlock@
xpcConnectionBlock :: IsMTRXPCDeviceControllerParameters mtrxpcDeviceControllerParameters => mtrxpcDeviceControllerParameters -> IO (Ptr ())
xpcConnectionBlock mtrxpcDeviceControllerParameters  =
    fmap castPtr $ sendMsg mtrxpcDeviceControllerParameters (mkSelector "xpcConnectionBlock") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithXPCConnectionBlock:uniqueIdentifier:@
initWithXPCConnectionBlock_uniqueIdentifierSelector :: Selector
initWithXPCConnectionBlock_uniqueIdentifierSelector = mkSelector "initWithXPCConnectionBlock:uniqueIdentifier:"

-- | @Selector@ for @initWithXPConnectionBlock:uniqueIdentifier:@
initWithXPConnectionBlock_uniqueIdentifierSelector :: Selector
initWithXPConnectionBlock_uniqueIdentifierSelector = mkSelector "initWithXPConnectionBlock:uniqueIdentifier:"

-- | @Selector@ for @uniqueIdentifier@
uniqueIdentifierSelector :: Selector
uniqueIdentifierSelector = mkSelector "uniqueIdentifier"

-- | @Selector@ for @xpcConnectionBlock@
xpcConnectionBlockSelector :: Selector
xpcConnectionBlockSelector = mkSelector "xpcConnectionBlock"

