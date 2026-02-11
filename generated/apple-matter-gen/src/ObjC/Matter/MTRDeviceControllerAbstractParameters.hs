{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Parameters that can be used to initialize an MTRDeviceController.  Specific interfaces inheriting from this one should be used to actually do the initialization.
--
-- Generated bindings for @MTRDeviceControllerAbstractParameters@.
module ObjC.Matter.MTRDeviceControllerAbstractParameters
  ( MTRDeviceControllerAbstractParameters
  , IsMTRDeviceControllerAbstractParameters(..)
  , init_
  , new
  , startSuspended
  , setStartSuspended
  , initSelector
  , newSelector
  , startSuspendedSelector
  , setStartSuspendedSelector


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
init_ :: IsMTRDeviceControllerAbstractParameters mtrDeviceControllerAbstractParameters => mtrDeviceControllerAbstractParameters -> IO (Id MTRDeviceControllerAbstractParameters)
init_ mtrDeviceControllerAbstractParameters  =
    sendMsg mtrDeviceControllerAbstractParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRDeviceControllerAbstractParameters)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerAbstractParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Whether the controller should start out suspended.
--
-- Defaults to NO.
--
-- ObjC selector: @- startSuspended@
startSuspended :: IsMTRDeviceControllerAbstractParameters mtrDeviceControllerAbstractParameters => mtrDeviceControllerAbstractParameters -> IO Bool
startSuspended mtrDeviceControllerAbstractParameters  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrDeviceControllerAbstractParameters (mkSelector "startSuspended") retCULong []

-- | Whether the controller should start out suspended.
--
-- Defaults to NO.
--
-- ObjC selector: @- setStartSuspended:@
setStartSuspended :: IsMTRDeviceControllerAbstractParameters mtrDeviceControllerAbstractParameters => mtrDeviceControllerAbstractParameters -> Bool -> IO ()
setStartSuspended mtrDeviceControllerAbstractParameters  value =
    sendMsg mtrDeviceControllerAbstractParameters (mkSelector "setStartSuspended:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @startSuspended@
startSuspendedSelector :: Selector
startSuspendedSelector = mkSelector "startSuspended"

-- | @Selector@ for @setStartSuspended:@
setStartSuspendedSelector :: Selector
setStartSuspendedSelector = mkSelector "setStartSuspended:"

