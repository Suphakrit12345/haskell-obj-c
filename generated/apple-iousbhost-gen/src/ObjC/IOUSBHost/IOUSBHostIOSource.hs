{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostIOSource
--
-- The abstract class IOUSBHostPipe and IOUSBHostStream derive from.
--
-- Defines common methods that are shared between IOUSBHostPipe and IOUSBHostStream.
--
-- Generated bindings for @IOUSBHostIOSource@.
module ObjC.IOUSBHost.IOUSBHostIOSource
  ( IOUSBHostIOSource
  , IsIOUSBHostIOSource(..)
  , init_
  , hostInterface
  , deviceAddress
  , endpointAddress
  , initSelector
  , hostInterfaceSelector
  , deviceAddressSelector
  , endpointAddressSelector


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

import ObjC.IOUSBHost.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO (Id IOUSBHostIOSource)
init_ iousbHostIOSource  =
    sendMsg iousbHostIOSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Retrieve the source's IOUSBHostInterface
--
-- Returns: IOUSBHostInterface pointer that the IOSource was created from.
--
-- ObjC selector: @- hostInterface@
hostInterface :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO (Id IOUSBHostInterface)
hostInterface iousbHostIOSource  =
    sendMsg iousbHostIOSource (mkSelector "hostInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Retrieve the device's address
--
-- Returns: Current address of the device
--
-- ObjC selector: @- deviceAddress@
deviceAddress :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO CULong
deviceAddress iousbHostIOSource  =
    sendMsg iousbHostIOSource (mkSelector "deviceAddress") retCULong []

-- | Retrieve the IOSource's endpoint address
--
-- Returns: Current address of the endpoint
--
-- ObjC selector: @- endpointAddress@
endpointAddress :: IsIOUSBHostIOSource iousbHostIOSource => iousbHostIOSource -> IO CULong
endpointAddress iousbHostIOSource  =
    sendMsg iousbHostIOSource (mkSelector "endpointAddress") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @hostInterface@
hostInterfaceSelector :: Selector
hostInterfaceSelector = mkSelector "hostInterface"

-- | @Selector@ for @deviceAddress@
deviceAddressSelector :: Selector
deviceAddressSelector = mkSelector "deviceAddress"

-- | @Selector@ for @endpointAddress@
endpointAddressSelector :: Selector
endpointAddressSelector = mkSelector "endpointAddress"

