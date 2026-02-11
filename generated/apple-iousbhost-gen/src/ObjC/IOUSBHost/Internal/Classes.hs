{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.IOUSBHost.Internal.Classes (
    module ObjC.IOUSBHost.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- IOUSBHostCIControllerStateMachine ----------

-- | IOUSBHostCIControllerStateMachine
--
-- The object representing the state of a user-mode USB host controller
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.
--
-- IOUSBHostCIControllerStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
-- 
-- Phantom type for @IOUSBHostCIControllerStateMachine@.
data IOUSBHostCIControllerStateMachine

instance IsObjCObject (Id IOUSBHostCIControllerStateMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostCIControllerStateMachine"

class IsNSObject a => IsIOUSBHostCIControllerStateMachine a where
  toIOUSBHostCIControllerStateMachine :: a -> Id IOUSBHostCIControllerStateMachine

instance IsIOUSBHostCIControllerStateMachine (Id IOUSBHostCIControllerStateMachine) where
  toIOUSBHostCIControllerStateMachine = unsafeCastId

instance IsNSObject (Id IOUSBHostCIControllerStateMachine) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostCIDeviceStateMachine ----------

-- | IOUSBHostCIDeviceStateMachine
--
-- The object representing the state of a user-mode USB host controller device
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller device, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.  Clients should create an IOUSBHostCIDeviceStateMachine in          response to an IOUSBHostCIMessageTypeDeviceCreate command, and then use the provided interfaces to identify and process commands          for the device.  The IOUSBHostCIDeviceStateMachine should be destroyed in response to an IOUSBHostCIMessageTypeDeviceDestroy command.
--
-- IOUSBHostCIDeviceStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
-- 
-- Phantom type for @IOUSBHostCIDeviceStateMachine@.
data IOUSBHostCIDeviceStateMachine

instance IsObjCObject (Id IOUSBHostCIDeviceStateMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostCIDeviceStateMachine"

class IsNSObject a => IsIOUSBHostCIDeviceStateMachine a where
  toIOUSBHostCIDeviceStateMachine :: a -> Id IOUSBHostCIDeviceStateMachine

instance IsIOUSBHostCIDeviceStateMachine (Id IOUSBHostCIDeviceStateMachine) where
  toIOUSBHostCIDeviceStateMachine = unsafeCastId

instance IsNSObject (Id IOUSBHostCIDeviceStateMachine) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostCIEndpointStateMachine ----------

-- | IOUSBHostCIEndpointStateMachine
--
-- The object representing the state of a user-mode USB host controller endpoint
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller endpoint, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.  Clients should create an IOUSBHostCIEndpointStateMachine in          response to an IOUSBHostCIMessageTypeEndpointCreate command, and then use the provided interfaces to identify and process commands,          doorbells, and transfer structures for the endpoint.  The IOUSBHostCIEndpointStateMachine should be destroyed in response to an          IOUSBHostCIMessageTypeEndpointDestroy command.
--
-- Endpoint state is controlled by IOUSBHostCIMessage structures representing commands and transfer completions, and IOUSBHostCIDoorbell messages.          Only an endpoint in the IOUSBHostCIEndpointStateActive state may inspect transfer structures, read or modify IO buffers, and generate transfer completions.
--
-- IOUSBHostCIEndpointStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
-- 
-- Phantom type for @IOUSBHostCIEndpointStateMachine@.
data IOUSBHostCIEndpointStateMachine

instance IsObjCObject (Id IOUSBHostCIEndpointStateMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostCIEndpointStateMachine"

class IsNSObject a => IsIOUSBHostCIEndpointStateMachine a where
  toIOUSBHostCIEndpointStateMachine :: a -> Id IOUSBHostCIEndpointStateMachine

instance IsIOUSBHostCIEndpointStateMachine (Id IOUSBHostCIEndpointStateMachine) where
  toIOUSBHostCIEndpointStateMachine = unsafeCastId

instance IsNSObject (Id IOUSBHostCIEndpointStateMachine) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostCIPortStateMachine ----------

-- | IOUSBHostCIPortStateMachine
--
-- The object representing the state of a user-mode USB host controller root port
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller root port, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.
--
-- IOUSBHostCIPortStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
-- 
-- Phantom type for @IOUSBHostCIPortStateMachine@.
data IOUSBHostCIPortStateMachine

instance IsObjCObject (Id IOUSBHostCIPortStateMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostCIPortStateMachine"

class IsNSObject a => IsIOUSBHostCIPortStateMachine a where
  toIOUSBHostCIPortStateMachine :: a -> Id IOUSBHostCIPortStateMachine

instance IsIOUSBHostCIPortStateMachine (Id IOUSBHostCIPortStateMachine) where
  toIOUSBHostCIPortStateMachine = unsafeCastId

instance IsNSObject (Id IOUSBHostCIPortStateMachine) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostControllerInterface ----------

-- | Phantom type for @IOUSBHostControllerInterface@.
data IOUSBHostControllerInterface

instance IsObjCObject (Id IOUSBHostControllerInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostControllerInterface"

class IsNSObject a => IsIOUSBHostControllerInterface a where
  toIOUSBHostControllerInterface :: a -> Id IOUSBHostControllerInterface

instance IsIOUSBHostControllerInterface (Id IOUSBHostControllerInterface) where
  toIOUSBHostControllerInterface = unsafeCastId

instance IsNSObject (Id IOUSBHostControllerInterface) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostIOSource ----------

-- | IOUSBHostIOSource
--
-- The abstract class IOUSBHostPipe and IOUSBHostStream derive from.
--
-- Defines common methods that are shared between IOUSBHostPipe and IOUSBHostStream.
-- 
-- Phantom type for @IOUSBHostIOSource@.
data IOUSBHostIOSource

instance IsObjCObject (Id IOUSBHostIOSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostIOSource"

class IsNSObject a => IsIOUSBHostIOSource a where
  toIOUSBHostIOSource :: a -> Id IOUSBHostIOSource

instance IsIOUSBHostIOSource (Id IOUSBHostIOSource) where
  toIOUSBHostIOSource = unsafeCastId

instance IsNSObject (Id IOUSBHostIOSource) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostObject ----------

-- | IOUSBHostObject
--
-- The Abstract class IOUSBHostDevice and IOUSBHostInterface derive from.
--
-- Defines common methods that are shared between IOUSBHostDevice and IOUSBHostInterface including instance          management.
-- 
-- Phantom type for @IOUSBHostObject@.
data IOUSBHostObject

instance IsObjCObject (Id IOUSBHostObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostObject"

class IsNSObject a => IsIOUSBHostObject a where
  toIOUSBHostObject :: a -> Id IOUSBHostObject

instance IsIOUSBHostObject (Id IOUSBHostObject) where
  toIOUSBHostObject = unsafeCastId

instance IsNSObject (Id IOUSBHostObject) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostPipe ----------

-- | IOUSBHostPipe
--
-- The IOUSBHostIOSource representing a USB endpoint
--
-- This class provides functionality to transfer data across USB.
-- 
-- Phantom type for @IOUSBHostPipe@.
data IOUSBHostPipe

instance IsObjCObject (Id IOUSBHostPipe) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostPipe"

class IsIOUSBHostIOSource a => IsIOUSBHostPipe a where
  toIOUSBHostPipe :: a -> Id IOUSBHostPipe

instance IsIOUSBHostPipe (Id IOUSBHostPipe) where
  toIOUSBHostPipe = unsafeCastId

instance IsIOUSBHostIOSource (Id IOUSBHostPipe) where
  toIOUSBHostIOSource = unsafeCastId

instance IsNSObject (Id IOUSBHostPipe) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostStream ----------

-- | Phantom type for @IOUSBHostStream@.
data IOUSBHostStream

instance IsObjCObject (Id IOUSBHostStream) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostStream"

class IsIOUSBHostIOSource a => IsIOUSBHostStream a where
  toIOUSBHostStream :: a -> Id IOUSBHostStream

instance IsIOUSBHostStream (Id IOUSBHostStream) where
  toIOUSBHostStream = unsafeCastId

instance IsIOUSBHostIOSource (Id IOUSBHostStream) where
  toIOUSBHostIOSource = unsafeCastId

instance IsNSObject (Id IOUSBHostStream) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostDevice ----------

-- | IOUSBHostDevice
--
-- The IOUSBHostObject representing a USB device
--
-- This class provides functionality to send control requests to the default control endpoint
-- 
-- Phantom type for @IOUSBHostDevice@.
data IOUSBHostDevice

instance IsObjCObject (Id IOUSBHostDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostDevice"

class IsIOUSBHostObject a => IsIOUSBHostDevice a where
  toIOUSBHostDevice :: a -> Id IOUSBHostDevice

instance IsIOUSBHostDevice (Id IOUSBHostDevice) where
  toIOUSBHostDevice = unsafeCastId

instance IsIOUSBHostObject (Id IOUSBHostDevice) where
  toIOUSBHostObject = unsafeCastId

instance IsNSObject (Id IOUSBHostDevice) where
  toNSObject = unsafeCastId

-- ---------- IOUSBHostInterface ----------

-- | IOUSBHostInterface
--
-- The IOUSBHostObject representing a USB interface
--
-- This class provides functionality to send control requests to the default control endpoint, as well as              create IOUSBHostPipe objects to transfer data.
-- 
-- Phantom type for @IOUSBHostInterface@.
data IOUSBHostInterface

instance IsObjCObject (Id IOUSBHostInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOUSBHostInterface"

class IsIOUSBHostObject a => IsIOUSBHostInterface a where
  toIOUSBHostInterface :: a -> Id IOUSBHostInterface

instance IsIOUSBHostInterface (Id IOUSBHostInterface) where
  toIOUSBHostInterface = unsafeCastId

instance IsIOUSBHostObject (Id IOUSBHostInterface) where
  toIOUSBHostObject = unsafeCastId

instance IsNSObject (Id IOUSBHostInterface) where
  toNSObject = unsafeCastId
