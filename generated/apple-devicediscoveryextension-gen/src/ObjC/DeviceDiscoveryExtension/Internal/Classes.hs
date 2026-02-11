{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.DeviceDiscoveryExtension.Internal.Classes (
    module ObjC.DeviceDiscoveryExtension.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- DDDevice ----------

-- | DeviceDiscoveryExtension device.
-- 
-- Phantom type for @DDDevice@.
data DDDevice

instance IsObjCObject (Id DDDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DDDevice"

class IsNSObject a => IsDDDevice a where
  toDDDevice :: a -> Id DDDevice

instance IsDDDevice (Id DDDevice) where
  toDDDevice = unsafeCastId

instance IsNSObject (Id DDDevice) where
  toNSObject = unsafeCastId

-- ---------- DDDeviceEvent ----------

-- | Device-related event (e.g. found, lost).
-- 
-- Phantom type for @DDDeviceEvent@.
data DDDeviceEvent

instance IsObjCObject (Id DDDeviceEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DDDeviceEvent"

class IsNSObject a => IsDDDeviceEvent a where
  toDDDeviceEvent :: a -> Id DDDeviceEvent

instance IsDDDeviceEvent (Id DDDeviceEvent) where
  toDDDeviceEvent = unsafeCastId

instance IsNSObject (Id DDDeviceEvent) where
  toNSObject = unsafeCastId

-- ---------- DDDiscoverySession ----------

-- | Manages a session between the extension and host.
-- 
-- Phantom type for @DDDiscoverySession@.
data DDDiscoverySession

instance IsObjCObject (Id DDDiscoverySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DDDiscoverySession"

class IsNSObject a => IsDDDiscoverySession a where
  toDDDiscoverySession :: a -> Id DDDiscoverySession

instance IsDDDiscoverySession (Id DDDiscoverySession) where
  toDDDiscoverySession = unsafeCastId

instance IsNSObject (Id DDDiscoverySession) where
  toNSObject = unsafeCastId
