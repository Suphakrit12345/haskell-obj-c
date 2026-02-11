{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ParavirtualizedGraphics.Internal.Classes (
    module ObjC.ParavirtualizedGraphics.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- PGDeviceDescriptor ----------

-- | Phantom type for @PGDeviceDescriptor@.
data PGDeviceDescriptor

instance IsObjCObject (Id PGDeviceDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PGDeviceDescriptor"

class IsNSObject a => IsPGDeviceDescriptor a where
  toPGDeviceDescriptor :: a -> Id PGDeviceDescriptor

instance IsPGDeviceDescriptor (Id PGDeviceDescriptor) where
  toPGDeviceDescriptor = unsafeCastId

instance IsNSObject (Id PGDeviceDescriptor) where
  toNSObject = unsafeCastId

-- ---------- PGDisplayDescriptor ----------

-- | PGDisplayDescriptor:
--
-- Descriptor to facilitate creation of PGDisplay.
--
-- See [PGDevice newDisplayWithDescriptor:port:serialNum]
-- 
-- Phantom type for @PGDisplayDescriptor@.
data PGDisplayDescriptor

instance IsObjCObject (Id PGDisplayDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PGDisplayDescriptor"

class IsNSObject a => IsPGDisplayDescriptor a where
  toPGDisplayDescriptor :: a -> Id PGDisplayDescriptor

instance IsPGDisplayDescriptor (Id PGDisplayDescriptor) where
  toPGDisplayDescriptor = unsafeCastId

instance IsNSObject (Id PGDisplayDescriptor) where
  toNSObject = unsafeCastId

-- ---------- PGDisplayMode ----------

-- | PGDisplayMode:
--
-- Description of supported display mode.
--
-- Client of PGDisplay can dynamically supply NSArray of PGDisplayMode objects to convey supported modes.  The first mode in array is preferred.
-- 
-- Phantom type for @PGDisplayMode@.
data PGDisplayMode

instance IsObjCObject (Id PGDisplayMode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PGDisplayMode"

class IsNSObject a => IsPGDisplayMode a where
  toPGDisplayMode :: a -> Id PGDisplayMode

instance IsPGDisplayMode (Id PGDisplayMode) where
  toPGDisplayMode = unsafeCastId

instance IsNSObject (Id PGDisplayMode) where
  toNSObject = unsafeCastId
