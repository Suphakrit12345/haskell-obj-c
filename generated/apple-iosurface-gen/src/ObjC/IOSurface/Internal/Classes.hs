{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.IOSurface.Internal.Classes (
    module ObjC.IOSurface.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- IOSurface ----------

-- | Phantom type for @IOSurface@.
data IOSurface

instance IsObjCObject (Id IOSurface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOSurface"

class IsNSObject a => IsIOSurface a where
  toIOSurface :: a -> Id IOSurface

instance IsIOSurface (Id IOSurface) where
  toIOSurface = unsafeCastId

instance IsNSObject (Id IOSurface) where
  toNSObject = unsafeCastId
