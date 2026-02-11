{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SecurityUI.Internal.Classes (
    module ObjC.SecurityUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SFCertificatePresentation ----------

-- | Phantom type for @SFCertificatePresentation@.
data SFCertificatePresentation

instance IsObjCObject (Id SFCertificatePresentation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFCertificatePresentation"

class IsNSObject a => IsSFCertificatePresentation a where
  toSFCertificatePresentation :: a -> Id SFCertificatePresentation

instance IsSFCertificatePresentation (Id SFCertificatePresentation) where
  toSFCertificatePresentation = unsafeCastId

instance IsNSObject (Id SFCertificatePresentation) where
  toNSObject = unsafeCastId
