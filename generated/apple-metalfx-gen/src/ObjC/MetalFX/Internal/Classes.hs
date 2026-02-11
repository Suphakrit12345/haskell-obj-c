{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MetalFX.Internal.Classes (
    module ObjC.MetalFX.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MTLFXFrameInterpolatorDescriptor ----------

-- | A set of properties that configure a frame interpolator, and a factory method that creates the effect.
--
-- A frame interpolator inspects two frames your game or app renders and, based on their properties, generates an extra frame at a fraction of the cost, helping you to increase your frame rate.
--
-- When you configure this descriptor, set the properties that determine the pixel format for each texture to the respective format of the texture you later assign to the scaler. For example, make sure that the format to which you set the ``colorTextureFormat`` property matches the format of the texture you later assign to the interpolator's ``MTLFXFrameInterpolatorDescriptor/colorTexture`` property.
-- 
-- Phantom type for @MTLFXFrameInterpolatorDescriptor@.
data MTLFXFrameInterpolatorDescriptor

instance IsObjCObject (Id MTLFXFrameInterpolatorDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFXFrameInterpolatorDescriptor"

class IsNSObject a => IsMTLFXFrameInterpolatorDescriptor a where
  toMTLFXFrameInterpolatorDescriptor :: a -> Id MTLFXFrameInterpolatorDescriptor

instance IsMTLFXFrameInterpolatorDescriptor (Id MTLFXFrameInterpolatorDescriptor) where
  toMTLFXFrameInterpolatorDescriptor = unsafeCastId

instance IsNSObject (Id MTLFXFrameInterpolatorDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLFXSpatialScalerDescriptor ----------

-- | A set of properties that configure a spatial scaling effect, and a factory method that creates the effect.
-- 
-- Phantom type for @MTLFXSpatialScalerDescriptor@.
data MTLFXSpatialScalerDescriptor

instance IsObjCObject (Id MTLFXSpatialScalerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFXSpatialScalerDescriptor"

class IsNSObject a => IsMTLFXSpatialScalerDescriptor a where
  toMTLFXSpatialScalerDescriptor :: a -> Id MTLFXSpatialScalerDescriptor

instance IsMTLFXSpatialScalerDescriptor (Id MTLFXSpatialScalerDescriptor) where
  toMTLFXSpatialScalerDescriptor = unsafeCastId

instance IsNSObject (Id MTLFXSpatialScalerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLFXTemporalDenoisedScalerDescriptor ----------

-- | Phantom type for @MTLFXTemporalDenoisedScalerDescriptor@.
data MTLFXTemporalDenoisedScalerDescriptor

instance IsObjCObject (Id MTLFXTemporalDenoisedScalerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFXTemporalDenoisedScalerDescriptor"

class IsNSObject a => IsMTLFXTemporalDenoisedScalerDescriptor a where
  toMTLFXTemporalDenoisedScalerDescriptor :: a -> Id MTLFXTemporalDenoisedScalerDescriptor

instance IsMTLFXTemporalDenoisedScalerDescriptor (Id MTLFXTemporalDenoisedScalerDescriptor) where
  toMTLFXTemporalDenoisedScalerDescriptor = unsafeCastId

instance IsNSObject (Id MTLFXTemporalDenoisedScalerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MTLFXTemporalScalerDescriptor ----------

-- | Phantom type for @MTLFXTemporalScalerDescriptor@.
data MTLFXTemporalScalerDescriptor

instance IsObjCObject (Id MTLFXTemporalScalerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MTLFXTemporalScalerDescriptor"

class IsNSObject a => IsMTLFXTemporalScalerDescriptor a where
  toMTLFXTemporalScalerDescriptor :: a -> Id MTLFXTemporalScalerDescriptor

instance IsMTLFXTemporalScalerDescriptor (Id MTLFXTemporalScalerDescriptor) where
  toMTLFXTemporalScalerDescriptor = unsafeCastId

instance IsNSObject (Id MTLFXTemporalScalerDescriptor) where
  toNSObject = unsafeCastId
