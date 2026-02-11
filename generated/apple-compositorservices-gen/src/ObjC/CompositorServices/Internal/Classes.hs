{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CompositorServices.Internal.Classes (
    module ObjC.CompositorServices.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- CP_OBJECT_cp_layer_renderer ----------

-- | An opaque type that provides the Metal types and timing information you need to draw your content.
-- 
-- Phantom type for @CP_OBJECT_cp_layer_renderer@.
data CP_OBJECT_cp_layer_renderer

instance IsObjCObject (Id CP_OBJECT_cp_layer_renderer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CP_OBJECT_cp_layer_renderer"

class IsNSObject a => IsCP_OBJECT_cp_layer_renderer a where
  toCP_OBJECT_cp_layer_renderer :: a -> Id CP_OBJECT_cp_layer_renderer

instance IsCP_OBJECT_cp_layer_renderer (Id CP_OBJECT_cp_layer_renderer) where
  toCP_OBJECT_cp_layer_renderer = unsafeCastId

instance IsNSObject (Id CP_OBJECT_cp_layer_renderer) where
  toNSObject = unsafeCastId

-- ---------- CP_OBJECT_cp_layer_renderer_capabilities ----------

-- | A type that stores the supported configurations for a layer.
-- 
-- Phantom type for @CP_OBJECT_cp_layer_renderer_capabilities@.
data CP_OBJECT_cp_layer_renderer_capabilities

instance IsObjCObject (Id CP_OBJECT_cp_layer_renderer_capabilities) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CP_OBJECT_cp_layer_renderer_capabilities"

class IsNSObject a => IsCP_OBJECT_cp_layer_renderer_capabilities a where
  toCP_OBJECT_cp_layer_renderer_capabilities :: a -> Id CP_OBJECT_cp_layer_renderer_capabilities

instance IsCP_OBJECT_cp_layer_renderer_capabilities (Id CP_OBJECT_cp_layer_renderer_capabilities) where
  toCP_OBJECT_cp_layer_renderer_capabilities = unsafeCastId

instance IsNSObject (Id CP_OBJECT_cp_layer_renderer_capabilities) where
  toNSObject = unsafeCastId

-- ---------- CP_OBJECT_cp_layer_renderer_configuration ----------

-- | An opaque type that stores the settings to apply to a Compositor layer renderer.
--
-- You don’t create this type directly. If your ``CompositorLayer`` uses a custom ``CompositorLayerConfiguration``, the compositor layer creates an instance of this type and passes it to the provider’s ``CompositorLayerConfiguration/makeConfiguration(capabilities:configuration:)`` function. Use that instance to modify the default configuration settings for your layer.
-- 
-- Phantom type for @CP_OBJECT_cp_layer_renderer_configuration@.
data CP_OBJECT_cp_layer_renderer_configuration

instance IsObjCObject (Id CP_OBJECT_cp_layer_renderer_configuration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CP_OBJECT_cp_layer_renderer_configuration"

class IsNSObject a => IsCP_OBJECT_cp_layer_renderer_configuration a where
  toCP_OBJECT_cp_layer_renderer_configuration :: a -> Id CP_OBJECT_cp_layer_renderer_configuration

instance IsCP_OBJECT_cp_layer_renderer_configuration (Id CP_OBJECT_cp_layer_renderer_configuration) where
  toCP_OBJECT_cp_layer_renderer_configuration = unsafeCastId

instance IsNSObject (Id CP_OBJECT_cp_layer_renderer_configuration) where
  toNSObject = unsafeCastId

-- ---------- CP_OBJECT_cp_layer_renderer_properties ----------

-- | An opaque type that describes the organization of the layer's textures and the relationships between those textures and the views you use for drawing.
--
-- You might use the layer's properties to configure other parts of your app. For example, use them to configure your app's render pipeline.
--
-- You can obtain layer properties directly from your layer. If you don't yet have the layer type, you can create an equivalent set of properties using the ``cp_layer_renderer_properties_create_using_configuration`` function.
-- 
-- Phantom type for @CP_OBJECT_cp_layer_renderer_properties@.
data CP_OBJECT_cp_layer_renderer_properties

instance IsObjCObject (Id CP_OBJECT_cp_layer_renderer_properties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CP_OBJECT_cp_layer_renderer_properties"

class IsNSObject a => IsCP_OBJECT_cp_layer_renderer_properties a where
  toCP_OBJECT_cp_layer_renderer_properties :: a -> Id CP_OBJECT_cp_layer_renderer_properties

instance IsCP_OBJECT_cp_layer_renderer_properties (Id CP_OBJECT_cp_layer_renderer_properties) where
  toCP_OBJECT_cp_layer_renderer_properties = unsafeCastId

instance IsNSObject (Id CP_OBJECT_cp_layer_renderer_properties) where
  toNSObject = unsafeCastId
