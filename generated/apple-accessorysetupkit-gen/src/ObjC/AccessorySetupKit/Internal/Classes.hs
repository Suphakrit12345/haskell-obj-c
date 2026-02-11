{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AccessorySetupKit.Internal.Classes (
    module ObjC.AccessorySetupKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ASAccessory ----------

-- | Phantom type for @ASAccessory@.
data ASAccessory

instance IsObjCObject (Id ASAccessory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccessory"

class IsNSObject a => IsASAccessory a where
  toASAccessory :: a -> Id ASAccessory

instance IsASAccessory (Id ASAccessory) where
  toASAccessory = unsafeCastId

instance IsNSObject (Id ASAccessory) where
  toNSObject = unsafeCastId

-- ---------- ASAccessoryEvent ----------

-- | Event for status and other updates.
-- 
-- Phantom type for @ASAccessoryEvent@.
data ASAccessoryEvent

instance IsObjCObject (Id ASAccessoryEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccessoryEvent"

class IsNSObject a => IsASAccessoryEvent a where
  toASAccessoryEvent :: a -> Id ASAccessoryEvent

instance IsASAccessoryEvent (Id ASAccessoryEvent) where
  toASAccessoryEvent = unsafeCastId

instance IsNSObject (Id ASAccessoryEvent) where
  toNSObject = unsafeCastId

-- ---------- ASAccessorySession ----------

-- | Manages accessories.
-- 
-- Phantom type for @ASAccessorySession@.
data ASAccessorySession

instance IsObjCObject (Id ASAccessorySession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccessorySession"

class IsNSObject a => IsASAccessorySession a where
  toASAccessorySession :: a -> Id ASAccessorySession

instance IsASAccessorySession (Id ASAccessorySession) where
  toASAccessorySession = unsafeCastId

instance IsNSObject (Id ASAccessorySession) where
  toNSObject = unsafeCastId

-- ---------- ASAccessorySettings ----------

-- | Phantom type for @ASAccessorySettings@.
data ASAccessorySettings

instance IsObjCObject (Id ASAccessorySettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASAccessorySettings"

class IsNSObject a => IsASAccessorySettings a where
  toASAccessorySettings :: a -> Id ASAccessorySettings

instance IsASAccessorySettings (Id ASAccessorySettings) where
  toASAccessorySettings = unsafeCastId

instance IsNSObject (Id ASAccessorySettings) where
  toNSObject = unsafeCastId

-- ---------- ASDiscoveryDescriptor ----------

-- | Phantom type for @ASDiscoveryDescriptor@.
data ASDiscoveryDescriptor

instance IsObjCObject (Id ASDiscoveryDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASDiscoveryDescriptor"

class IsNSObject a => IsASDiscoveryDescriptor a where
  toASDiscoveryDescriptor :: a -> Id ASDiscoveryDescriptor

instance IsASDiscoveryDescriptor (Id ASDiscoveryDescriptor) where
  toASDiscoveryDescriptor = unsafeCastId

instance IsNSObject (Id ASDiscoveryDescriptor) where
  toNSObject = unsafeCastId

-- ---------- ASPickerDisplayItem ----------

-- | Phantom type for @ASPickerDisplayItem@.
data ASPickerDisplayItem

instance IsObjCObject (Id ASPickerDisplayItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPickerDisplayItem"

class IsNSObject a => IsASPickerDisplayItem a where
  toASPickerDisplayItem :: a -> Id ASPickerDisplayItem

instance IsASPickerDisplayItem (Id ASPickerDisplayItem) where
  toASPickerDisplayItem = unsafeCastId

instance IsNSObject (Id ASPickerDisplayItem) where
  toNSObject = unsafeCastId

-- ---------- ASPickerDisplaySettings ----------

-- | A type that contains settings to customize the display of the accessory picker
-- 
-- Phantom type for @ASPickerDisplaySettings@.
data ASPickerDisplaySettings

instance IsObjCObject (Id ASPickerDisplaySettings) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPickerDisplaySettings"

class IsNSObject a => IsASPickerDisplaySettings a where
  toASPickerDisplaySettings :: a -> Id ASPickerDisplaySettings

instance IsASPickerDisplaySettings (Id ASPickerDisplaySettings) where
  toASPickerDisplaySettings = unsafeCastId

instance IsNSObject (Id ASPickerDisplaySettings) where
  toNSObject = unsafeCastId

-- ---------- ASPropertyCompareString ----------

-- | A type that specifies how to filter a property against a given string and comparison options.
-- 
-- Phantom type for @ASPropertyCompareString@.
data ASPropertyCompareString

instance IsObjCObject (Id ASPropertyCompareString) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASPropertyCompareString"

class IsNSObject a => IsASPropertyCompareString a where
  toASPropertyCompareString :: a -> Id ASPropertyCompareString

instance IsASPropertyCompareString (Id ASPropertyCompareString) where
  toASPropertyCompareString = unsafeCastId

instance IsNSObject (Id ASPropertyCompareString) where
  toNSObject = unsafeCastId

-- ---------- ASDiscoveredAccessory ----------

-- | A discovered accessory, for use in creating a customized picker display item.
--
-- When your app's picker uses the ``ASPickerDisplaySettings/Options/filterDiscoveryResults`` option, you receive ``ASAccessoryEventType/accessoryDiscovered`` events that contain this type. Use the discovered accessory's Bluetooth properties to create a new ``ASDiscoveredDisplayItem``, incorporating traits like a custom accessory name or a newly downloaded product image. You can then add this item to the picker to allow the person using the app to set up the accessory.
-- 
-- Phantom type for @ASDiscoveredAccessory@.
data ASDiscoveredAccessory

instance IsObjCObject (Id ASDiscoveredAccessory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASDiscoveredAccessory"

class IsASAccessory a => IsASDiscoveredAccessory a where
  toASDiscoveredAccessory :: a -> Id ASDiscoveredAccessory

instance IsASDiscoveredAccessory (Id ASDiscoveredAccessory) where
  toASDiscoveredAccessory = unsafeCastId

instance IsASAccessory (Id ASDiscoveredAccessory) where
  toASAccessory = unsafeCastId

instance IsNSObject (Id ASDiscoveredAccessory) where
  toNSObject = unsafeCastId

-- ---------- ASDiscoveredDisplayItem ----------

-- | A picker display item created from customizing a discovered accessory.
--
-- Use this type when your app's picker uses the ``ASPickerDisplaySettings/Options/filterDiscoveryResults`` option. With this option enabled, your discovery session receives ``ASAccessoryEventType/accessoryDiscovered`` events with discovered accessories. To include a discovered accessory in the picker, create an instance of this class, optionally using the Bluetooth properties of the event's ``ASDiscoveredAccessory`` to provide a more specific name or product image. Then send the @ASDiscoveredDisplayItem@ to the picker with the session's ``ASAccessorySession/updatePicker(showing:completionHandler:)`` method.
-- 
-- Phantom type for @ASDiscoveredDisplayItem@.
data ASDiscoveredDisplayItem

instance IsObjCObject (Id ASDiscoveredDisplayItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASDiscoveredDisplayItem"

class IsASPickerDisplayItem a => IsASDiscoveredDisplayItem a where
  toASDiscoveredDisplayItem :: a -> Id ASDiscoveredDisplayItem

instance IsASDiscoveredDisplayItem (Id ASDiscoveredDisplayItem) where
  toASDiscoveredDisplayItem = unsafeCastId

instance IsASPickerDisplayItem (Id ASDiscoveredDisplayItem) where
  toASPickerDisplayItem = unsafeCastId

instance IsNSObject (Id ASDiscoveredDisplayItem) where
  toNSObject = unsafeCastId

-- ---------- ASMigrationDisplayItem ----------

-- | Phantom type for @ASMigrationDisplayItem@.
data ASMigrationDisplayItem

instance IsObjCObject (Id ASMigrationDisplayItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ASMigrationDisplayItem"

class IsASPickerDisplayItem a => IsASMigrationDisplayItem a where
  toASMigrationDisplayItem :: a -> Id ASMigrationDisplayItem

instance IsASMigrationDisplayItem (Id ASMigrationDisplayItem) where
  toASMigrationDisplayItem = unsafeCastId

instance IsASPickerDisplayItem (Id ASMigrationDisplayItem) where
  toASPickerDisplayItem = unsafeCastId

instance IsNSObject (Id ASMigrationDisplayItem) where
  toNSObject = unsafeCastId
