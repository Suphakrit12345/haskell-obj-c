{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ScreenTime.Internal.Classes (
    module ObjC.ScreenTime.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- STScreenTimeConfiguration ----------

-- | The configuration for this device.
-- 
-- Phantom type for @STScreenTimeConfiguration@.
data STScreenTimeConfiguration

instance IsObjCObject (Id STScreenTimeConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "STScreenTimeConfiguration"

class IsNSObject a => IsSTScreenTimeConfiguration a where
  toSTScreenTimeConfiguration :: a -> Id STScreenTimeConfiguration

instance IsSTScreenTimeConfiguration (Id STScreenTimeConfiguration) where
  toSTScreenTimeConfiguration = unsafeCastId

instance IsNSObject (Id STScreenTimeConfiguration) where
  toNSObject = unsafeCastId

-- ---------- STScreenTimeConfigurationObserver ----------

-- | The object you use to observe changes to the current configuration.
--
-- Use this class to start and stop observing the current configuration. For example, you can opt to disable private browsing in your web browser’s view controller when ``STScreenTimeConfiguration/enforcesChildRestrictions`` is @true@.
-- 
-- Phantom type for @STScreenTimeConfigurationObserver@.
data STScreenTimeConfigurationObserver

instance IsObjCObject (Id STScreenTimeConfigurationObserver) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "STScreenTimeConfigurationObserver"

class IsNSObject a => IsSTScreenTimeConfigurationObserver a where
  toSTScreenTimeConfigurationObserver :: a -> Id STScreenTimeConfigurationObserver

instance IsSTScreenTimeConfigurationObserver (Id STScreenTimeConfigurationObserver) where
  toSTScreenTimeConfigurationObserver = unsafeCastId

instance IsNSObject (Id STScreenTimeConfigurationObserver) where
  toNSObject = unsafeCastId

-- ---------- STWebHistory ----------

-- | The object you use to delete web-usage data.
--
-- This class provides an easy way for you to delete web history, including:
--
-- - All history - History associated to a specific URL - History during a specific time interval
-- 
-- Phantom type for @STWebHistory@.
data STWebHistory

instance IsObjCObject (Id STWebHistory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "STWebHistory"

class IsNSObject a => IsSTWebHistory a where
  toSTWebHistory :: a -> Id STWebHistory

instance IsSTWebHistory (Id STWebHistory) where
  toSTWebHistory = unsafeCastId

instance IsNSObject (Id STWebHistory) where
  toNSObject = unsafeCastId

-- ---------- STWebpageController ----------

-- | Phantom type for @STWebpageController@.
data STWebpageController

instance IsObjCObject (Id STWebpageController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "STWebpageController"

class IsNSViewController a => IsSTWebpageController a where
  toSTWebpageController :: a -> Id STWebpageController

instance IsSTWebpageController (Id STWebpageController) where
  toSTWebpageController = unsafeCastId

instance IsNSObject (Id STWebpageController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id STWebpageController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id STWebpageController) where
  toNSViewController = unsafeCastId
