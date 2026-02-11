{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.BrowserEngineKit.Internal.Classes (
    module ObjC.BrowserEngineKit.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- BEAccessibilityTextMarker ----------

-- | Subclass BEAccessibilityTextMarker to encode information about text-based positioning information in browser engines within document models.
-- 
-- Phantom type for @BEAccessibilityTextMarker@.
data BEAccessibilityTextMarker

instance IsObjCObject (Id BEAccessibilityTextMarker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEAccessibilityTextMarker"

class IsNSObject a => IsBEAccessibilityTextMarker a where
  toBEAccessibilityTextMarker :: a -> Id BEAccessibilityTextMarker

instance IsBEAccessibilityTextMarker (Id BEAccessibilityTextMarker) where
  toBEAccessibilityTextMarker = unsafeCastId

instance IsNSObject (Id BEAccessibilityTextMarker) where
  toNSObject = unsafeCastId

-- ---------- BEAccessibilityTextMarkerRange ----------

-- | BEAccessibilityTextMarkerRange holds the start and end markers for a text range.
-- 
-- Phantom type for @BEAccessibilityTextMarkerRange@.
data BEAccessibilityTextMarkerRange

instance IsObjCObject (Id BEAccessibilityTextMarkerRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEAccessibilityTextMarkerRange"

class IsNSObject a => IsBEAccessibilityTextMarkerRange a where
  toBEAccessibilityTextMarkerRange :: a -> Id BEAccessibilityTextMarkerRange

instance IsBEAccessibilityTextMarkerRange (Id BEAccessibilityTextMarkerRange) where
  toBEAccessibilityTextMarkerRange = unsafeCastId

instance IsNSObject (Id BEAccessibilityTextMarkerRange) where
  toNSObject = unsafeCastId

-- ---------- BEDownloadMonitor ----------

-- | Phantom type for @BEDownloadMonitor@.
data BEDownloadMonitor

instance IsObjCObject (Id BEDownloadMonitor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEDownloadMonitor"

class IsNSObject a => IsBEDownloadMonitor a where
  toBEDownloadMonitor :: a -> Id BEDownloadMonitor

instance IsBEDownloadMonitor (Id BEDownloadMonitor) where
  toBEDownloadMonitor = unsafeCastId

instance IsNSObject (Id BEDownloadMonitor) where
  toNSObject = unsafeCastId

-- ---------- BEDownloadMonitorLocation ----------

-- | Phantom type for @BEDownloadMonitorLocation@.
data BEDownloadMonitorLocation

instance IsObjCObject (Id BEDownloadMonitorLocation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEDownloadMonitorLocation"

class IsNSObject a => IsBEDownloadMonitorLocation a where
  toBEDownloadMonitorLocation :: a -> Id BEDownloadMonitorLocation

instance IsBEDownloadMonitorLocation (Id BEDownloadMonitorLocation) where
  toBEDownloadMonitorLocation = unsafeCastId

instance IsNSObject (Id BEDownloadMonitorLocation) where
  toNSObject = unsafeCastId

-- ---------- BELayerHierarchy ----------

-- | Phantom type for @BELayerHierarchy@.
data BELayerHierarchy

instance IsObjCObject (Id BELayerHierarchy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BELayerHierarchy"

class IsNSObject a => IsBELayerHierarchy a where
  toBELayerHierarchy :: a -> Id BELayerHierarchy

instance IsBELayerHierarchy (Id BELayerHierarchy) where
  toBELayerHierarchy = unsafeCastId

instance IsNSObject (Id BELayerHierarchy) where
  toNSObject = unsafeCastId

-- ---------- BELayerHierarchyHandle ----------

-- | Phantom type for @BELayerHierarchyHandle@.
data BELayerHierarchyHandle

instance IsObjCObject (Id BELayerHierarchyHandle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BELayerHierarchyHandle"

class IsNSObject a => IsBELayerHierarchyHandle a where
  toBELayerHierarchyHandle :: a -> Id BELayerHierarchyHandle

instance IsBELayerHierarchyHandle (Id BELayerHierarchyHandle) where
  toBELayerHierarchyHandle = unsafeCastId

instance IsNSObject (Id BELayerHierarchyHandle) where
  toNSObject = unsafeCastId

-- ---------- BELayerHierarchyHostingTransactionCoordinator ----------

-- | Phantom type for @BELayerHierarchyHostingTransactionCoordinator@.
data BELayerHierarchyHostingTransactionCoordinator

instance IsObjCObject (Id BELayerHierarchyHostingTransactionCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BELayerHierarchyHostingTransactionCoordinator"

class IsNSObject a => IsBELayerHierarchyHostingTransactionCoordinator a where
  toBELayerHierarchyHostingTransactionCoordinator :: a -> Id BELayerHierarchyHostingTransactionCoordinator

instance IsBELayerHierarchyHostingTransactionCoordinator (Id BELayerHierarchyHostingTransactionCoordinator) where
  toBELayerHierarchyHostingTransactionCoordinator = unsafeCastId

instance IsNSObject (Id BELayerHierarchyHostingTransactionCoordinator) where
  toNSObject = unsafeCastId

-- ---------- BEMediaEnvironment ----------

-- | An object that represents a media playback environment
-- 
-- Phantom type for @BEMediaEnvironment@.
data BEMediaEnvironment

instance IsObjCObject (Id BEMediaEnvironment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEMediaEnvironment"

class IsNSObject a => IsBEMediaEnvironment a where
  toBEMediaEnvironment :: a -> Id BEMediaEnvironment

instance IsBEMediaEnvironment (Id BEMediaEnvironment) where
  toBEMediaEnvironment = unsafeCastId

instance IsNSObject (Id BEMediaEnvironment) where
  toNSObject = unsafeCastId

-- ---------- BENetworkingProcess ----------

-- | An object that represents a running network extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can only be one extension process per host browser. The first time this type is initialized, a  process will be launched. If a extension process is all ready running, the returned object will represent the already running process.
-- 
-- Phantom type for @BENetworkingProcess@.
data BENetworkingProcess

instance IsObjCObject (Id BENetworkingProcess) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BENetworkingProcess"

class IsNSObject a => IsBENetworkingProcess a where
  toBENetworkingProcess :: a -> Id BENetworkingProcess

instance IsBENetworkingProcess (Id BENetworkingProcess) where
  toBENetworkingProcess = unsafeCastId

instance IsNSObject (Id BENetworkingProcess) where
  toNSObject = unsafeCastId

-- ---------- BEProcessCapability ----------

-- | An object representing capabilities that can be granted to a helper extension process.
-- 
-- Phantom type for @BEProcessCapability@.
data BEProcessCapability

instance IsObjCObject (Id BEProcessCapability) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEProcessCapability"

class IsNSObject a => IsBEProcessCapability a where
  toBEProcessCapability :: a -> Id BEProcessCapability

instance IsBEProcessCapability (Id BEProcessCapability) where
  toBEProcessCapability = unsafeCastId

instance IsNSObject (Id BEProcessCapability) where
  toNSObject = unsafeCastId

-- ---------- BERenderingProcess ----------

-- | An object that represents a running GPU extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can only be one extension process per host browser. The first time this type is initialized, a  process will be launched. If a extension process is all ready running, the returned object will represent the already running process.
-- 
-- Phantom type for @BERenderingProcess@.
data BERenderingProcess

instance IsObjCObject (Id BERenderingProcess) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BERenderingProcess"

class IsNSObject a => IsBERenderingProcess a where
  toBERenderingProcess :: a -> Id BERenderingProcess

instance IsBERenderingProcess (Id BERenderingProcess) where
  toBERenderingProcess = unsafeCastId

instance IsNSObject (Id BERenderingProcess) where
  toNSObject = unsafeCastId

-- ---------- BETextAlternatives ----------

-- | Phantom type for @BETextAlternatives@.
data BETextAlternatives

instance IsObjCObject (Id BETextAlternatives) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BETextAlternatives"

class IsNSObject a => IsBETextAlternatives a where
  toBETextAlternatives :: a -> Id BETextAlternatives

instance IsBETextAlternatives (Id BETextAlternatives) where
  toBETextAlternatives = unsafeCastId

instance IsNSObject (Id BETextAlternatives) where
  toNSObject = unsafeCastId

-- ---------- BETextSuggestion ----------

-- | Phantom type for @BETextSuggestion@.
data BETextSuggestion

instance IsObjCObject (Id BETextSuggestion) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BETextSuggestion"

class IsNSObject a => IsBETextSuggestion a where
  toBETextSuggestion :: a -> Id BETextSuggestion

instance IsBETextSuggestion (Id BETextSuggestion) where
  toBETextSuggestion = unsafeCastId

instance IsNSObject (Id BETextSuggestion) where
  toNSObject = unsafeCastId

-- ---------- BEWebAppManifest ----------

-- | An object that represents a web application manifest
-- 
-- Phantom type for @BEWebAppManifest@.
data BEWebAppManifest

instance IsObjCObject (Id BEWebAppManifest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEWebAppManifest"

class IsNSObject a => IsBEWebAppManifest a where
  toBEWebAppManifest :: a -> Id BEWebAppManifest

instance IsBEWebAppManifest (Id BEWebAppManifest) where
  toBEWebAppManifest = unsafeCastId

instance IsNSObject (Id BEWebAppManifest) where
  toNSObject = unsafeCastId

-- ---------- BEWebContentFilter ----------

-- | An object that represents a web content filter
-- 
-- Phantom type for @BEWebContentFilter@.
data BEWebContentFilter

instance IsObjCObject (Id BEWebContentFilter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEWebContentFilter"

class IsNSObject a => IsBEWebContentFilter a where
  toBEWebContentFilter :: a -> Id BEWebContentFilter

instance IsBEWebContentFilter (Id BEWebContentFilter) where
  toBEWebContentFilter = unsafeCastId

instance IsNSObject (Id BEWebContentFilter) where
  toNSObject = unsafeCastId

-- ---------- BEWebContentProcess ----------

-- | An object that represents a running web content extension process.
--
-- The system guarantees that the extension process has launched by the time the initializer methods return. If the extension process exits, the system calls ``interruptionHandler``. There can multiple web content process per  host browser. Each time this type is initialized, a new extension process will be launched.
-- 
-- Phantom type for @BEWebContentProcess@.
data BEWebContentProcess

instance IsObjCObject (Id BEWebContentProcess) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BEWebContentProcess"

class IsNSObject a => IsBEWebContentProcess a where
  toBEWebContentProcess :: a -> Id BEWebContentProcess

instance IsBEWebContentProcess (Id BEWebContentProcess) where
  toBEWebContentProcess = unsafeCastId

instance IsNSObject (Id BEWebContentProcess) where
  toNSObject = unsafeCastId
