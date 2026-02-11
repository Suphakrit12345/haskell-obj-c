{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SafetyKit.Internal.Classes (
    module ObjC.SafetyKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- SACrashDetectionEvent ----------

-- | This object describes a Crash Detection event and response to it.
--
-- SACrashDetectionEvent
-- 
-- Phantom type for @SACrashDetectionEvent@.
data SACrashDetectionEvent

instance IsObjCObject (Id SACrashDetectionEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SACrashDetectionEvent"

class IsNSObject a => IsSACrashDetectionEvent a where
  toSACrashDetectionEvent :: a -> Id SACrashDetectionEvent

instance IsSACrashDetectionEvent (Id SACrashDetectionEvent) where
  toSACrashDetectionEvent = unsafeCastId

instance IsNSObject (Id SACrashDetectionEvent) where
  toNSObject = unsafeCastId

-- ---------- SACrashDetectionManager ----------

-- | SACrashDetectionManager
--
-- Use SACrashDetectionManager to receive information about Vehicular Crash Detection events. Not all phone models support Crash Detection, check for availability before creating an instance of SACrashDetectionManager. Set the delegate immediately after creating an instance of SACrashDetectionManager. Creating multiple instances of SACrashDetectionManager is not supported and should be avoided.
--
-- SACrashDetectionManager requires an entitlement from Apple. To apply for the entitlement, see Crash Detection Entitlement Request.
-- 
-- Phantom type for @SACrashDetectionManager@.
data SACrashDetectionManager

instance IsObjCObject (Id SACrashDetectionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SACrashDetectionManager"

class IsNSObject a => IsSACrashDetectionManager a where
  toSACrashDetectionManager :: a -> Id SACrashDetectionManager

instance IsSACrashDetectionManager (Id SACrashDetectionManager) where
  toSACrashDetectionManager = unsafeCastId

instance IsNSObject (Id SACrashDetectionManager) where
  toNSObject = unsafeCastId

-- ---------- SAEmergencyResponseManager ----------

-- | SAEmergencyResponseManager
--
-- Use SAEmergencyResponseManager to request actions in response to an emergency event. Set the delegate to monitor the progress of requested emergency response actions. SAEmergencyResponseManager requires user authorization for at least one of the emergency event detections e.g. SACrashDetectionEvent
--
-- SAEmergencyResponseManager requires an entitlement from Apple to at least one of the emergency event detections. To apply for the entitlement, see respective detection mechanisms
-- 
-- Phantom type for @SAEmergencyResponseManager@.
data SAEmergencyResponseManager

instance IsObjCObject (Id SAEmergencyResponseManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SAEmergencyResponseManager"

class IsNSObject a => IsSAEmergencyResponseManager a where
  toSAEmergencyResponseManager :: a -> Id SAEmergencyResponseManager

instance IsSAEmergencyResponseManager (Id SAEmergencyResponseManager) where
  toSAEmergencyResponseManager = unsafeCastId

instance IsNSObject (Id SAEmergencyResponseManager) where
  toNSObject = unsafeCastId
