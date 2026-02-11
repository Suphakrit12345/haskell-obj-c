{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRNotificationUsage@.
module ObjC.SensorKit.SRNotificationUsage
  ( SRNotificationUsage
  , IsSRNotificationUsage(..)
  , bundleIdentifier
  , event
  , bundleIdentifierSelector
  , eventSelector

  -- * Enum types
  , SRNotificationEvent(SRNotificationEvent)
  , pattern SRNotificationEventUnknown
  , pattern SRNotificationEventReceived
  , pattern SRNotificationEventDefaultAction
  , pattern SRNotificationEventSupplementaryAction
  , pattern SRNotificationEventClear
  , pattern SRNotificationEventNotificationCenterClearAll
  , pattern SRNotificationEventRemoved
  , pattern SRNotificationEventHide
  , pattern SRNotificationEventLongLook
  , pattern SRNotificationEventSilence
  , pattern SRNotificationEventAppLaunch
  , pattern SRNotificationEventExpired
  , pattern SRNotificationEventBannerPulldown
  , pattern SRNotificationEventTapCoalesce
  , pattern SRNotificationEventDeduped
  , pattern SRNotificationEventDeviceActivated
  , pattern SRNotificationEventDeviceUnlocked

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The bundle identifier of the application that corresponds to the notification. Only populated for Apple apps.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsSRNotificationUsage srNotificationUsage => srNotificationUsage -> IO (Id NSString)
bundleIdentifier srNotificationUsage  =
    sendMsg srNotificationUsage (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- event@
event :: IsSRNotificationUsage srNotificationUsage => srNotificationUsage -> IO SRNotificationEvent
event srNotificationUsage  =
    fmap (coerce :: CLong -> SRNotificationEvent) $ sendMsg srNotificationUsage (mkSelector "event") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

